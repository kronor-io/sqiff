{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What plpgsql_check finds in function bodies: the objects they use, and the errors an
-- edit causes in them.
module Sqiff.Catalog.PlpgsqlCheck (
    Skipped,
    BodyUses,
    bodyUses,
    PlFunction (..),
    PlFunctions (..),
    plpgsqlFunctions,
    analyseBodies,
    bodyErrors,
    BodyError (..),
    Outcome (..),
    bodyErrorText,
    tempTableStatements,
) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Functor.Contravariant (contramap)
import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement)
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import RIO.Text.Partial qualified as TP
import Sqiff.Catalog.Rows
import Sqiff.Connection (errorText, failWith, inRolledBackTransaction, qualifiedNames, run, serverError)
import Sqiff.Sql (doBlock, literal)
import Sqiff.Timing (timed)


plpgsqlFunctionsQuery :: ByteString
plpgsqlFunctionsQuery = $(makeRelativeToProject "sql/plpgsql/functions.sql" >>= embedFile)


plpgsqlDependQuery :: ByteString
plpgsqlDependQuery = $(makeRelativeToProject "sql/plpgsql/depend.sql" >>= embedFile)


plpgsqlTempTablesScript :: ByteString
plpgsqlTempTablesScript = $(makeRelativeToProject "sql/plpgsql/temp_tables.sql" >>= embedFile)


-- | Functions plpgsql_check could not analyse, with the reason.
type Skipped = [(Text, Text)]


-- | Pairs of a function and what its body uses, and the functions not analysed.
type BodyUses = ([(CatalogId, CatalogId)], Skipped)


-- | What PL/pgSQL bodies use, as catalog ids, for the functions in @only@ or all of them;
-- 'Nothing' when plpgsql_check is not installed.
bodyUses :: NonEmpty Connection.Connection -> [Text] -> Maybe (Set Int64) -> IO (Maybe Text) -> IO (Maybe BodyUses)
bodyUses conns@(conn :| _) excluded only madeTempTables =
    plpgsqlFunctions conn excluded >>= traverse \fs -> do
        let checkable = maybe fs.checkable (\oids -> [f | f <- fs.checkable, f.oid `Set.member` oids]) only
        (uses, failed) <- analyseBodies conns fs{tempTables = madeTempTables >>= maybe fs.tempTables pure} checkable
        pure (uses, fs.unchecked <> failed)


-- | The statements temp_tables.sql runs on a connection without temp tables; 'Nothing'
-- when PostgreSQL refuses the script, so the script itself is run instead, or when
-- plpgsql_check is not installed. Losing the connection is an error.
tempTableStatements :: Connection.Connection -> IO (Maybe Text)
tempTableStatements conn =
    timed "tempTableStatements"
        $ run conn () plpgsqlCheckSchema
        >>= \case
            Nothing -> pure Nothing
            Just extSchema -> do
                result <-
                    inRolledBackTransaction conn "the temp table script" $ Connection.use conn do
                        Session.script (TP.replace "@extschema@" extSchema (decodeUtf8Lenient plpgsqlTempTablesScript))
                        Session.statement () [singletonStatement| select coalesce(current_setting('sqiff.temp_tables', true), '') :: text |]
                case result of
                    Right statements -> pure (Just ("set client_min_messages = error;\n" <> statements))
                    Left e
                        | isJust (serverError e) -> pure Nothing
                        | otherwise -> failWith [i|the temp table script failed: #{errorText e}|]


-- | A PL/pgSQL function plpgsql_check can analyse: its oid, name, trigger table and
-- estimated cost.
data PlFunction = PlFunction
    { oid :: Int64
    , name :: Text
    , relid :: Maybe Int64
    , cost :: Int64
    }


-- | The PL/pgSQL functions to analyse, the ones that cannot be, and the functions behind
-- user-defined casts and operators.
data PlFunctions = PlFunctions
    { query :: Text
    , tempTables :: IO Text
    , checkable :: [PlFunction]
    , unchecked :: Skipped
    , implicit :: [Int64]
    }


-- | The PL/pgSQL functions the describe queries cover; 'Nothing' when plpgsql_check is
-- not installed.
plpgsqlFunctions :: Connection.Connection -> [Text] -> IO (Maybe PlFunctions)
plpgsqlFunctions conn excluded =
    timed "plpgsqlFunctions"
        $ qualifiedNames conn
        $ run conn () plpgsqlCheckSchema
        >>= traverse \extSchema -> do
            functions <- run conn excluded (Statement.unpreparable (decodeUtf8Lenient plpgsqlFunctionsQuery) excludedParam (D.rowList functionRow))
            implicit <- toList <$> run conn () implicitQuery
            let (unchecked, checkable) = partitionEithers (map triggerTable functions)
            let withSchema = TP.replace "@extschema@" extSchema . decodeUtf8Lenient
            pure PlFunctions{query = withSchema plpgsqlDependQuery, tempTables = pure (withSchema plpgsqlTempTablesScript), checkable, unchecked, implicit}
  where
    implicitQuery =
        [vectorStatement|
            select castfunc::int8
            from pg_cast
            where castfunc >= 16384
            union
            select oprcode::int8
            from pg_operator
            where oprcode >= 16384
        |]
    functionRow = (,,,,) <$> D.column (D.nonNullable D.int8) <*> textColumn <*> D.column (D.nonNullable D.bool) <*> D.column (D.nullable D.int8) <*> int8Column
    triggerTable (oid, name, isTrigger, relid, cost)
        | isTrigger && isNothing relid = Left (name, "no trigger uses it")
        | otherwise = Right (PlFunction oid name relid cost)


-- | What the bodies of @functions@ use, and the ones plpgsql_check failed on, in the
-- order of @fs@, the slowest analysed first.
analyseBodies :: NonEmpty Connection.Connection -> PlFunctions -> [PlFunction] -> IO BodyUses
analyseBodies conns fs functions = do
    let
        (slow, fast) = L.partition ((> 0) . (.cost)) functions
    queue <- newIORef (map pure (L.sortOn (Down . (.cost)) slow) <> chunks fast)
    let next = atomicModifyIORef' queue \case
            [] -> ([], Nothing)
            b : rest -> (rest, Just b)
        batch c b = let firstName = maybe "" (\f -> f.name) (listToMaybe b) in (:) <$> timed [i|analyse.batch n=#{length b} first=#{firstName}|] (inBatches c fs.query b) <*> work c
        work c = next >>= maybe (pure []) (batch c)
        worker c =
            next >>= \case
                Nothing -> pure []
                Just b -> do
                    void (timed "analyse.tempTables" $ fs.tempTables >>= Connection.use c . Session.script)
                    batch c b `finally` void (Connection.use c (Session.script "discard temp; reset client_min_messages"))
    results <- concat <$> forConcurrently (toList conns) worker
    let position = Map.fromList (zip [f.name | f <- fs.checkable] [0 :: Int ..])
        failed = L.sortOn (\(name, _) -> Map.lookup name position) (concatMap snd results)
    pure (concatMap fst results, failed)
  where
    chunks xs = case L.splitAt batchSize xs of
        ([], _) -> []
        (b, rest) -> b : chunks rest

    batchSize = 16

    inBatches _ _ [] = pure ([], [])
    inBatches c query batch =
        Connection.use c (Session.statement batch (Statement.unpreparable query dependParams (D.rowList useRow)))
            >>= \case
                Right uses -> pure ([(CatalogId pgProc f 0, CatalogId (ClassOid k) o 0) | (f, k, o) <- uses], [])
                Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError _ message _ _ _)))
                    | [f] <- batch -> pure ([], [(f.name, message)])
                    | otherwise -> do
                        let (left, right) = L.splitAt (length batch `div` 2) batch
                        (e1, s1) <- inBatches c query left
                        (e2, s2) <- inBatches c query right
                        pure (e1 <> e2, s1 <> s2)
                Left e -> failWith [i|query failed: #{errorText e}|]

    dependParams =
        contramap (map (.oid)) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int8))))
            <> contramap (map (.relid)) (E.param (E.nonNullable (E.foldableArray (E.nullable E.int8))))

    useRow = (,,) <$> int8Column <*> int8Column <*> int8Column


-- | What checking one function's body found.
data BodyError = BodyError
    { signature :: Text
    , line :: Int32
    -- ^ 0 when the error is not on a line of the body.
    , outcome :: Outcome
    , message :: Text
    }


-- | Whether the body was checked and found wrong, or could not be checked.
data Outcome = Fails | NotChecked
    deriving stock (Eq, Show)


-- | Errors compare as their function, line and text, so one function's errors keep the
-- order they had as text.
instance Eq BodyError where
    a == b = errorOrder a == errorOrder b


instance Ord BodyError where
    compare = comparing errorOrder


-- [allow:SqiffNamedTuples] a sort key: errors compare as this triple
errorOrder :: BodyError -> (Text, Int32, Text)
errorOrder e = (e.signature, e.line, bodyErrorText e)


-- | The error as the report shows it.
bodyErrorText :: BodyError -> Text
bodyErrorText e = case e.outcome of
    Fails -> e.message
    NotChecked -> "not checked: " <> e.message


-- | plpgsql_check's errors for these signatures in the transaction @conn@ is in;
-- 'Nothing' when plpgsql_check is not installed.
bodyErrors :: Connection.Connection -> Text -> Text -> IO (Maybe Text) -> [Text] -> [Text] -> IO (Maybe (Set BodyError))
bodyErrors conn aside table made signatures sqlSignatures =
    run conn () plpgsqlCheckSchema
        >>= traverse \extSchema -> do
            tempTables <- made >>= maybe (pure (TP.replace "@extschema@" extSchema (decodeUtf8Lenient plpgsqlTempTablesScript))) pure
            made' <- timed "bodyErrors.tempTables" $ guarded (Connection.use conn (Session.script tempTables))
            recompiled <- if null signatures then pure (Right ()) else timed "bodyErrors.recompile" $ guarded (Connection.use conn (Session.script recompile))
            plErrors <- case (made', recompiled) of
                (Left e, _) -> pure [[notChecked s ("plpgsql_check's temp tables could not be made: " <> errorText e) | s <- signatures]]
                (_, Left e) -> pure [[notChecked s ("the functions could not be recompiled: " <> errorText e) | s <- signatures]]
                _ -> for (chunks signatures) (timed "bodyErrors.batch" . inBatches extSchema)
            sqlErrors <- for sqlSignatures \signature ->
                either (\e -> [BodyError{signature, line = 0, outcome = Fails, message = errorText e}]) (const [])
                    <$> timed ("bodyErrors.sql " <> signature) (guarded (Connection.use conn (Session.statement signature validate)))
            pure (Set.fromList (concat (plErrors <> sqlErrors)))
  where
    validate = [singletonStatement| select pg_catalog.fmgr_sql_validator(to_regprocedure($1 :: text)) :: text? |]
    recompile =
        let list = T.intercalate ", " (map literal signatures)
         in doBlock
                [i|
declare f record;
begin
    for f in select p.oid::regprocedure as signature, p.prosecdef from pg_proc p where p.oid in (select to_regprocedure(s) from unnest(array[#{list}]::text[]) s) loop
        execute format('alter routine %s %s', f.signature, case when f.prosecdef then 'security definer' else 'security invoker' end);
    end loop;
end |]
    guarded action = do
        savepoint "savepoint sqiff_check"
        result <- action
        savepoint (either (const "rollback to savepoint sqiff_check") (const "release savepoint sqiff_check") result)
        pure result
    savepoint sql = Connection.use conn (Session.script sql) >>= either (\e -> failWith [i|the body check lost its transaction: #{errorText e}|]) pure
    inBatches _ [] = pure []
    inBatches extSchema batch =
        guarded (Connection.use conn (Session.statement (batch, table) (Statement.unpreparable (query extSchema) params decoder))) >>= \case
            Right errors -> pure errors
            Left e@(Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError _))
                | [s] <- batch -> pure [notChecked s (errorText e)]
            Left e
                | [_] <- batch -> failWith [i|the body check failed: #{errorText e}|]
                | otherwise -> do
                    let (left, right) = L.splitAt (length batch `div` 2) batch
                    (<>) <$> inBatches extSchema left <*> inBatches extSchema right
    chunks xs = case L.splitAt 16 xs of
        ([], _) -> []
        (b, rest) -> b : chunks rest
    params = contramap fst (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text)))) <> contramap snd (E.param (E.nonNullable E.text))
    notChecked signature message = BodyError{signature, line = 0, outcome = NotChecked, message}
    decoder = D.rowList ((\signature line message -> BodyError{signature, line, outcome = Fails, message}) <$> textColumn <*> D.column (D.nonNullable D.int4) <*> textColumn)
    query extSchema =
        [i|
            select s.signature, e.lineno, e.message
            from unnest($1::text[]) with ordinality s (signature, n)
            cross join lateral (
                select coalesce(c.lineno, 0)::int4 as lineno, c.message
                from pg_proc f
                left join lateral (select t.tgrelid, t.tgoldtable, t.tgnewtable from pg_trigger t where t.tgfoid = f.oid order by t.oid limit 1) t on true
                cross join lateral #{extSchema}.plpgsql_check_function_tb(
                    f.oid::regprocedure,
                    coalesce(
                        case
                            when (select n.nspname from pg_class k join pg_namespace n on n.oid = k.relnamespace where k.oid = t.tgrelid) = #{literal aside} then to_regclass($2)
                            else t.tgrelid::regclass
                        end,
                        0::regclass
                    ),
                    fatal_errors => false,
                    oldtable => t.tgoldtable,
                    newtable => t.tgnewtable
                ) c
                where f.oid = to_regprocedure(s.signature)
                    and not (f.prorettype = 'trigger'::regtype and t.tgrelid is null)
                    and c.level = 'error'
            ) e
            order by s.n
        |]


-- | The schema plpgsql_check is installed in, if it is.
plpgsqlCheckSchema :: Statement.Statement () (Maybe Text)
plpgsqlCheckSchema =
    [maybeStatement|
        select extnamespace::regnamespace::text
        from pg_extension
        where extname = 'plpgsql_check'
    |]
