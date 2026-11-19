{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Reads a database's schema from its catalog: one definition per object, keyed by
-- @pg_identify_object_as_address@ so the same object matches across databases.
module Sqiff.Catalog (
    Aspect (..),
    Address (..),
    ObjectKey (..),
    Schema,
    Edge (..),
    UserError (..),
    failWith,
    connect,
    describe,
    Partitions (..),
    Addresses,
    describeOn,
    dependencies,
    dependenciesOn,
    dependenciesFrom,
    qualifiedNames,
    Skipped,
    bodyUses,
    BodyUses,
    PlFunction (..),
    PlFunctions (..),
    plpgsqlFunctions,
    analyseBodies,
    edgesBetween,
    relationColumns,
    bodyErrors,
    errorText,
    errorPosition,
    notChecked,
    serverError,
    tempTableStatements,
    render,
) where

import Data.FileEmbed (embedDir, embedFile, makeRelativeToProject)
import Data.Functor.Contravariant (contramap)
import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement)
import Pqi.Ffi qualified
import RIO
import RIO.FilePath (takeBaseName)
import RIO.List (unzip3, zip3)
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import RIO.Text.Partial qualified as TP
import RIO.Vector qualified as V
import Sqiff.Timing (timed)


data Aspect = Definition | Privileges | Comment
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


data Address = Address
    { objectType :: Text
    , objectNames :: [Text]
    , objectArgs :: [Text]
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


data ObjectKey = ObjectKey
    { aspect :: Aspect
    , address :: Address
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


type Schema = Map ObjectKey Text


-- | @dependent@ needs @referenced@ to exist, as recorded in @pg_depend@ or seen in a
-- PL/pgSQL body by plpgsql_check.
data Edge = Edge
    { dependent :: Address
    , referenced :: Address
    }
    deriving stock (Eq, Ord, Show)


describeQueries :: [(FilePath, ByteString)]
describeQueries = $(makeRelativeToProject "sql/describe" >>= embedDir)


dependQuery :: ByteString
dependQuery = $(makeRelativeToProject "sql/depend.sql" >>= embedFile)


plpgsqlFunctionsQuery :: ByteString
plpgsqlFunctionsQuery = $(makeRelativeToProject "sql/plpgsql/functions.sql" >>= embedFile)


plpgsqlDependQuery :: ByteString
plpgsqlDependQuery = $(makeRelativeToProject "sql/plpgsql/depend.sql" >>= embedFile)


plpgsqlTempTablesScript :: ByteString
plpgsqlTempTablesScript = $(makeRelativeToProject "sql/plpgsql/temp_tables.sql" >>= embedFile)


-- | A failure the command line reports as one line.
newtype UserError = UserError Text
    deriving stock (Show)


instance Exception UserError


failWith :: MonadIO m => Text -> m a
failWith = throwIO . UserError . T.unwords . T.words


connect :: Text -> IO Connection.Connection
connect conninfo =
    Connection.acquire Pqi.Ffi.adapter (Settings.connectionString conninfo)
        >>= either (\e -> failWith ("could not connect: " <> firstLine (reason e))) pure
  where
    reason = \case
        Errors.NetworkingConnectionError t -> t
        Errors.AuthenticationConnectionError t -> t
        Errors.CompatibilityConnectionError t -> t
        Errors.OtherConnectionError t -> t
    firstLine = T.takeWhile (/= '\n')


-- | An object's class, object and sub-object id, as in @pg_depend@.
type CatalogId = (Int64, Int64, Int32)


-- | The address of each described object, by its catalog ids.
type Addresses = Map CatalogId Address


-- | Whether partitions and their objects are described.
data Partitions = SkipPartitions | IncludePartitions
    deriving stock (Eq, Show)


-- | Every object outside system schemas, extension members and the excluded schemas.
describe :: Connection.Connection -> [Text] -> Partitions -> IO Schema
describe conn excluded partitions = fst <$> describeOn [conn] excluded partitions Nothing


-- | 'describe' with the queries spread over the connections, limited to one table's
-- objects when @onlyTable@ names it, and with each object's catalog ids.
describeOn :: [Connection.Connection] -> [Text] -> Partitions -> Maybe Text -> IO (Schema, Addresses)
describeOn conns excluded partitions onlyTable = do
    queue <- newIORef (maybe ordered (const (map (,Nothing) describeQueries)) onlyTable)
    let next = atomicModifyIORef' queue \case
            [] -> ([], Nothing)
            q : rest -> (rest, Just q)
        worker conn = next >>= maybe (pure []) (\q -> (:) <$> describeOne conn q <*> worker conn)
    parts <- concat <$> forConcurrently conns (\conn -> qualifiedNames conn (worker conn))
    (,) <$> evaluate (Map.unions (map fst parts)) <*> evaluate (Map.unions (map snd parts))
  where
    ordered =
        concat [[(q, Just 0), (q, Just 1)] | q <- byName slowest]
            <> [(q, Nothing) | q <- byName middling <> [q | q@(f, _) <- describeQueries, takeBaseName f `notElem` (slowest <> middling)]]
    slowest = ["column", "privileges", "function"]
    middling = ["constraint", "index", "relation"]
    byName names = [q | n <- names, q@(f, _) <- describeQueries, takeBaseName f == n]

    describeOne conn ((file, body), parity) =
        timed [i|describeQuery #{name} #{maybe "" show parity}|] do
            rows <- run conn (excluded, partitions == IncludePartitions, onlyTable, parity) (describeStatement body)
            evaluate
                $ force
                    ( Map.fromList [(ObjectKey (aspectOf name) address, definition) | (_, address, definition) <- rows]
                    , Map.fromList [(ids, address) | (ids, address, _) <- rows]
                    )
      where
        name = takeBaseName file

    describeStatement body =
        Statement.unpreparable (wrap (decodeUtf8Lenient body)) describeParams
            $ D.rowList ((,,) <$> catalogIdRow <*> addressRow <*> textColumn)

    aspectOf = \case
        "privileges" -> Privileges
        "comment" -> Comment
        _ -> Definition

    wrap body =
        [i|
            with o as (
            #{body}
            )
            select o.classid::int8, o.objid::int8, o.objsubid::int4, a.type, a.object_names, a.object_args, coalesce(o.definition, '')
            from o
            cross join lateral pg_identify_object_as_address(o.classid, o.objid, o.objsubid) a
            where ($3::text is null or o.relid = to_regclass($3::text))
                and ($4::int4 is null or o.objid::int8 % 2 = $4::int4)
                and not exists (
                    select
                    from pg_depend e
                    where e.deptype = 'e'
                        and (
                            (e.classid = o.classid and e.objid = o.objid)
                            or (e.classid = 'pg_class'::regclass and e.objid = o.relid)
                        )
                )
                and (
                    cardinality($1) = 0
                    or not coalesce(
                        coalesce(
                            (select ident.schema from pg_identify_object(o.classid, o.objid, o.objsubid) ident),
                            (select n.nspname::text from pg_class c join pg_namespace n on n.oid = c.relnamespace where c.oid = o.relid),
                            case when a.type = 'schema' then a.object_names[1] end
                        ) = any($1),
                        false
                    )
                )
                and ($2 or not exists (select from pg_class p where p.relispartition and p.oid = o.relid))
        |]


-- | Edges from @pg_depend@ between described objects.
dependencies :: Connection.Connection -> Addresses -> IO [Edge]
dependencies conn addresses = do
    (c1, o1, s1, c2, o2, s2) <-
        run conn (unzip3 (Map.keys addresses))
            $ Statement.unpreparable
                (decodeUtf8Lenient dependQuery)
                catalogIdsParams
                (D.singleRow ((,,,,,) <$> int8Array <*> int8Array <*> int4Array <*> int8Array <*> int8Array <*> int4Array))
    pure (edgesBetween addresses (zip (zip3 c1 o1 s1) (zip3 c2 o2 s2)))


-- | 'dependencies' limited to the edges whose referenced end is one of @targets@.
dependenciesOn :: Connection.Connection -> Addresses -> [CatalogId] -> IO [Edge]
dependenciesOn _ _ [] = pure []
dependenciesOn conn addresses targets = timed [i|dependenciesOn n=#{length targets}|] do
    (c1, o1, s1, c2, o2, s2) <-
        run conn (unzip3 (Map.keys addresses), unzip3 targets)
            $ Statement.unpreparable
                (decodeUtf8Lenient dependOnQuery)
                (contramap fst catalogIdsParams <> contramap snd catalogIdsParams)
                (D.singleRow ((,,,,,) <$> int8Array <*> int8Array <*> int4Array <*> int8Array <*> int8Array <*> int4Array))
    pure (edgesBetween addresses (zip (zip3 c1 o1 s1) (zip3 c2 o2 s2)))


dependOnQuery :: ByteString
dependOnQuery = $(makeRelativeToProject "sql/depend_on.sql" >>= embedFile)


-- | 'dependencies' limited to the edges reachable from @starts@ through dependents.
dependenciesFrom :: Connection.Connection -> Addresses -> [CatalogId] -> IO [Edge]
dependenciesFrom _ _ [] = pure []
dependenciesFrom conn addresses starts = timed [i|dependenciesFrom n=#{length starts}|] do
    (c1, o1, s1, c2, o2, s2) <-
        run conn (unzip3 starts)
            $ Statement.unpreparable
                (decodeUtf8Lenient dependFromQuery)
                catalogIdsParams
                (D.singleRow ((,,,,,) <$> int8Array <*> int8Array <*> int4Array <*> int8Array <*> int8Array <*> int4Array))
    pure (edgesBetween addresses (zip (zip3 c1 o1 s1) (zip3 c2 o2 s2)))


dependFromQuery :: ByteString
dependFromQuery = $(makeRelativeToProject "sql/depend_from.sql" >>= embedFile)


-- | The edges whose ends are both described.
edgesBetween :: Addresses -> [(CatalogId, CatalogId)] -> [Edge]
edgesBetween addresses pairs =
    [Edge a b | (x, y) <- pairs, Just a <- [Map.lookup x addresses], Just b <- [Map.lookup y addresses]]


-- | Runs @action@ with an empty @search_path@, which schema-qualifies the names the catalog
-- functions print.
qualifiedNames :: Connection.Connection -> IO a -> IO a
qualifiedNames conn action = do
    previous <- run conn () [singletonStatement| select current_setting('search_path') :: text |]
    void (run conn "" setPath)
    action `finally` void (Connection.use conn (Session.statement previous setPath))
  where
    setPath = [singletonStatement| select set_config('search_path', $1 :: text, false) :: text |]


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
-- when it fails or plpgsql_check is not installed.
tempTableStatements :: Connection.Connection -> IO (Maybe Text)
tempTableStatements conn =
    timed "tempTableStatements"
        $ run conn () plpgsqlCheckSchema
        >>= \case
            Nothing -> pure Nothing
            Just extSchema -> do
                result <-
                    Connection.use conn do
                        Session.script "begin"
                        Session.script (TP.replace "@extschema@" extSchema (decodeUtf8Lenient plpgsqlTempTablesScript))
                        Session.statement () [singletonStatement| select coalesce(current_setting('sqiff.temp_tables', true), '') :: text |]
                _ <- Connection.use conn (Session.script "rollback")
                pure (either (const Nothing) (Just . ("set client_min_messages = error;\n" <>)) result)


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
            pure (PlFunctions (withSchema plpgsqlDependQuery) (pure (withSchema plpgsqlTempTablesScript)) checkable unchecked implicit)
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
                Right uses -> pure ([((pgProc, f, 0), (k, o, 0)) | (f, k, o) <- uses], [])
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

    pgProc = 1255


render :: Address -> Text
render addr = addr.objectType <> " " <> T.intercalate "." addr.objectNames <> args
  where
    args
        | null addr.objectArgs = ""
        | otherwise = let list = T.intercalate ", " addr.objectArgs in [i|(#{list})|]


-- | plpgsql_check's errors, as (function, line, message), for these signatures in the
-- transaction @conn@ is in; 'Nothing' when plpgsql_check is not installed.
bodyErrors :: Connection.Connection -> Text -> Text -> IO (Maybe Text) -> [Text] -> [Text] -> IO (Maybe (Set (Text, Int32, Text)))
bodyErrors conn aside table made signatures sqlSignatures =
    run conn () plpgsqlCheckSchema
        >>= traverse \extSchema -> do
            tempTables <- made >>= maybe (pure (TP.replace "@extschema@" extSchema (decodeUtf8Lenient plpgsqlTempTablesScript))) pure
            made' <- timed "bodyErrors.tempTables" $ guarded (Connection.use conn (Session.script tempTables))
            recompiled <- if null signatures then pure (Right ()) else timed "bodyErrors.recompile" $ guarded (Connection.use conn (Session.script recompile))
            plErrors <- case (made', recompiled) of
                (Left e, _) -> pure [[(s, 0, notChecked <> "plpgsql_check's temp tables could not be made: " <> errorText e) | s <- signatures]]
                (_, Left e) -> pure [[(s, 0, notChecked <> "the functions could not be recompiled: " <> errorText e) | s <- signatures]]
                _ -> for (chunks signatures) (timed "bodyErrors.batch" . inBatches extSchema)
            sqlErrors <- for sqlSignatures \signature ->
                either (\e -> [(signature, 0, errorText e)]) (const [])
                    <$> timed ("bodyErrors.sql " <> signature) (guarded (Connection.use conn (Session.statement signature validate)))
            pure (Set.fromList (concat (plErrors <> sqlErrors)))
  where
    validate = [singletonStatement| select pg_catalog.fmgr_sql_validator(to_regprocedure($1 :: text)) :: text? |]
    recompile =
        let list = T.intercalate ", " (map literal signatures)
         in [i|do $sqiff$
declare f record;
begin
    for f in select p.oid::regprocedure as signature, p.prosecdef from pg_proc p where p.oid in (select to_regprocedure(s) from unnest(array[#{list}]::text[]) s) loop
        execute format('alter routine %s %s', f.signature, case when f.prosecdef then 'security definer' else 'security invoker' end);
    end loop;
end $sqiff$|]
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
                | [s] <- batch -> pure [(s, 0, notChecked <> errorText e)]
            Left e
                | [_] <- batch -> failWith [i|the body check failed: #{errorText e}|]
                | otherwise -> do
                    let (left, right) = L.splitAt (length batch `div` 2) batch
                    (<>) <$> inBatches extSchema left <*> inBatches extSchema right
    chunks xs = case L.splitAt 16 xs of
        ([], _) -> []
        (b, rest) -> b : chunks rest
    params = contramap fst (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text)))) <> contramap snd (E.param (E.nonNullable E.text))
    decoder = D.rowList ((,,) <$> textColumn <*> D.column (D.nonNullable D.int4) <*> textColumn)
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
    literal t = [i|'#{TP.replace "'" "''" t}'|] :: Text


-- | How 'bodyErrors' starts the message for a function it could not check.
notChecked :: Text
notChecked = "not checked: "


-- | The message of an error the server raised; 'Nothing' for connection and client errors.
serverError :: Errors.SessionError -> Maybe Text
serverError e = case e of
    Errors.ScriptSessionError _ _ -> Just (errorText e)
    Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError _) -> Just (errorText e)
    _ -> Nothing


errorText :: Errors.SessionError -> Text
errorText = \case
    Errors.ScriptSessionError _ (Errors.ServerError _ message detail _ _) -> message <> maybe "" ("; " <>) detail
    Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError _ message detail _ _)) -> message <> maybe "" ("; " <>) detail
    e -> tshow e


-- | Where in the SQL the server put the error, as a 1-based character index.
errorPosition :: Errors.SessionError -> Maybe Int
errorPosition = \case
    Errors.ScriptSessionError _ (Errors.ServerError _ _ _ _ position) -> position
    Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError _ _ _ _ position)) -> position
    _ -> Nothing


-- | The column names of the given relations, by schema and name.
relationColumns :: Connection.Connection -> [(Text, Text)] -> IO (Map (Text, Text) (Set Text))
relationColumns conn relations =
    Map.fromListWith (<>)
        . map (\(s, r, c) -> ((s, r), Set.singleton c))
        . toList
        <$> run conn (V.fromList (map fst relations), V.fromList (map snd relations)) query
  where
    query :: Statement.Statement (Vector Text, Vector Text) (Vector (Text, Text, Text))
    query =
        [vectorStatement|
            select
                n.nspname::text
              , c.relname::text
              , a.attname::text
            from unnest($1::text[], $2::text[]) u (s, r)
            join pg_namespace n
              on n.nspname = u.s
            join pg_class c
              on c.relnamespace = n.oid
             and c.relname = u.r
            join pg_attribute a
              on a.attrelid = c.oid
             and a.attnum > 0
             and not a.attisdropped
        |]


-- | The schema plpgsql_check is installed in, if it is.
plpgsqlCheckSchema :: Statement.Statement () (Maybe Text)
plpgsqlCheckSchema =
    [maybeStatement|
        select extnamespace::regnamespace::text
        from pg_extension
        where extname = 'plpgsql_check'
    |]


run :: Connection.Connection -> params -> Statement.Statement params a -> IO a
run conn params statement =
    Connection.use conn (Session.statement params statement)
        >>= either (\e -> failWith [i|query failed: #{errorText e}|]) pure


excludedParam :: E.Params [Text]
excludedParam = E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text)))


describeParams :: E.Params ([Text], Bool, Maybe Text, Maybe Int32)
describeParams =
    contramap (\(excluded, _, _, _) -> excluded) excludedParam
        <> contramap (\(_, includePartitions, _, _) -> includePartitions) (E.param (E.nonNullable E.bool))
        <> contramap (\(_, _, onlyTable, _) -> onlyTable) (E.param (E.nullable E.text))
        <> contramap (\(_, _, _, half) -> half) (E.param (E.nullable E.int4))


catalogIdsParams :: E.Params ([Int64], [Int64], [Int32])
catalogIdsParams =
    contramap (\(c, _, _) -> c) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int8))))
        <> contramap (\(_, o, _) -> o) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int8))))
        <> contramap (\(_, _, s) -> s) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int4))))


catalogIdRow :: D.Row CatalogId
catalogIdRow = (,,) <$> int8Column <*> int8Column <*> D.column (D.nonNullable D.int4)


int8Column :: D.Row Int64
int8Column = D.column (D.nonNullable D.int8)


int8Array :: D.Row [Int64]
int8Array = D.column (D.nonNullable (D.listArray (D.nonNullable D.int8)))


int4Array :: D.Row [Int32]
int4Array = D.column (D.nonNullable (D.listArray (D.nonNullable D.int4)))


addressRow :: D.Row Address
addressRow = Address <$> textColumn <*> textArrayColumn <*> textArrayColumn


textColumn :: D.Row Text
textColumn = D.column (D.nonNullable D.text)


textArrayColumn :: D.Row [Text]
textArrayColumn = D.column (D.nonNullable (D.listArray (D.nonNullable D.text)))
