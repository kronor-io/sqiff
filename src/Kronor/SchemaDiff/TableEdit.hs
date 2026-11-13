{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Compares the table a deploy file creates with the one before the edit, or with the
-- live table, in a transaction that is always rolled back.
module Kronor.SchemaDiff.TableEdit (
    TableEdit (..),
    Baseline (..),
    Drops (..),
    compareTable,
    compareTableWith,
    withEditedTable,
    foreignKeyError,
    firstFailure,
    typeChangeFacts,
    aside,
    columnOrder,
) where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Hasql.TH (singletonStatement, vectorStatement)
import Kronor.SchemaDiff.Catalog (Address (..), Aspect (..), ObjectKey (..), Partitions (..), Schema, describeOn, errorText, failWith)
import Kronor.SchemaDiff.Diff (Change (..), diff)
import Kronor.SchemaDiff.Object
import Kronor.SchemaDiff.Timing (timed)
import RIO
import RIO.Map qualified as Map


-- | The error PostgreSQL gives for a foreign key from columns of the first types to columns
-- of the second, tried on temporary tables in a transaction that is rolled back.
foreignKeyError :: Connection.Connection -> [(Text, Text)] -> IO (Maybe Text)
foreignKeyError conn types = do
    result <- Connection.use conn (Session.script [i|begin; create temp table schema_diff_pk (#{columns snd}, primary key (#{names})); create temp table schema_diff_fk (#{columns fst}, foreign key (#{names}) references schema_diff_pk (#{names}))|])
    _ <- Connection.use conn (Session.script "rollback")
    pure (either (Just . tidy . errorText) (const Nothing) result)
  where
    tidy e = case T.breakOn "are of incompatible types" e of
        (_, "") -> e
        (_, rest) -> [i|the key columns #{rest}|]
    names = T.intercalate ", " [[i|c#{n}|] | n <- [1 .. length types]]
    columns side = T.intercalate ", " [[i|c#{n} #{side ty}|] | (n, ty) <- zip [1 :: Int ..] types]


-- | Whether changing a column from @old@ to @new@ rewrites the table, and whether @new@ is
-- a domain, found on a temporary table in a transaction that is rolled back.
typeChangeFacts :: Connection.Connection -> Text -> Text -> IO (Maybe (Bool, Bool))
typeChangeFacts conn old new = do
    _ <- Connection.use conn (Session.script [i|begin; create temp table schema_diff_retype (c #{old})|])
    before <- fileNode
    altered <- Connection.use conn (Session.script [i|alter table pg_temp.schema_diff_retype alter column c type #{new} using c::#{new}|])
    after <- fileNode
    domain <- Connection.use conn (Session.statement new isDomain)
    _ <- Connection.use conn (Session.script "rollback")
    pure case (before, altered, after, domain) of
        (Right b, Right (), Right a, Right d) -> Just (a /= b, d)
        _ -> Nothing
  where
    fileNode =
        Connection.use conn
            $ Session.statement
                ()
                [singletonStatement|
                    select relfilenode::int8
                    from pg_class
                    where oid = to_regclass('pg_temp.schema_diff_retype')
                |]
    isDomain =
        [singletonStatement|
            select coalesce((select typtype = 'd' from pg_type where oid = to_regtype($1::text)), false)::bool
        |]


-- | The table's column names in the table's order.
columnOrder :: Connection.Connection -> QName -> IO [Text]
columnOrder conn table =
    Connection.use conn (Session.statement (qualified table) query)
        >>= either (\e -> failWith [i|could not read the columns of #{qualified table}: #{errorText e}|]) (pure . toList)
  where
    query =
        [vectorStatement|
            select attname::text
            from pg_attribute
            where attrelid = to_regclass($1::text)
              and attnum > 0
              and not attisdropped
            order by attnum
        |]


-- | The first of @statements@ that fails, with its error. Runs inside the caller's
-- transaction under a savepoint that is rolled back.
firstFailure :: Connection.Connection -> [Text] -> IO (Maybe Text)
firstFailure conn statements = do
    _ <- Connection.use conn (Session.script "savepoint schema_diff_first_failure")
    result <- go (zip [1 :: Int ..] statements)
    _ <- Connection.use conn (Session.script "rollback to savepoint schema_diff_first_failure")
    pure result
  where
    go [] = pure Nothing
    go ((n, s) : rest) = Connection.use conn (Session.script s) >>= either (\e -> pure (Just [i|statement #{n}: #{errorText e}|])) (const (go rest))


data TableEdit = TableEdit
    { current :: Schema
    , desired :: Schema
    , changes :: Map ObjectKey Change
    , ignored :: [(ObjectKey, Text)]
    -- ^ Differences left out, with the reason.
    }


aside :: Text
aside = "schema_diff_aside"


-- | What the edited file is compared with.
data Baseline
    = -- | The statements of the file before the edit. Drift between the files and the
      -- database cancels out, and everything the edit removes is meant.
      BaseFile [Text]
    | -- | The table as it is.
      LiveTable Drops


-- | What happens to columns and constraints the live table has and the file does not.
data Drops = ReportDrops | AllowDrops
    deriving stock (Eq, Show)


-- | Whether a pass over the edit describes the table.
data Describing = DescribeTable | SkipDescribe
    deriving stock (Eq)


compareTable :: Connection.Connection -> Baseline -> QName -> [Text] -> IO TableEdit
compareTable conn baseline table statements = fst <$> compareTableWith conn baseline table statements (pure ()) (const (pure ()))


-- | 'compareTable' that also runs @before@ in the transaction before anything changes,
-- and @after@ once the table is what the edited file creates.
compareTableWith :: Connection.Connection -> Baseline -> QName -> [Text] -> IO b -> (b -> IO a) -> IO (TableEdit, a)
compareTableWith = editTable DescribeTable


-- | 'compareTableWith' for the result of @after@ alone: the table is neither built from
-- the file before the edit nor described.
withEditedTable :: Connection.Connection -> Baseline -> QName -> [Text] -> IO b -> (b -> IO a) -> IO a
withEditedTable conn baseline table statements beforeEdit afterEdit = snd <$> editTable SkipDescribe conn baseline table statements beforeEdit afterEdit


editTable :: Describing -> Connection.Connection -> Baseline -> QName -> [Text] -> IO b -> (b -> IO a) -> IO (TableEdit, a)
editTable describing conn baseline table statements beforeEdit afterEdit = do
    (current, desired, afterResult) <-
        ( do
            script "begin"
            xact <- currentXact
            script "set local lock_timeout = '5s'"
            exists <- script' [i|select 1 from pg_class where oid = to_regclass(#{literal (qualified table)})|]
            unless exists $ failWith [i|#{qualified table} does not exist yet; a new table only needs its own change|]
            beforeResult <- timed "tableEdit.before" beforeEdit
            owned <-
                Connection.use conn (Session.statement (qualified table) ownedSequences)
                    >>= either setupFailed (pure . toList)
            for_ (filter (\s -> any ((s `T.isInfixOf`) . withoutLineComments) (statements <> baseStatements)) owned) \s ->
                script [i|alter sequence #{tableSchemaRef}#{quoteIdent s} owned by none|]
            script [i|create schema #{aside}|]
            script [i|alter table #{qualified table} set schema #{aside}|]
            current <- timed "tableEdit.current" case baseline of
                _ | describing == SkipDescribe -> pure Map.empty
                BaseFile base -> do
                    script "savepoint schema_diff_base"
                    built <- build xact "before the edit" base
                    script "rollback to savepoint schema_diff_base"
                    pure built
                LiveTable _ -> do
                    schema <- describeTable (QName aside table.name)
                    pure (Map.fromList [(moveBack k, restore def) | (k, def) <- Map.toList schema, belongsTo (QName aside table.name) k def])
            desired <- timed "tableEdit.desired" $ build xact "as edited" statements
            afterResult <- timed "tableEdit.after" $ afterEdit beforeResult
            pure (current, desired, afterResult)
        )
            `finally` void (Connection.use conn (Session.script "rollback"))
    let all' = diff (\_ a b -> a == b) current (Map.mapWithKey (keepSettings current) desired)
        reasons = Map.mapMaybeWithKey leftOut all'
    pure
        ( TableEdit
            { current
            , desired
            , changes = all' `Map.difference` reasons
            , ignored = Map.toList reasons
            }
        , afterResult
        )
  where
    script sql = Connection.use conn (Session.script sql) >>= either setupFailed pure

    setupFailed e = failWith [i|could not set up the comparison: #{errorText e}|]

    baseStatements = case baseline of
        BaseFile base -> base
        LiveTable _ -> []

    build :: Text -> Text -> [Text] -> IO Schema
    build xact label stmts = do
        for_ (zip [1 :: Int ..] stmts) \(n, statement) -> do
            Connection.use conn (Session.script statement)
                >>= either (\e -> failWith [i|statement #{n} of the file #{label} failed: #{errorText e}|]) pure
            after <- currentXact
            when (after /= xact)
                $ failWith [i|statement #{n} of the file #{label} ended the transaction the comparison runs in|]
        if describing == DescribeTable
            then do
                schema <- describeTable table
                pure (Map.fromList [(k, def) | (k, def) <- Map.toList schema, belongsTo table k def])
            else pure Map.empty

    currentXact =
        Connection.use conn (Session.statement () [singletonStatement| select pg_current_xact_id() :: text |])
            >>= either setupFailed pure

    describeTable q = timed "tableEdit.describeTable" $ fst <$> describeOn [conn] [] SkipPartitions (Just (qualified q))

    ownedSequences =
        [vectorStatement|
            select s.relname::text
            from pg_class s
            join pg_depend d
              on d.classid = 'pg_class'::regclass
             and d.objid = s.oid
             and d.deptype = 'a'
            join pg_class t
              on t.oid = d.refobjid
            where s.relkind = 'S'
              and t.oid = to_regclass($1::text)
        |]

    withoutLineComments = T.unlines . map (fst . T.breakOn "--") . T.lines

    script' :: Text -> IO Bool
    script' sql = do
        result <- Connection.use conn (Session.script [i|do $$ begin if not exists (#{sql}) then raise exception 'missing'; end if; end $$|])
        pure (isRight result)

    literal :: Text -> Text
    literal t = [i|'#{T.replace "'" "''" t}'|]

    asideRef = [i|#{quoteIdent aside}.|] :: Text
    tableSchemaRef = let schema = quoteIdent table.schema in [i|#{schema}.|] :: Text

    moveBack k = case k.address.objectNames of
        s : rest | s == aside -> k{address = k.address{objectNames = table.schema : rest}}
        _ -> k

    restore = T.replace asideRef tableSchemaRef

    belongsTo q k def = case fromAddress k.address of
        Table t -> t == q
        Index _ -> any (`T.isInfixOf` def) [[i| ON #{qualified q} |], [i| ON ONLY #{qualified q} |]]
        Sequence _ -> False
        o -> owningTable o == Just q

    keepSettings current k def = case (fromAddress k.address, k.aspect, Map.lookup k current) of
        (Table _, Definition, Just old) ->
            let settings = filter (\l -> l `elem` ["row level security", "force row level security"] || "with (" `T.isPrefixOf` l) (T.lines old)
                missing = filter (`notElem` T.lines def) settings
                hasOptions = any ("with (" `T.isPrefixOf`) (T.lines def)
             in T.intercalate "\n" (T.lines def <> filter (\l -> not ("with (" `T.isPrefixOf` l && hasOptions)) missing)
        _ -> def

    leftOut k c
        | BaseFile _ <- baseline = Nothing
        | k.aspect == Privileges = Just "grants are kept outside table files"
        | otherwise = case (fromAddress k.address, c) of
            (Index _, Dropped _) -> Just "not in the file; indexes are often kept in their own files"
            (Trigger _ _, Dropped _) -> Just "not in the file; triggers are kept in their own files"
            (Policy _ _, Dropped _) -> Just "not in the file; policies may be kept in other files"
            (_, Dropped _) | LiveTable ReportDrops <- baseline -> Just "in the database but not in the file; --allow-drops drops it"
            _ -> Nothing
