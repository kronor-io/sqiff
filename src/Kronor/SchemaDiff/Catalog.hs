{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Reads a database's schema from its catalog: one definition per object, keyed by
-- @pg_identify_object_as_address@ so the same object matches across databases.
module Kronor.SchemaDiff.Catalog (
    Aspect (..),
    Address (..),
    ObjectKey (..),
    Schema,
    Edge (..),
    UserError (..),
    failWith,
    connect,
    describe,
    Addresses,
    describeOn,
    dependencies,
    qualifiedNames,
    Skipped,
    bodyDependencies,
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
import Pqi.Ffi qualified
import RIO
import RIO.FilePath (takeBaseName)
import RIO.List (unzip3, zip3)
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Text qualified as T
import RIO.Text.Partial qualified as TP


data Aspect = Definition | Privileges
    deriving stock (Eq, Ord, Show)


data Address = Address
    { objectType :: Text
    , objectNames :: [Text]
    , objectArgs :: [Text]
    }
    deriving stock (Eq, Ord, Show)


data ObjectKey = ObjectKey
    { aspect :: Aspect
    , address :: Address
    }
    deriving stock (Eq, Ord, Show)


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


-- | A failure the command line reports as one line.
newtype UserError = UserError Text
    deriving stock (Show)


instance Exception UserError


failWith :: Text -> IO a
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


-- | Every object outside system schemas, extension members and the excluded schemas.
-- Partitions and their columns only when asked for.
describe :: Connection.Connection -> [Text] -> Bool -> IO Schema
describe conn excluded includePartitions = fst <$> describeOn [conn] excluded includePartitions Nothing


-- | 'describe' with the queries spread over the connections, limited to one table's
-- objects when @onlyTable@ names it, and with each object's catalog ids.
describeOn :: [Connection.Connection] -> [Text] -> Bool -> Maybe Text -> IO (Schema, Addresses)
describeOn conns excluded includePartitions onlyTable = do
    queue <- newIORef describeQueries
    let next = atomicModifyIORef' queue \case
            [] -> ([], Nothing)
            q : rest -> (rest, Just q)
        worker conn = next >>= maybe (pure []) (\q -> (:) <$> describeOne conn q <*> worker conn)
    parts <- concat <$> forConcurrently conns (\conn -> qualifiedNames conn (worker conn))
    pure (Map.unions (map fst parts), Map.unions (map snd parts))
  where
    describeOne conn (file, body) = do
        rows <-
            run conn (excluded, includePartitions, onlyTable)
                $ Statement.unpreparable (wrap (decodeUtf8Lenient body)) describeParams (D.rowList ((,,) <$> catalogIdRow <*> addressRow <*> textColumn))
        let objectAspect = if takeBaseName file == "privileges" then Privileges else Definition
        pure
            ( Map.fromList [(ObjectKey objectAspect addr, def) | (_, addr, def) <- rows]
            , Map.fromList [(ids, addr) | (ids, addr, _) <- rows]
            )

    -- Each query returns its objects' ids, the table they belong to as relid, and their definition.
    wrap body =
        [i|
            with o as (
            #{body}
            )
            select o.classid::int8, o.objid::int8, o.objsubid::int4, a.type, a.object_names, a.object_args, coalesce(o.definition, '')
            from o
            cross join lateral pg_identify_object_as_address(o.classid, o.objid, o.objsubid) a
            where ($3::text is null or o.relid = to_regclass($3::text))
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


-- | The edges whose ends are both described.
edgesBetween :: Addresses -> [(CatalogId, CatalogId)] -> [Edge]
edgesBetween addresses pairs =
    [Edge a b | (x, y) <- pairs, Just a <- [Map.lookup x addresses], Just b <- [Map.lookup y addresses]]


-- | Runs @action@ with an empty @search_path@, so that names printed by the catalog
-- functions are schema-qualified whatever the session's path is.
qualifiedNames :: Connection.Connection -> IO a -> IO a
qualifiedNames conn action = do
    previous <- run conn () (Statement.unpreparable "select current_setting('search_path')" mempty (D.singleRow textColumn))
    void (run conn "" setPath)
    action `finally` void (Connection.use conn (Session.statement previous setPath))
  where
    setPath = Statement.unpreparable "select set_config('search_path', $1, false)" (E.param (E.nonNullable E.text)) (D.singleRow textColumn)


-- | Functions plpgsql_check could not analyse, with the reason.
type Skipped = [(Text, Text)]


-- | What PL/pgSQL bodies use among the described objects, from plpgsql_check, with the
-- functions shared out over the connections; 'Nothing' when it is not installed.
bodyDependencies :: NonEmpty Connection.Connection -> [Text] -> Addresses -> IO (Maybe ([Edge], Skipped))
bodyDependencies conns@(conn :| _) excluded addresses =
    qualifiedNames conn
        $ run conn () (Statement.unpreparable "select extnamespace::regnamespace::text from pg_extension where extname = 'plpgsql_check'" mempty (D.rowMaybe textColumn))
        >>= traverse \extSchema -> do
            functions <- run conn excluded (Statement.unpreparable (decodeUtf8Lenient plpgsqlFunctionsQuery) excludedParam (D.rowList functionRow))
            let (unchecked, checkable) = partitionEithers (map triggerTable functions)
                query = TP.replace "@extschema@" extSchema (decodeUtf8Lenient plpgsqlDependQuery)
                shares = [[f | (n, f) <- zip [0 :: Int ..] checkable, n `mod` length conns == k] | k <- [0 .. length conns - 1]]
            results <- forConcurrently (zip (toList conns) shares) (\(c, share) -> inBatches c query share)
            let position = Map.fromList (zip [name | (_, name, _) <- checkable] [0 :: Int ..])
                failed = L.sortOn (\(name, _) -> Map.lookup name position) (concatMap snd results)
            pure (concatMap fst results, unchecked <> failed)
  where
    functionRow = (,,,) <$> D.column (D.nonNullable D.int8) <*> textColumn <*> D.column (D.nonNullable D.bool) <*> D.column (D.nullable D.int8)
    triggerTable (oid, name, isTrigger, relid)
        | isTrigger && isNothing relid = Left (name, "no trigger uses it")
        | otherwise = Right (oid, name, relid)

    -- One query for all functions; a batch that fails is split until the failing functions stand alone.
    inBatches _ _ [] = pure ([], [])
    inBatches c query batch =
        Connection.use c (Session.statement batch (Statement.unpreparable query dependParams (D.rowList useRow)))
            >>= \case
                Right uses -> pure (edgesBetween addresses [((pgProc, f, 0), (k, o, 0)) | (f, k, o) <- uses], [])
                Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError _ message _ _ _)))
                    | [(_, name, _)] <- batch -> pure ([], [(name, message)])
                    | otherwise -> do
                        let (left, right) = L.splitAt (length batch `div` 2) batch
                        (e1, s1) <- inBatches c query left
                        (e2, s2) <- inBatches c query right
                        pure (e1 <> e2, s1 <> s2)
                Left e -> failWith ("query failed: " <> tshow e)

    dependParams =
        contramap (map (\(oid, _, _) -> oid)) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int8))))
            <> contramap (map (\(_, _, relid) -> relid)) (E.param (E.nonNullable (E.foldableArray (E.nullable E.int8))))

    useRow = (,,) <$> int8Column <*> int8Column <*> int8Column

    -- pg_proc's own oid, which is fixed.
    pgProc = 1255


render :: Address -> Text
render addr = addr.objectType <> " " <> T.intercalate "." addr.objectNames <> args
  where
    args
        | null addr.objectArgs = ""
        | otherwise = "(" <> T.intercalate ", " addr.objectArgs <> ")"


run :: Connection.Connection -> params -> Statement.Statement params a -> IO a
run conn params statement =
    Connection.use conn (Session.statement params statement)
        >>= either (\e -> failWith ("query failed: " <> tshow e)) pure


excludedParam :: E.Params [Text]
excludedParam = E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text)))


describeParams :: E.Params ([Text], Bool, Maybe Text)
describeParams =
    contramap (\(excluded, _, _) -> excluded) excludedParam
        <> contramap (\(_, includePartitions, _) -> includePartitions) (E.param (E.nonNullable E.bool))
        <> contramap (\(_, _, onlyTable) -> onlyTable) (E.param (E.nullable E.text))


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
