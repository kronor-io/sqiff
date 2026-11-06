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


-- | Every object outside system schemas, extension members and the excluded schemas.
-- Partitions and their columns only when asked for.
describe :: Connection.Connection -> [Text] -> Bool -> IO Schema
describe conn excluded includePartitions = qualifiedNames conn (Map.unions <$> traverse describeOne describeQueries)
  where
    describeOne (file, body) = do
        rows <-
            run conn (excluded, includePartitions)
                $ Statement.unpreparable (wrap (decodeUtf8Lenient body)) describeParams (D.rowList objectRow)
        let objectAspect = if takeBaseName file == "privileges" then Privileges else Definition
        pure $ Map.fromList [(ObjectKey objectAspect addr, def) | (addr, def) <- rows]

    wrap body =
        [i|
            with o as (
            #{body}
            )
            select a.type, a.object_names, a.object_args, coalesce(o.definition, '')
            from o
            cross join lateral pg_identify_object_as_address(o.classid, o.objid, o.objsubid) a
            cross join lateral (
                select case o.classid
                    when 'pg_class'::regclass::oid then
                        coalesce((select x.indrelid from pg_index x where x.indexrelid = o.objid), o.objid)
                    when 'pg_constraint'::regclass::oid then
                        (select c.conrelid from pg_constraint c where c.oid = o.objid)
                    when 'pg_trigger'::regclass::oid then
                        (select t.tgrelid from pg_trigger t where t.oid = o.objid)
                    when 'pg_policy'::regclass::oid then
                        (select pl.polrelid from pg_policy pl where pl.oid = o.objid)
                end as relid
            ) owner
            where not exists (
                select
                from pg_depend e
                where e.deptype = 'e'
                    and (
                        (e.classid = o.classid and e.objid = o.objid)
                        or (e.classid = 'pg_class'::regclass and e.objid = owner.relid)
                    )
            )
                and not coalesce(
                    coalesce(
                        (select ident.schema from pg_identify_object(o.classid, o.objid, o.objsubid) ident),
                        (select n.nspname::text from pg_class c join pg_namespace n on n.oid = c.relnamespace where c.oid = owner.relid),
                        case when a.type = 'schema' then a.object_names[1] end
                    ) = any($1),
                    false
                )
                and ($2 or not exists (select from pg_class p where p.relispartition and p.oid = owner.relid))
        |]


dependencies :: Connection.Connection -> [Text] -> IO [Edge]
dependencies conn excluded =
    qualifiedNames conn
        . run conn excluded
        $ Statement.unpreparable (decodeUtf8Lenient dependQuery) excludedParam (D.rowList (Edge <$> addressRow <*> addressRow))


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


-- | What PL/pgSQL bodies use, from plpgsql_check; 'Nothing' when it is not installed.
bodyDependencies :: Connection.Connection -> [Text] -> IO (Maybe ([Edge], Skipped))
bodyDependencies conn excluded =
    qualifiedNames conn
        $ run conn () (Statement.unpreparable "select extnamespace::regnamespace::text from pg_extension where extname = 'plpgsql_check'" mempty (D.rowMaybe textColumn))
        >>= traverse \extSchema -> do
            functions <- run conn excluded (Statement.unpreparable (decodeUtf8Lenient plpgsqlFunctionsQuery) excludedParam (D.rowList functionRow))
            let (unchecked, checkable) = partitionEithers (map triggerTable functions)
                query = TP.replace "@extschema@" extSchema (decodeUtf8Lenient plpgsqlDependQuery)
            (edges, failed) <- inBatches query checkable
            pure (edges, unchecked <> failed)
  where
    functionRow = (,,,) <$> D.column (D.nonNullable D.int8) <*> textColumn <*> D.column (D.nonNullable D.bool) <*> D.column (D.nullable D.int8)
    triggerTable (oid, name, isTrigger, relid)
        | isTrigger && isNothing relid = Left (name, "no trigger uses it")
        | otherwise = Right (oid, name, relid)

    -- One query for all functions; a batch that fails is split until the failing functions stand alone.
    inBatches _ [] = pure ([], [])
    inBatches query batch =
        Connection.use conn (Session.statement (batch, excluded) (Statement.unpreparable query dependParams (D.rowList (Edge <$> addressRow <*> addressRow))))
            >>= \case
                Right edges -> pure (edges, [])
                Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError _ message _ _ _)))
                    | [(_, name, _)] <- batch -> pure ([], [(name, message)])
                    | otherwise -> do
                        let (left, right) = L.splitAt (length batch `div` 2) batch
                        (e1, s1) <- inBatches query left
                        (e2, s2) <- inBatches query right
                        pure (e1 <> e2, s1 <> s2)
                Left e -> failWith ("query failed: " <> tshow e)

    dependParams =
        contramap (map (\(oid, _, _) -> oid) . fst) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int8))))
            <> contramap (map (\(_, _, relid) -> relid) . fst) (E.param (E.nonNullable (E.foldableArray (E.nullable E.int8))))
            <> contramap snd excludedParam


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


describeParams :: E.Params ([Text], Bool)
describeParams = contramap fst excludedParam <> contramap snd (E.param (E.nonNullable E.bool))


objectRow :: D.Row (Address, Text)
objectRow = (,) <$> addressRow <*> textColumn


addressRow :: D.Row Address
addressRow = Address <$> textColumn <*> textArrayColumn <*> textArrayColumn


textColumn :: D.Row Text
textColumn = D.column (D.nonNullable D.text)


textArrayColumn :: D.Row [Text]
textArrayColumn = D.column (D.nonNullable (D.listArray (D.nonNullable D.text)))
