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
import RIO.Map qualified as Map
import RIO.Text qualified as T


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


-- | @dependent@ needs @referenced@ to exist, as recorded in @pg_depend@.
data Edge = Edge
    { dependent :: Address
    , referenced :: Address
    }
    deriving stock (Eq, Ord, Show)


describeQueries :: [(FilePath, ByteString)]
describeQueries = $(makeRelativeToProject "sql/describe" >>= embedDir)


dependQuery :: ByteString
dependQuery = $(makeRelativeToProject "sql/depend.sql" >>= embedFile)


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
describe conn excluded includePartitions = Map.unions <$> traverse describeOne describeQueries
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
                        case when a.type = 'schema' then a.object_names[1] end
                    ) = any($1),
                    false
                )
                and ($2 or not exists (select from pg_class p where p.relispartition and p.oid = owner.relid))
        |]


dependencies :: Connection.Connection -> [Text] -> IO [Edge]
dependencies conn excluded =
    run conn excluded
        $ Statement.unpreparable (decodeUtf8Lenient dependQuery) excludedParam (D.rowList (Edge <$> addressRow <*> addressRow))


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
