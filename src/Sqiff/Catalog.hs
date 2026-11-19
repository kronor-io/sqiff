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
    CatalogId (..),
    ClassOid (..),
    pgClass,
    pgProc,
    describe,
    Partitions (..),
    Addresses,
    describeOn,
    dependencies,
    dependenciesOn,
    dependenciesFrom,
    edgesBetween,
    relationColumns,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.FileEmbed (embedDir, embedFile, makeRelativeToProject)
import Data.Functor.Contravariant (contramap)
import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement qualified as Statement
import Hasql.TH (vectorStatement)
import RIO
import RIO.FilePath (takeBaseName)
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import RIO.Vector qualified as V
import Sqiff.Catalog.Rows
import Sqiff.Connection (failWith, qualifiedNames, run)
import Sqiff.Definition (Definition (..))
import Sqiff.Identifier (Identifier, QName (..), identifier, nameText, qname)
import Sqiff.Model
import Sqiff.Timing (timed)


describeQueries :: [(FilePath, ByteString)]
describeQueries = $(makeRelativeToProject "sql/describe" >>= embedDir)


dependQuery :: ByteString
dependQuery = $(makeRelativeToProject "sql/depend.sql" >>= embedFile)


-- | Each described object, by its catalog ids.
type Addresses = Map CatalogId ObjectId


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
            definitions <- for rows \(_, object, text) ->
                either (\e -> failWith [i|the catalog's description of #{render object} does not parse: #{e}|]) (pure . (ObjectKey (aspectOf name) object,)) (decodeDefinition name text)
            evaluate
                $ force
                    ( Map.fromList definitions
                    , Map.fromList [(ids, object) | (ids, object, _) <- rows]
                    )
      where
        name = takeBaseName file

    describeStatement body =
        Statement.unpreparable (wrap (decodeUtf8Lenient body)) describeParams
            $ D.rowList ((,,) <$> catalogIdRow <*> objectRow <*> textColumn)

    aspectOf = \case
        "privileges" -> Privileges
        "comment" -> Comment
        _ -> Definition

    -- Most queries describe their objects as JSON; routines, triggers, policies,
    -- schemas and comments are kept as the catalog prints them.
    decodeDefinition :: String -> Text -> Either String Definition
    decodeDefinition = \case
        "column" -> fmap ColumnDefinition . Aeson.eitherDecodeStrict . encodeUtf8
        "privileges" -> fmap AclDefinition . Aeson.eitherDecodeStrict . encodeUtf8
        "index" -> fmap IndexDefinition . Aeson.eitherDecodeStrict . encodeUtf8
        "constraint" -> fmap ConstraintDefinition . Aeson.eitherDecodeStrict . encodeUtf8
        "relation" -> Aeson.eitherDecodeStrict . encodeUtf8 >=> relation
        "sequence" -> fmap SequenceDefinition . Aeson.eitherDecodeStrict . encodeUtf8
        "extension" -> fmap ExtensionDefinition . Aeson.eitherDecodeStrict . encodeUtf8
        "type" -> fmap TypeDefinition . Aeson.eitherDecodeStrict . encodeUtf8
        _ -> Right . Printed
    relation = Aeson.parseEither \v -> flip (Aeson.withObject "relation") v \o ->
        o Aeson..: "kind" >>= \case
            "view" -> ViewDefinition <$> Aeson.parseJSON v
            "table" -> TableDefinition <$> Aeson.parseJSON v
            other -> fail ("unknown relation kind " <> T.unpack other)

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
                -- Functions a type makes for itself, such as a range type's constructors,
                -- come and go with the type.
                and not (
                    o.classid = 'pg_proc'::regclass
                    and exists (select from pg_depend d where d.classid = o.classid and d.objid = o.objid and d.deptype = 'i')
                )
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
dependencies conn addresses =
    edgesBetween addresses <$> run conn (Map.keys addresses) (Statement.unpreparable (decodeUtf8Lenient dependQuery) catalogIdsParams edgeRows)


-- | 'dependencies' limited to the edges whose referenced end is one of @targets@.
dependenciesOn :: Connection.Connection -> Addresses -> [CatalogId] -> IO [Edge]
dependenciesOn _ _ [] = pure []
dependenciesOn conn addresses targets =
    timed [i|dependenciesOn n=#{length targets}|]
        $ edgesBetween addresses
        . filter (\(_, referenced) -> referenced `Set.member` targetSet)
        <$> run conn targets (Statement.unpreparable (decodeUtf8Lenient dependOnQuery) catalogIdsParams edgeRows)
  where
    targetSet = Set.fromList targets


dependOnQuery :: ByteString
dependOnQuery = $(makeRelativeToProject "sql/depend_on.sql" >>= embedFile)


-- | 'dependencies' limited to the edges reachable from @starts@ through dependents.
dependenciesFrom :: Connection.Connection -> Addresses -> [CatalogId] -> IO [Edge]
dependenciesFrom _ _ [] = pure []
dependenciesFrom conn addresses starts =
    timed [i|dependenciesFrom n=#{length starts}|]
        $ edgesBetween addresses
        <$> run conn starts (Statement.unpreparable (decodeUtf8Lenient dependFromQuery) catalogIdsParams edgeRows)


dependFromQuery :: ByteString
dependFromQuery = $(makeRelativeToProject "sql/depend_from.sql" >>= embedFile)


-- | The dependency queries' one row: dependents and referenced objects as six parallel arrays.
edgeRows :: D.Result [(CatalogId, CatalogId)]
edgeRows = D.singleRow (zip <$> catalogIdArrays <*> catalogIdArrays)
  where
    catalogIdArrays = L.zipWith3 CatalogId <$> (map ClassOid <$> int8Array) <*> int8Array <*> int4Array


-- | The edges whose ends are both described.
edgesBetween :: Addresses -> [(CatalogId, CatalogId)] -> [Edge]
edgesBetween addresses pairs =
    [Edge a b | (x, y) <- pairs, Just a <- [Map.lookup x addresses], Just b <- [Map.lookup y addresses]]


-- | The column names of the given relations, by schema and name.
relationColumns :: Connection.Connection -> [QName] -> IO (Map QName (Set Identifier))
relationColumns conn relations =
    Map.fromListWith (<>)
        . map (\(s, r, c) -> (qname s r, Set.singleton (identifier c)))
        . toList
        <$> run conn (V.fromList (map (nameText . (.schema)) relations), V.fromList (map (nameText . (.name)) relations)) query
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


describeParams :: E.Params ([Text], Bool, Maybe Text, Maybe Int32)
describeParams =
    contramap (\(excluded, _, _, _) -> excluded) excludedParam
        <> contramap (\(_, includePartitions, _, _) -> includePartitions) (E.param (E.nonNullable E.bool))
        <> contramap (\(_, _, onlyTable, _) -> onlyTable) (E.param (E.nullable E.text))
        <> contramap (\(_, _, _, half) -> half) (E.param (E.nullable E.int4))
