{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What the catalog says about an object's aspect. Columns and grants arrive as
-- structured data and are decoded once, where the catalog is read; every other kind is
-- kept as the text the catalog prints.
--
-- 'printed' renders any definition as text, for messages, files and line diffs.
module Sqiff.Definition (
    Definition (..),
    printed,
    mapText,
    mapNames,
    asColumn,
    asTable,
    asView,
    asIndex,
    asConstraint,
    asSequence,
    asExtension,
    asType,
    asAcl,

    -- * Columns
    ColumnDef (..),
    Collation (..),
    collationSql,
    Position (..),
    positionNumber,
    Nullability (..),
    Validity (..),
    ColumnValue (..),
    IdentityKind (..),
    Storage (..),
    columnText,
    identityKindSql,
    storageSql,
    defaultExpr,
    isNotNull,
    notNullValid,
    notNullName,
    identityKindOf,
    identityOptionsOf,
    generatedOf,
    failsOnRows,

    -- * Relations
    TableDef (..),
    PartitionOf (..),
    tableText,
    ViewDef (..),
    ViewColumns (..),
    viewColumns,
    extendsColumns,
    viewText,
    withOptions,

    -- * Indexes and constraints
    IndexDef (..),
    ConstraintDef (..),
    ConstraintKind (..),
    ForeignKeyTarget (..),
    withoutValidity,

    -- * Sequences, extensions and types
    SequenceDef (..),
    sequenceOptions,
    ExtensionDef (..),
    TypeDef (..),

    -- * Privileges
    Acl (..),
    Grant (..),
    Grantor (..),
    Grantee (..),
    aclText,
    granteeSql,
) where

import Data.Aeson (FromJSON (..), Value (Null, String), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Set qualified as Set
import Sqiff.Identifier
import Sqiff.Sql (literal)


-- | One aspect of one object.
data Definition
    = ColumnDefinition ColumnDef
    | AclDefinition Acl
    | TableDefinition TableDef
    | ViewDefinition ViewDef
    | IndexDefinition IndexDef
    | ConstraintDefinition ConstraintDef
    | SequenceDefinition SequenceDef
    | ExtensionDefinition ExtensionDef
    | TypeDefinition TypeDef
    | -- | As the catalog prints it: a @create@ statement, an expression, a comment.
      Printed Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | The definition as text, in the form @sqiff describe@ writes.
printed :: Definition -> Text
printed = \case
    ColumnDefinition c -> columnText c
    AclDefinition acl -> aclText acl
    TableDefinition t -> tableText t
    ViewDefinition v -> viewText v
    IndexDefinition ix -> ix.statement
    ConstraintDefinition c -> c.clause
    SequenceDefinition q -> sequenceText q
    ExtensionDefinition e -> "version " <> e.version <> " schema " <> quoted e.schema
    TypeDefinition t -> typeDefText t
    Printed t -> t


-- | The definition with every piece of SQL text in it changed by @f@: expressions and
-- types, not names.
mapText :: (Text -> Text) -> Definition -> Definition
mapText f = \case
    ColumnDefinition c ->
        ColumnDefinition
            c
                { colType = typeName (f (typeText c.colType))
                , value = case c.value of
                    PlainColumn e -> PlainColumn (f <$> e)
                    GeneratedColumn storage e -> GeneratedColumn storage (f e)
                    identity -> identity
                }
    AclDefinition acl -> AclDefinition acl
    TableDefinition t ->
        TableDefinition
            t
                { partitionBy = f <$> t.partitionBy
                , partitionOf = (\p -> p{bound = f p.bound}) <$> t.partitionOf
                }
    ViewDefinition v -> ViewDefinition v{query = f v.query}
    IndexDefinition ix -> IndexDefinition ix{statement = f ix.statement}
    ConstraintDefinition c -> ConstraintDefinition c{clause = f c.clause}
    TypeDefinition (DomainType base notNull dflt) -> TypeDefinition (DomainType (typeName (f (typeText base))) notNull (f <$> dflt))
    def@(SequenceDefinition _) -> def
    def@(ExtensionDefinition _) -> def
    def@(TypeDefinition _) -> def
    Printed t -> Printed (f t)


-- | The definition with each schema-qualified name in it changed by @f@: an index's
-- table, a foreign key's, a partition's parent.
mapNames :: (QName -> QName) -> Definition -> Definition
mapNames f = \case
    TableDefinition t -> TableDefinition t{partitionOf = (\p -> p{parent = f p.parent}) <$> t.partitionOf}
    IndexDefinition (IndexDef table unique partitioned statement) -> IndexDefinition (IndexDef (f table) unique partitioned statement)
    ConstraintDefinition c -> ConstraintDefinition c{references = (\(ForeignKeyTarget table columns) -> ForeignKeyTarget (f table) columns) <$> c.references}
    SequenceDefinition q -> SequenceDefinition q{ownedBy = (\(table, column) -> (f table, column)) <$> q.ownedBy}
    def -> def


asColumn :: Definition -> Maybe ColumnDef
asColumn = \case
    ColumnDefinition c -> Just c
    _ -> Nothing


asTable :: Definition -> Maybe TableDef
asTable = \case
    TableDefinition t -> Just t
    _ -> Nothing


asView :: Definition -> Maybe ViewDef
asView = \case
    ViewDefinition v -> Just v
    _ -> Nothing


asIndex :: Definition -> Maybe IndexDef
asIndex = \case
    IndexDefinition ix -> Just ix
    _ -> Nothing


asConstraint :: Definition -> Maybe ConstraintDef
asConstraint = \case
    ConstraintDefinition c -> Just c
    _ -> Nothing


asSequence :: Definition -> Maybe SequenceDef
asSequence = \case
    SequenceDefinition q -> Just q
    _ -> Nothing


asExtension :: Definition -> Maybe ExtensionDef
asExtension = \case
    ExtensionDefinition e -> Just e
    _ -> Nothing


asType :: Definition -> Maybe TypeDef
asType = \case
    TypeDefinition t -> Just t
    _ -> Nothing


asAcl :: Definition -> Maybe Acl
asAcl = \case
    AclDefinition acl -> Just acl
    _ -> Nothing


-- | A table column, as @alter table ... add column@ would declare it.
data ColumnDef = ColumnDef
    { colType :: TypeName
    , collation :: Maybe Collation
    -- ^ Only when it is not the type's own.
    , nullability :: Nullability
    , value :: ColumnValue
    , rewritesOnAdd :: Bool
    -- ^ Adding the column rewrites the table: its default is volatile, or its type a
    -- domain with constraints.
    , position :: Position
    , inheritedOnly :: Bool
    -- ^ The table has it only because a parent does, so it comes and goes with the parent's.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | Where a column sits in its table (@attnum@), so a new table gets its columns in
-- order. It is not part of the column's definition: every position equals every other,
-- so the same column compares equal in two databases that added their columns in
-- different orders. Sort by 'positionNumber'.
newtype Position = Position Int
    deriving stock (Show)
    deriving newtype (NFData)


instance Eq Position where
    _ == _ = True


instance Ord Position where
    compare _ _ = EQ


positionNumber :: Position -> Int
positionNumber (Position n) = n


data Nullability
    = Nullable
    | -- | A not-null constraint, with its name when PostgreSQL would not give it that one.
      NotNull Validity (Maybe Identifier)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


data Validity = Validated | NotValidated
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | Where a column's values come from; a column has at most one of these.
data ColumnValue
    = -- | Written, or the default expression when there is one.
      PlainColumn (Maybe Text)
    | -- | With its sequence options that differ from the defaults, such as @increment by 5@.
      IdentityColumn IdentityKind [Text]
    | GeneratedColumn Storage Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


data IdentityKind = Always | ByDefault
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


data Storage = Stored | Virtual
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


identityKindSql :: IdentityKind -> Text
identityKindSql = \case
    Always -> "always"
    ByDefault -> "by default"


storageSql :: Storage -> Text
storageSql = \case
    Stored -> "stored"
    Virtual -> "virtual"


defaultExpr :: ColumnDef -> Maybe Text
defaultExpr c = case c.value of
    PlainColumn e -> e
    _ -> Nothing


isNotNull :: ColumnDef -> Bool
isNotNull c = case c.nullability of
    NotNull _ _ -> True
    Nullable -> False


-- | Whether the not-null constraint, if there is one, is validated.
notNullValid :: ColumnDef -> Bool
notNullValid c = case c.nullability of
    NotNull NotValidated _ -> False
    _ -> True


notNullName :: ColumnDef -> Maybe Identifier
notNullName c = case c.nullability of
    NotNull _ name -> name
    Nullable -> Nothing


identityKindOf :: ColumnDef -> Maybe IdentityKind
identityKindOf c = case c.value of
    IdentityColumn kind _ -> Just kind
    _ -> Nothing


identityOptionsOf :: ColumnDef -> [Text]
identityOptionsOf c = case c.value of
    IdentityColumn _ options -> options
    _ -> []


generatedOf :: ColumnDef -> Maybe (Storage, Text)
generatedOf c = case c.value of
    GeneratedColumn storage e -> Just (storage, e)
    _ -> Nothing


-- | Adding the column fails on a table with rows: it is not null and has no value to fill in.
failsOnRows :: ColumnDef -> Bool
failsOnRows c = isNotNull c && c.value == PlainColumn Nothing


-- | A collation, by its schema unless it is one of pg_catalog's.
data Collation = Collation
    { schema :: Maybe Identifier
    , name :: Identifier
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


collationSql :: Collation -> Text
collationSql c = maybe "" (\s -> quoted s <> ".") c.schema <> quoted c.name


instance FromJSON Collation where
    parseJSON v = case v of
        -- A name alone, as sqiff wrote it before it kept the schema.
        String n -> pure (Collation Nothing (identifier n))
        _ -> withObject "collation" (\o -> Collation <$> (fmap identifier <$> o .:? "schema") <*> (identifier <$> o .: "name")) v


-- | The column as text, one fact per line.
columnText :: ColumnDef -> Text
columnText c =
    T.intercalate "\n"
        $ ["type " <> typeText c.colType]
        <> ["collate " <> collationSql co | Just co <- [c.collation]]
        <> case c.nullability of
            Nullable -> []
            NotNull validity name ->
                ["not null" <> (if validity == NotValidated then " not valid" else "") <> maybe "" ((" constraint " <>) . nameText) name]
        <> case c.value of
            PlainColumn e -> ["default " <> x | Just x <- [e]]
            IdentityColumn kind options ->
                ["identity " <> identityKindSql kind] <> ["identity options " <> T.intercalate ", " options | not (null options)]
            GeneratedColumn storage e -> ["generated " <> storageSql storage <> " " <> e]
        <> ["rewrites the table when added" | c.rewritesOnAdd]


instance FromJSON ColumnDef where
    parseJSON = withObject "column" \o -> do
        colType <- typeName <$> o .: "type"
        collation <- o .:? "collation"
        notNull <- o .:? "not_null"
        nullability <- case notNull of
            Nothing -> pure Nullable
            Just n -> withObject "not null" (\nn -> NotNull <$> (bool NotValidated Validated <$> nn .: "validated") <*> (fmap identifier <$> nn .:? "name")) n
        identity <- o .:? "identity"
        generated <- o .:? "generated"
        expression <- o .:? "expression"
        value <- case (identity, generated) of
            (Just kind, Nothing) -> IdentityColumn <$> identityKind kind <*> o .:? "identity_options" .!= []
            (Nothing, Just storage) -> GeneratedColumn <$> storageKind storage <*> maybe (fail "a generated column without an expression") pure expression
            (Nothing, Nothing) -> pure (PlainColumn expression)
            (Just _, Just _) -> fail "a column cannot be both an identity and generated"
        rewritesOnAdd <- o .:? "rewrites_on_add" .!= False
        position <- Position <$> o .:? "position" .!= 0
        inheritedOnly <- o .:? "inherited" .!= False
        pure ColumnDef{colType, collation, nullability, value, rewritesOnAdd, position, inheritedOnly}
      where
        identityKind :: Text -> Parser IdentityKind
        identityKind = \case
            "always" -> pure Always
            "by default" -> pure ByDefault
            other -> fail ("unknown identity kind " <> T.unpack other)
        storageKind :: Text -> Parser Storage
        storageKind = \case
            "stored" -> pure Stored
            "virtual" -> pure Virtual
            other -> fail ("unknown generated column kind " <> T.unpack other)


-- | A table's settings; its columns and constraints are objects of their own.
data TableDef = TableDef
    { unlogged :: Bool
    , partitionBy :: Maybe Text
    -- ^ The partition key, as @pg_get_partkeydef@ prints it.
    , partitionOf :: Maybe PartitionOf
    , rowSecurity :: Bool
    , forceRowSecurity :: Bool
    , options :: [Text]
    -- ^ Storage options such as @fillfactor=70@; the TOAST table's with a @toast.@ prefix.
    , inherits :: [QName]
    -- ^ Parents by @inherits@, in order.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


newtype InheritedFrom = InheritedFrom QName


instance FromJSON InheritedFrom where
    parseJSON = withObject "parent" \p -> InheritedFrom <$> (qname <$> p .: "schema" <*> p .: "name")


data PartitionOf = PartitionOf
    { parent :: QName
    , bound :: Text
    -- ^ As @pg_get_expr@ prints it, such as @FOR VALUES FROM (0) TO (10)@.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | @with (a=1, b=2)@, or nothing for no options.
withOptions :: [Text] -> Maybe Text
withOptions = \case
    [] -> Nothing
    options -> Just ("with (" <> T.intercalate ", " options <> ")")


tableText :: TableDef -> Text
tableText t =
    T.intercalate "\n"
        $ ["unlogged" | t.unlogged]
        <> ["partition by " <> key | Just key <- [t.partitionBy]]
        <> ["partition of " <> qualified p.parent <> " " <> p.bound | Just p <- [t.partitionOf]]
        <> ["inherits " <> qualified p | p <- t.inherits]
        <> ["row level security" | t.rowSecurity]
        <> ["force row level security" | t.forceRowSecurity]
        <> maybeToList (withOptions t.options)


-- | A view or materialized view.
data ViewDef = ViewDef
    { options :: Maybe [Text]
    -- ^ Nothing when the view has no options at all.
    , query :: Text
    -- ^ As @pg_get_viewdef@ prints it.
    , columns :: ViewColumns
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | A view's columns, names and types in order. They follow from its query, so they are
-- not part of its definition: every list equals every other, as a view over a retyped
-- column is not itself changed. A recipe reads them with 'viewColumns'.
newtype ViewColumns = ViewColumns [(Identifier, TypeName)]
    deriving stock (Show)
    deriving newtype (NFData)


instance Eq ViewColumns where
    _ == _ = True


viewColumns :: ViewDef -> [(Identifier, TypeName)]
viewColumns v = let ViewColumns cs = v.columns in cs


-- | Whether @new@ keeps @old@'s columns as they are, adding any at the end, which is what
-- @create or replace view@ allows.
extendsColumns :: ViewDef -> ViewDef -> Bool
extendsColumns old new = viewColumns old `L.isPrefixOf` viewColumns new


viewText :: ViewDef -> Text
viewText v = T.intercalate "\n" (["with (" <> T.intercalate ", " o <> ")" | Just o <- [v.options]] <> [v.query])


data IndexDef = IndexDef
    { table :: QName
    , unique :: Bool
    , partitioned :: Bool
    -- ^ On a partitioned table, where it is created @ON ONLY@ the parent.
    , statement :: Text
    -- ^ As @pg_get_indexdef@ prints it.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


data ConstraintKind = Check | ForeignKey | PrimaryKey | Unique | Exclusion
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | A table's or domain's constraint.
data ConstraintDef = ConstraintDef
    { kind :: ConstraintKind
    , validity :: Validity
    , columns :: [Identifier]
    -- ^ The table's columns it covers, in order; none for a domain's.
    , references :: Maybe ForeignKeyTarget
    , clause :: Text
    -- ^ As @pg_get_constraintdef@ prints it, @NOT VALID@ included.
    , inheritedOnly :: Bool
    -- ^ The table has it only because a parent does.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


data ForeignKeyTarget = ForeignKeyTarget
    { table :: QName
    , columns :: [Identifier]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | The constraint as it is once validated.
withoutValidity :: ConstraintDef -> ConstraintDef
withoutValidity c = c{validity = Validated, clause = fromMaybe c.clause (T.stripSuffix " NOT VALID" c.clause)}


instance FromJSON TableDef where
    parseJSON = withObject "table" \o ->
        TableDef
            <$> o
            .: "unlogged"
            <*> o
            .:? "partition_by"
            <*> (o .:? "partition_of" >>= traverse (withObject "partition of" \p -> PartitionOf <$> (qname <$> p .: "schema" <*> p .: "name") <*> p .: "bound"))
            <*> o
            .: "row_security"
            <*> o
            .: "force_row_security"
            <*> o
            .:? "options"
            .!= []
            <*> (map (\(InheritedFrom q) -> q) <$> o .:? "inherits" .!= [])


instance FromJSON ViewDef where
    parseJSON = withObject "view" \o -> do
        columns <- o .:? "columns" .!= []
        ViewDef
            <$> o
            .:? "options"
            <*> o
            .: "query"
            <*> pure (ViewColumns [(identifier n, typeName t) | (n, t) <- columns])


instance FromJSON IndexDef where
    parseJSON = withObject "index" \o ->
        IndexDef
            <$> (qname <$> o .: "table_schema" <*> o .: "table_name")
            <*> o
            .: "unique"
            <*> o
            .: "partitioned"
            <*> o
            .: "statement"


instance FromJSON ConstraintDef where
    parseJSON = withObject "constraint" \o -> do
        kind <-
            o .: "kind" >>= \case
                "c" -> pure Check
                "f" -> pure ForeignKey
                "p" -> pure PrimaryKey
                "u" -> pure Unique
                "x" -> pure Exclusion
                other -> fail ("unknown constraint kind " <> T.unpack other)
        validity <- bool NotValidated Validated <$> o .: "validated"
        columns <- map identifier <$> o .:? "columns" .!= []
        references <- o .:? "references" >>= traverse (withObject "references" \r -> ForeignKeyTarget <$> (qname <$> r .: "schema" <*> r .: "table") <*> (map identifier <$> r .: "columns"))
        clause <- o .: "clause"
        inheritedOnly <- o .:? "inherited" .!= False
        pure ConstraintDef{kind, validity, columns, references, clause, inheritedOnly}


-- | A sequence that is not an identity column's.
data SequenceDef = SequenceDef
    { dataType :: TypeName
    , increment :: Integer
    , minValue :: Integer
    , maxValue :: Integer
    , start :: Integer
    , cache :: Integer
    , cycles :: Bool
    , ownedBy :: Maybe (QName, Identifier)
    -- ^ The column whose drop takes the sequence with it.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | The options @create sequence@ and @alter sequence@ take, all of them spelled out.
sequenceOptions :: SequenceDef -> Text
sequenceOptions q =
    T.unwords
        [ "as " <> typeText q.dataType
        , "increment by " <> tshow q.increment
        , "minvalue " <> tshow q.minValue
        , "maxvalue " <> tshow q.maxValue
        , "start with " <> tshow q.start
        , "cache " <> tshow q.cache
        , if q.cycles then "cycle" else "no cycle"
        ]


sequenceText :: SequenceDef -> Text
sequenceText q = sequenceOptions q <> foldMap (\(t, c) -> " owned by " <> qualified t <> "." <> quoted c) q.ownedBy


data ExtensionDef = ExtensionDef
    { version :: Text
    , schema :: Identifier
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | A user-defined type; a domain's constraints are objects of their own.
data TypeDef
    = -- | The labels, in order.
      EnumType [Text]
    | -- | The base type, whether it is not null, and its default.
      DomainType TypeName Bool (Maybe Text)
    | -- | The attributes and their types.
      CompositeType [(Identifier, TypeName)]
    | -- | The subtype.
      RangeType TypeName
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


typeDefText :: TypeDef -> Text
typeDefText = \case
    EnumType labels -> "enum (" <> T.intercalate ", " (map literal labels) <> ")"
    DomainType base notNull dflt -> T.unwords (["domain " <> typeText base] <> ["not null" | notNull] <> ["default " <> d | Just d <- [dflt]])
    CompositeType attributes -> "composite (" <> T.intercalate ", " [quoted a <> " " <> typeText t | (a, t) <- attributes] <> ")"
    RangeType subtype -> "range (subtype = " <> typeText subtype <> ")"


instance FromJSON SequenceDef where
    parseJSON = withObject "sequence" \o ->
        SequenceDef
            <$> (typeName <$> o .: "type")
            <*> o
            .: "increment"
            <*> o
            .: "min"
            <*> o
            .: "max"
            <*> o
            .: "start"
            <*> o
            .: "cache"
            <*> o
            .: "cycle"
            <*> (o .:? "owned_by" >>= traverse (withObject "owner" \w -> (,) <$> (qname <$> w .: "schema" <*> w .: "table") <*> (identifier <$> w .: "column")))


instance FromJSON ExtensionDef where
    parseJSON = withObject "extension" \o -> ExtensionDef <$> o .: "version" <*> (identifier <$> o .: "schema")


instance FromJSON TypeDef where
    parseJSON = withObject "type" \o ->
        o .: "kind" >>= \case
            "enum" -> EnumType <$> o .:? "labels" .!= []
            "domain" -> DomainType <$> (typeName <$> o .: "base") <*> o .: "not_null" <*> o .:? "default"
            "composite" -> CompositeType . map (\(a, t) -> (identifier a, typeName t)) <$> o .:? "attributes" .!= []
            "range" -> RangeType . typeName <$> o .: "subtype"
            other -> fail ("unknown kind of type " <> T.unpack other)


-- | An object's privileges.
data Acl
    = -- | No ACL: the defaults for the kind of object and its owner.
      DefaultAcl
    | -- | These grants exactly, the owner's own included.
      ExplicitAcl (Set Grant)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


data Grant = Grant
    { grantee :: Grantee
    , privilege :: Text
    -- ^ As @aclexplode@ names it, such as @SELECT@.
    , grantable :: Bool
    , grantor :: Grantor
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


-- | Who gave a grant: the object's owner, whoever that is in each database, or another
-- role, which passes on a privilege it holds with the grant option.
data Grantor = ByOwner | ByRole Identifier
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


data Grantee = Public | Role Identifier
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


granteeSql :: Grantee -> Text
granteeSql = \case
    Public -> "public"
    Role r -> quoted r


-- | One line per grant, in order: @role PRIVILEGE[ with grant option][ granted by role]@.
aclText :: Acl -> Text
aclText = \case
    DefaultAcl -> "default"
    ExplicitAcl grants -> T.intercalate "\n" (L.sort (map line (Set.toList grants)))
  where
    line g =
        granteeText g.grantee
            <> " "
            <> g.privilege
            <> (if g.grantable then " with grant option" else "")
            <> case g.grantor of
                ByOwner -> ""
                ByRole r -> " granted by " <> nameText r
    granteeText = \case
        Public -> "public"
        Role r -> nameText r


-- | @null@ for the defaults, else the list of grants.
instance FromJSON Acl where
    parseJSON v
        | v == Null = pure DefaultAcl
        | otherwise = ExplicitAcl . Set.fromList <$> parseJSON v


instance FromJSON Grant where
    parseJSON = withObject "grant" \o ->
        Grant
            <$> (maybe Public (Role . identifier) <$> o .:? "grantee")
            <*> o
            .: "privilege"
            <*> o
            .: "grantable"
            <*> (maybe ByOwner (ByRole . identifier) <$> o .:? "grantor")
