{-# LANGUAGE DeriveAnyClass #-}
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
    asColumn,
    asAcl,

    -- * Columns
    ColumnDef (..),
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

    -- * Privileges
    Acl (..),
    Grant (..),
    Grantee (..),
    aclText,
    granteeSql,
) where

import Data.Aeson (FromJSON (..), Value (Null), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Set qualified as Set
import Sqiff.Identifier


-- | One aspect of one object.
data Definition
    = ColumnDefinition ColumnDef
    | AclDefinition Acl
    | -- | As the catalog prints it: a @create@ statement, an expression, a comment.
      Printed Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


-- | The definition as text, in the form @sqiff describe@ writes.
printed :: Definition -> Text
printed = \case
    ColumnDefinition c -> columnText c
    AclDefinition acl -> aclText acl
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
    Printed t -> Printed (f t)


asColumn :: Definition -> Maybe ColumnDef
asColumn = \case
    ColumnDefinition c -> Just c
    _ -> Nothing


asAcl :: Definition -> Maybe Acl
asAcl = \case
    AclDefinition acl -> Just acl
    _ -> Nothing


-- | A table column, as @alter table ... add column@ would declare it.
data ColumnDef = ColumnDef
    { colType :: TypeName
    , collation :: Maybe Identifier
    -- ^ Only when it is not the type's own.
    , nullability :: Nullability
    , value :: ColumnValue
    , rewritesOnAdd :: Bool
    -- ^ Adding the column rewrites the table: its default is volatile, or its type a
    -- domain with constraints.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


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


-- | The column as text, one fact per line.
columnText :: ColumnDef -> Text
columnText c =
    T.intercalate "\n"
        $ ["type " <> typeText c.colType]
        <> ["collate " <> quoted co | Just co <- [c.collation]]
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
        collation <- fmap identifier <$> o .:? "collation"
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
        pure ColumnDef{colType, collation, nullability, value, rewritesOnAdd}
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
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


data Grantee = Public | Role Identifier
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


granteeSql :: Grantee -> Text
granteeSql = \case
    Public -> "public"
    Role r -> quoted r


-- | One line per grant, in order: @role PRIVILEGE[ with grant option]@.
aclText :: Acl -> Text
aclText = \case
    DefaultAcl -> "default"
    ExplicitAcl grants -> T.intercalate "\n" (L.sort (map line (Set.toList grants)))
  where
    line g = granteeText g.grantee <> " " <> g.privilege <> (if g.grantable then " with grant option" else "")
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
