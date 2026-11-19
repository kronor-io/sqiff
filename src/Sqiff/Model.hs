{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What a schema is made of: objects named one way per kind, the aspects sqiff compares,
-- and the dependencies between objects.
--
-- The catalog names an object by @pg_identify_object_as_address@: a kind as text and two
-- lists of names. That 'Address' is parsed once, where the catalog is read, into an
-- 'ObjectId'; everything after works with the parsed form, and only renders it back to an
-- 'Address' for messages, file names and catalog lookups.
module Sqiff.Model (
    -- * Objects
    ObjectId (..),
    RoutineKind (..),
    ColumnOf (..),
    owningTable,
    mapQNames,
    isRelation,
    isView,
    isRoutine,

    -- * Catalog addresses
    Address (..),
    fromAddress,
    toAddress,
    render,

    -- * Schemas
    Aspect (..),
    ObjectKey (..),
    definitionKey,
    Schema,
    Edge (..),
) where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import Sqiff.Definition (Definition)
import Sqiff.Identifier


data RoutineKind = Function | Procedure | Aggregate
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


-- | What a column other than a table's belongs to; such columns only carry comments.
data ColumnOf = OfView | OfMaterializedView | OfCompositeType | OfForeignTable
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


-- | One object of the schema, named the way its kind is named.
data ObjectId
    = Schema Identifier
    | Extension Identifier
    | Table QName
    | View QName
    | MaterializedView QName
    | -- | A table's column.
      Column QName Identifier
    | -- | A column of a view, materialized view, composite type or foreign table.
      OtherColumn ColumnOf QName Identifier
    | TableConstraint QName Identifier
    | DomainConstraint TypeName Identifier
    | Index QName
    | Sequence QName
    | Type TypeName
    | Routine RoutineKind QName [TypeName]
    | Trigger QName Identifier
    | Policy QName Identifier
    | -- | A kind sqiff does not model, as the catalog addressed it. Only 'fromAddress'
      -- makes one, so it never holds an address one of the other constructors covers.
      Unknown Address
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


-- | The table a column, constraint, trigger or policy belongs to. Indexes name their
-- table only in their definition.
owningTable :: ObjectId -> Maybe QName
owningTable = \case
    Column t _ -> Just t
    TableConstraint t _ -> Just t
    Trigger t _ -> Just t
    Policy t _ -> Just t
    _ -> Nothing


-- | The object with each schema-qualified name in its identity changed by @f@, such as
-- the table a column belongs to. Type names are SQL text and stay as they are.
mapQNames :: (QName -> QName) -> ObjectId -> ObjectId
mapQNames f = \case
    Table q -> Table (f q)
    View q -> View (f q)
    MaterializedView q -> MaterializedView (f q)
    Column t c -> Column (f t) c
    OtherColumn of_ t c -> OtherColumn of_ (f t) c
    TableConstraint t c -> TableConstraint (f t) c
    Index q -> Index (f q)
    Sequence q -> Sequence (f q)
    Routine kind q args -> Routine kind (f q) args
    Trigger t n -> Trigger (f t) n
    Policy t n -> Policy (f t) n
    other@(Schema _) -> other
    other@(Extension _) -> other
    other@(DomainConstraint _ _) -> other
    other@(Type _) -> other
    other@(Unknown _) -> other


-- | A table, view or materialized view, with its name.
isRelation :: ObjectId -> Maybe QName
isRelation = \case
    Table q -> Just q
    View q -> Just q
    MaterializedView q -> Just q
    _ -> Nothing


isView :: ObjectId -> Bool
isView = \case
    View _ -> True
    MaterializedView _ -> True
    _ -> False


-- | Functions and procedures, whose bodies can use other objects.
isRoutine :: ObjectId -> Bool
isRoutine = \case
    Routine Function _ _ -> True
    Routine Procedure _ _ -> True
    _ -> False


-- | How @pg_identify_object_as_address@ names an object.
data Address = Address
    { objectType :: Text
    , objectNames :: [Text]
    , objectArgs :: [Text]
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


fromAddress :: Address -> ObjectId
fromAddress addr = case (addr.objectType, addr.objectNames, addr.objectArgs) of
    ("schema", [s], []) -> Schema (identifier s)
    ("extension", [e], []) -> Extension (identifier e)
    ("table", [s, n], []) -> Table (qname s n)
    ("view", [s, n], []) -> View (qname s n)
    ("materialized view", [s, n], []) -> MaterializedView (qname s n)
    ("table column", [s, t, c], []) -> Column (qname s t) (identifier c)
    ("view column", [s, t, c], []) -> OtherColumn OfView (qname s t) (identifier c)
    ("materialized view column", [s, t, c], []) -> OtherColumn OfMaterializedView (qname s t) (identifier c)
    ("composite type column", [s, t, c], []) -> OtherColumn OfCompositeType (qname s t) (identifier c)
    ("foreign table column", [s, t, c], []) -> OtherColumn OfForeignTable (qname s t) (identifier c)
    ("table constraint", [s, t, c], []) -> TableConstraint (qname s t) (identifier c)
    ("domain constraint", [d], [c]) -> DomainConstraint (typeName d) (identifier c)
    ("index", [s, n], []) -> Index (qname s n)
    ("sequence", [s, n], []) -> Sequence (qname s n)
    ("type", [t], []) -> Type (typeName t)
    ("function", [s, n], args) -> Routine Function (qname s n) (map typeName args)
    ("procedure", [s, n], args) -> Routine Procedure (qname s n) (map typeName args)
    ("aggregate", [s, n], args) -> Routine Aggregate (qname s n) (map typeName args)
    ("trigger", [s, t, n], []) -> Trigger (qname s t) (identifier n)
    ("policy", [s, t, n], []) -> Policy (qname s t) (identifier n)
    _ -> Unknown addr


-- | The address 'fromAddress' parsed this object from.
toAddress :: ObjectId -> Address
toAddress = \case
    Schema s -> Address "schema" [nameText s] []
    Extension e -> Address "extension" [nameText e] []
    Table q -> Address "table" (names q) []
    View q -> Address "view" (names q) []
    MaterializedView q -> Address "materialized view" (names q) []
    Column t c -> Address "table column" (names t <> [nameText c]) []
    OtherColumn of_ t c -> Address (columnKind of_) (names t <> [nameText c]) []
    TableConstraint t c -> Address "table constraint" (names t <> [nameText c]) []
    DomainConstraint d c -> Address "domain constraint" [typeText d] [nameText c]
    Index q -> Address "index" (names q) []
    Sequence q -> Address "sequence" (names q) []
    Type t -> Address "type" [typeText t] []
    Routine kind q args -> Address (routineKind kind) (names q) (map typeText args)
    Trigger t n -> Address "trigger" (names t <> [nameText n]) []
    Policy t n -> Address "policy" (names t <> [nameText n]) []
    Unknown addr -> addr
  where
    names q = [nameText q.schema, nameText q.name]
    columnKind = \case
        OfView -> "view column"
        OfMaterializedView -> "materialized view column"
        OfCompositeType -> "composite type column"
        OfForeignTable -> "foreign table column"
    routineKind = \case
        Function -> "function"
        Procedure -> "procedure"
        Aggregate -> "aggregate"


-- | The object as messages name it: its kind, dotted names and arguments.
render :: ObjectId -> Text
render object = [i|#{addr.objectType} #{names}#{args}|]
  where
    addr = toAddress object
    names = T.intercalate "." addr.objectNames
    args :: Text
    args
        | null addr.objectArgs = ""
        | otherwise = let list = T.intercalate ", " addr.objectArgs in [i|(#{list})|]


-- | The parts of an object sqiff compares apart.
data Aspect = Definition | Privileges | Comment
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


data ObjectKey = ObjectKey
    { aspect :: Aspect
    , object :: ObjectId
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


definitionKey :: ObjectId -> ObjectKey
definitionKey = ObjectKey Definition


-- | Each object's aspects, as the catalog describes them.
type Schema = Map ObjectKey Definition


-- | @dependent@ needs @referenced@ to exist, as recorded in @pg_depend@ or seen in a
-- PL/pgSQL body by plpgsql_check.
data Edge = Edge
    { dependent :: ObjectId
    , referenced :: ObjectId
    }
    deriving stock (Eq, Ord, Show)
