{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Catalog addresses as one constructor per object kind, and the SQL names to use for them.
module Kronor.SchemaDiff.Object (
    QName (..),
    RoutineKind (..),
    Object (..),
    fromAddress,
    owningTable,
    quoteIdent,
    qualified,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.Text qualified as T
import Kronor.SchemaDiff.Catalog (Address (..))
import RIO
import RIO.Set qualified as Set


data QName = QName
    { schema :: Text
    , name :: Text
    }
    deriving stock (Eq, Ord, Show)


data RoutineKind = Function | Procedure | Aggregate
    deriving stock (Eq, Ord, Show)


data Object
    = Schema Text
    | Extension Text
    | Table QName
    | View QName
    | MaterializedView QName
    | Column QName Text
    | TableConstraint QName Text
    | -- | The domain as @format_type@ prints it, and the constraint name.
      DomainConstraint Text Text
    | Index QName
    | Sequence QName
    | -- | As @format_type@ prints it: qualified and quoted.
      Type Text
    | Routine RoutineKind QName [Text]
    | Trigger QName Text
    | Policy QName Text
    | Unknown Address
    deriving stock (Eq, Ord, Show)


fromAddress :: Address -> Object
fromAddress addr = case (addr.objectType, addr.objectNames, addr.objectArgs) of
    ("schema", [s], _) -> Schema s
    ("extension", [e], _) -> Extension e
    ("table", [s, n], _) -> Table (QName s n)
    ("view", [s, n], _) -> View (QName s n)
    ("materialized view", [s, n], _) -> MaterializedView (QName s n)
    ("table column", [s, t, c], _) -> Column (QName s t) c
    ("table constraint", [s, t, c], _) -> TableConstraint (QName s t) c
    ("domain constraint", [d], [c]) -> DomainConstraint d c
    ("index", [s, n], _) -> Index (QName s n)
    ("sequence", [s, n], _) -> Sequence (QName s n)
    ("type", [t], _) -> Type t
    ("function", [s, n], args) -> Routine Function (QName s n) args
    ("procedure", [s, n], args) -> Routine Procedure (QName s n) args
    ("aggregate", [s, n], args) -> Routine Aggregate (QName s n) args
    ("trigger", [s, t, n], _) -> Trigger (QName s t) n
    ("policy", [s, t, n], _) -> Policy (QName s t) n
    _ -> Unknown addr


-- | The table a column, constraint, trigger or policy belongs to. Indexes name their
-- table only in their definition.
owningTable :: Object -> Maybe QName
owningTable = \case
    Column t _ -> Just t
    TableConstraint t _ -> Just t
    Trigger t _ -> Just t
    Policy t _ -> Just t
    _ -> Nothing


qualified :: QName -> Text
qualified q = quoteIdent q.schema <> "." <> quoteIdent q.name


-- | Like PostgreSQL's @quote_ident@: quotes unless the name is lower case and not reserved.
quoteIdent :: Text -> Text
quoteIdent t
    | simple && not (t `Set.member` reserved) = t
    | otherwise = "\"" <> T.replace "\"" "\"\"" t <> "\""
  where
    simple = case T.uncons t of
        Just (c, rest) -> (isAsciiLower c || c == '_') && T.all (\x -> isAsciiLower x || isDigit x || x == '_' || x == '$') rest
        Nothing -> False


-- | PostgreSQL's reserved and type/function-name keywords.
reserved :: Set Text
reserved =
    Set.fromList
        [ "all"
        , "analyse"
        , "analyze"
        , "and"
        , "any"
        , "array"
        , "as"
        , "asc"
        , "asymmetric"
        , "authorization"
        , "between"
        , "bigint"
        , "binary"
        , "bit"
        , "boolean"
        , "both"
        , "case"
        , "cast"
        , "char"
        , "character"
        , "check"
        , "coalesce"
        , "collate"
        , "collation"
        , "column"
        , "concurrently"
        , "constraint"
        , "create"
        , "cross"
        , "current_catalog"
        , "current_date"
        , "current_role"
        , "current_schema"
        , "current_time"
        , "current_timestamp"
        , "current_user"
        , "dec"
        , "decimal"
        , "default"
        , "deferrable"
        , "desc"
        , "distinct"
        , "do"
        , "else"
        , "end"
        , "except"
        , "exists"
        , "extract"
        , "false"
        , "fetch"
        , "float"
        , "for"
        , "foreign"
        , "freeze"
        , "from"
        , "full"
        , "grant"
        , "greatest"
        , "group"
        , "grouping"
        , "having"
        , "ilike"
        , "in"
        , "initially"
        , "inner"
        , "inout"
        , "int"
        , "integer"
        , "intersect"
        , "interval"
        , "into"
        , "is"
        , "isnull"
        , "join"
        , "json"
        , "json_array"
        , "json_arrayagg"
        , "json_exists"
        , "json_object"
        , "json_objectagg"
        , "json_query"
        , "json_scalar"
        , "json_serialize"
        , "json_table"
        , "json_value"
        , "lateral"
        , "leading"
        , "least"
        , "left"
        , "like"
        , "limit"
        , "localtime"
        , "localtimestamp"
        , "merge_action"
        , "national"
        , "natural"
        , "nchar"
        , "none"
        , "normalize"
        , "not"
        , "notnull"
        , "null"
        , "nullif"
        , "numeric"
        , "offset"
        , "on"
        , "only"
        , "or"
        , "order"
        , "out"
        , "outer"
        , "overlaps"
        , "overlay"
        , "placing"
        , "position"
        , "precision"
        , "primary"
        , "real"
        , "references"
        , "returning"
        , "right"
        , "row"
        , "select"
        , "session_user"
        , "setof"
        , "similar"
        , "smallint"
        , "some"
        , "substring"
        , "symmetric"
        , "system_user"
        , "table"
        , "tablesample"
        , "then"
        , "time"
        , "timestamp"
        , "to"
        , "trailing"
        , "treat"
        , "trim"
        , "true"
        , "union"
        , "unique"
        , "user"
        , "using"
        , "values"
        , "varchar"
        , "variadic"
        , "verbose"
        , "when"
        , "where"
        , "window"
        , "with"
        , "xmlattributes"
        , "xmlconcat"
        , "xmlelement"
        , "xmlexists"
        , "xmlforest"
        , "xmlnamespaces"
        , "xmlparse"
        , "xmlpi"
        , "xmlroot"
        , "xmlserialize"
        , "xmltable"
        ]
