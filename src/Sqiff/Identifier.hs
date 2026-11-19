{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Names as PostgreSQL stores them, kept apart from SQL text: a name reaches SQL only
-- through 'quoted' or 'qualified', so it cannot be spliced in unquoted by mistake.
module Sqiff.Identifier (
    Identifier,
    identifier,
    nameText,
    quoted,
    QName (..),
    qname,
    qualified,
    dotted,
    TypeName,
    typeName,
    typeText,
    quoteIdent,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Conversion (InterpSink, Interpolatable (..))
import Data.Text qualified as T
import GHC.TypeLits (ErrorMessage (..), TypeError)
import RIO
import RIO.Set qualified as Set


-- | One name as the catalog stores it: unquoted, case and all.
newtype Identifier = Identifier Text
    deriving newtype (Eq, Ord, NFData)


-- | Shows the name as SQL would quote it, which keeps test failures readable.
instance Show Identifier where
    show = T.unpack . quoted


-- | Any text is a name: PostgreSQL quotes what it cannot leave bare.
identifier :: Text -> Identifier
identifier = Identifier


-- | The name itself, for messages, file names and change names; never for SQL.
nameText :: Identifier -> Text
nameText (Identifier t) = t


-- | The name as SQL writes it.
quoted :: Identifier -> Text
quoted (Identifier t) = quoteIdent t


-- | Interpolating a name into SQL would splice it in unquoted: say how to render it.
instance {-# OVERLAPPING #-} (TypeError ('Text "Interpolate an Identifier with quoted (SQL) or nameText (messages, paths)"), InterpSink flag dst) => Interpolatable flag Identifier dst where
    interpolate = error "unreachable"


instance {-# OVERLAPPING #-} (TypeError ('Text "Interpolate a QName with qualified (SQL) or dotted (messages, paths)"), InterpSink flag dst) => Interpolatable flag QName dst where
    interpolate = error "unreachable"


instance {-# OVERLAPPING #-} (TypeError ('Text "Interpolate a TypeName with typeText"), InterpSink flag dst) => Interpolatable flag TypeName dst where
    interpolate = error "unreachable"


-- | A name in a schema.
data QName = QName
    { schema :: Identifier
    , name :: Identifier
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


qname :: Text -> Text -> QName
qname s n = QName (identifier s) (identifier n)


-- | @schema.name@, each part quoted as needed.
qualified :: QName -> Text
qualified (QName s n) = [i|#{quoted s}.#{quoted n}|]


-- | @schema.name@ unquoted, as pg_identify_object_as_address and messages give it.
dotted :: QName -> Text
dotted (QName s n) = [i|#{nameText s}.#{nameText n}|]


-- | A type as @format_type@ prints it: already SQL, qualified and quoted as needed.
newtype TypeName = TypeName Text
    deriving newtype (Eq, Ord, Show, NFData)


typeName :: Text -> TypeName
typeName = TypeName


-- | The type's SQL text.
typeText :: TypeName -> Text
typeText (TypeName t) = t


-- | Like PostgreSQL's @quote_ident@: quotes unless the name is lower case and not reserved.
quoteIdent :: Text -> Text
quoteIdent t
    | simple && not (t `Set.member` reserved) = t
    | otherwise = [i|"#{T.replace "\"" "\"\"" t}"|]
  where
    simple = case T.uncons t of
        Just (c, rest) -> (isAsciiLower c || c == '_') && T.all (\x -> isAsciiLower x || isDigit x || x == '_') rest
        Nothing -> False


-- | The words @pg_get_keywords()@ lists with a category other than unreserved (PostgreSQL 18).
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
