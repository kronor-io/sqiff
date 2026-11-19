{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Byte-for-byte tests of rewriting a table's deploy file: the text it keeps, where it
-- puts what it adds, what it removes with a statement, and the differences it leaves.
module Rewriting (tests) where

import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.Definition
import Sqiff.Identifier (QName, identifier, qname, typeName)
import Sqiff.Model (Aspect (..), ObjectId (..), ObjectKey (..), Schema, definitionKey)
import Sqiff.Rewrite (Rewrite (..), rewriteFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))


tests :: TestTree
tests =
    testGroup
        "rewriting a deploy file"
        [ testCase "an unchanged file comes back as it was" unchanged
        , testCase "column, constraint, index and comment edits together" mixed
        , testCase "a removed statement or column takes its own comments with it" removals
        , testCase "new indexes and comments go after the table when the file keeps none" anchoredOnTable
        , testCase "new objects stay unplaced when the file does not create the table" noTable
        , testCase "table settings become alter statements and a with clause" settings
        ]


-- | The table every test rewrites.
widgets :: QName
widgets = qname "app" "widgets"


-- | Rewrites @file@, which creates @fromFile@, to create @live@: the new text, or the
-- error in its place, and the differences left unplaced.
rewrite :: Schema -> Schema -> Text -> (Text, [Text])
rewrite fromFile live file = case rewriteFile widgets Set.empty liveOrder live fromFile file of
    Left e -> ("error: " <> e, [])
    Right r -> (r.text, r.unplaced)
  where
    liveOrder = [c | k <- Map.keys live, k.aspect == Definition, Column _ c <- [k.object]]


-- | Asserts the rewrite's text line by line, so a failure points at the line.
rewritesTo :: (Text, [Text]) -> (Text, [Text]) -> Assertion
rewritesTo (actual, actualUnplaced) (expected, expectedUnplaced) = do
    T.lines actual @?= T.lines expected
    actual @?= expected
    actualUnplaced @?= expectedUnplaced


columnRow :: Text -> Text -> Nullability -> (ObjectKey, Definition)
columnRow c ty nullability =
    ( definitionKey (Column widgets (identifier c))
    , ColumnDefinition
        ColumnDef
            { colType = typeName ty
            , collation = Nothing
            , nullability
            , value = PlainColumn Nothing
            , rewritesOnAdd = False
            , position = Position 0
            , inheritedOnly = False
            }
    )


tableRow :: TableDef -> (ObjectKey, Definition)
tableRow def = (definitionKey (Table widgets), TableDefinition def)


plainTable :: TableDef
plainTable =
    TableDef
        { unlogged = False
        , partitionBy = Nothing
        , partitionOf = Nothing
        , rowSecurity = False
        , forceRowSecurity = False
        , options = []
        , inherits = []
        }


checkRow :: Text -> [Text] -> Text -> (ObjectKey, Definition)
checkRow n cs clause =
    ( definitionKey (TableConstraint widgets (identifier n))
    , ConstraintDefinition ConstraintDef{kind = Check, validity = Validated, columns = map identifier cs, references = Nothing, clause, inheritedOnly = False}
    )


indexRow :: Text -> Text -> (ObjectKey, Definition)
indexRow n statement =
    ( definitionKey (Index (qname "app" n))
    , IndexDefinition IndexDef{table = widgets, unique = False, partitioned = False, statement}
    )


commentRow :: ObjectId -> Text -> (ObjectKey, Definition)
commentRow o t = (ObjectKey Comment o, Printed t)


notNull :: Nullability
notNull = NotNull Validated Nothing


unchanged :: Assertion
unchanged = do
    let file =
            T.unlines
                [ "-- Deploy app:widgets to pg"
                , ""
                , "begin;"
                , ""
                , "create table app.widgets ("
                , "    id bigint not null,"
                , "    -- what it is called"
                , "    name text, -- shown to people"
                , "    qty integer,"
                , "    constraint widgets_qty_check check (qty > 0)"
                , ");"
                , ""
                , "create index widgets_name_idx"
                , "    on app.widgets (name);"
                , ""
                , "comment on table app.widgets is 'Things';"
                , ""
                , "commit;"
                ]
        schema =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , columnRow "name" "text" Nullable
                , columnRow "qty" "integer" Nullable
                , checkRow "widgets_qty_check" ["qty"] "CHECK ((qty > 0))"
                , indexRow "widgets_name_idx" "CREATE INDEX widgets_name_idx ON app.widgets USING btree (name)"
                , commentRow (Table widgets) "Things"
                ]
    rewrite schema schema file `rewritesTo` (file, [])


mixed :: Assertion
mixed = do
    let file =
            T.unlines
                [ "-- Deploy app:widgets to pg"
                , ""
                , "begin;"
                , ""
                , "create table app.widgets ("
                , "    id bigint not null,"
                , "    -- the old name"
                , "    label text,"
                , "    qty integer, -- how many"
                , "    constraint widgets_qty_check check (qty > 0)"
                , ");"
                , ""
                , "create index widgets_label_idx"
                , "    on app.widgets (label);"
                , ""
                , "create index widgets_qty_idx on app.widgets (qty);"
                , ""
                , "comment on table app.widgets is 'Things';"
                , ""
                , "comment on column app.widgets.label is 'Old label';"
                , ""
                , "commit;"
                ]
        fromFile =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , columnRow "label" "text" Nullable
                , columnRow "qty" "integer" Nullable
                , checkRow "widgets_qty_check" ["qty"] "CHECK ((qty > 0))"
                , indexRow "widgets_label_idx" "CREATE INDEX widgets_label_idx ON app.widgets USING btree (label)"
                , indexRow "widgets_qty_idx" "CREATE INDEX widgets_qty_idx ON app.widgets USING btree (qty)"
                , commentRow (Table widgets) "Things"
                , commentRow (Column widgets (identifier "label")) "Old label"
                ]
        live =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , columnRow "qty" "bigint" Nullable
                , columnRow "title" "text" notNull
                , checkRow "widgets_qty_check" ["qty"] "CHECK ((qty >= 0))"
                , indexRow "widgets_qty_idx" "CREATE INDEX widgets_qty_idx ON app.widgets USING btree (qty DESC)"
                , indexRow "widgets_title_idx" "CREATE UNIQUE INDEX widgets_title_idx ON app.widgets USING btree (title)"
                , commentRow (Table widgets) "Widgets, it's said"
                , commentRow (Column widgets (identifier "title")) "What it is called"
                ]
    rewrite fromFile live file
        `rewritesTo` ( T.unlines
                        [ "-- Deploy app:widgets to pg"
                        , ""
                        , "begin;"
                        , ""
                        , "create table app.widgets ("
                        , "    id bigint not null,"
                        , "    qty bigint, -- how many"
                        , "    title text not null,"
                        , "    constraint widgets_qty_check check ((qty >= 0))"
                        , ");"
                        , ""
                        , "create index if not exists widgets_qty_idx"
                        , "    on app.widgets (qty DESC);"
                        , ""
                        , "create unique index if not exists widgets_title_idx"
                        , "    on app.widgets (title);"
                        , ""
                        , "comment on table app.widgets is 'Widgets, it''s said';"
                        , ""
                        , "comment on column app.widgets.title is"
                        , "    'What it is called';"
                        , ""
                        , "commit;"
                        ]
                     , []
                     )


removals :: Assertion
removals = do
    let file =
            T.unlines
                [ "begin;"
                , ""
                , "create table app.widgets ("
                , "    id bigint not null,"
                , ""
                , "    -- Kept for old clients."
                , "    -- Remove after the migration."
                , "    legacy text,"
                , "    -- Details"
                , ""
                , "    notes text,"
                , "    name text"
                , ");"
                , ""
                , "-- Lookups by name."
                , "create index widgets_name_idx on app.widgets (name);"
                , ""
                , "-- The legacy column's comment."
                , "comment on column app.widgets.legacy is 'Old';"
                , "comment on column app.widgets.name is 'Name';"
                , ""
                , "commit;"
                ]
        fromFile =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , columnRow "legacy" "text" Nullable
                , columnRow "notes" "text" Nullable
                , columnRow "name" "text" Nullable
                , indexRow "widgets_name_idx" "CREATE INDEX widgets_name_idx ON app.widgets USING btree (name)"
                , commentRow (Column widgets (identifier "legacy")) "Old"
                , commentRow (Column widgets (identifier "name")) "Name"
                ]
        live =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , columnRow "name" "text" Nullable
                , commentRow (Column widgets (identifier "name")) "Name"
                ]
    rewrite fromFile live file
        `rewritesTo` ( T.unlines
                        [ "begin;"
                        , ""
                        , "create table app.widgets ("
                        , "    id bigint not null,"
                        , "    -- Details"
                        , ""
                        , "    name text"
                        , ");"
                        , ""
                        , "comment on column app.widgets.name is 'Name';"
                        , ""
                        , "commit;"
                        ]
                     , []
                     )


anchoredOnTable :: Assertion
anchoredOnTable = do
    let file =
            T.unlines
                [ "begin;"
                , ""
                , "create table app.widgets ("
                , "    id bigint not null"
                , ");"
                , ""
                , "create index widgets_old_idx on app.widgets (id);"
                , ""
                , "commit;"
                ]
        fromFile =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , indexRow "widgets_old_idx" "CREATE INDEX widgets_old_idx ON app.widgets USING btree (id)"
                ]
        live =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , indexRow "widgets_id_idx" "CREATE INDEX widgets_id_idx ON app.widgets USING hash (id)"
                , commentRow (Table widgets) "Things"
                , commentRow (Column widgets (identifier "id")) "Line one\nline two"
                ]
    rewrite fromFile live file
        `rewritesTo` ( T.unlines
                        [ "begin;"
                        , ""
                        , "create table app.widgets ("
                        , "    id bigint not null"
                        , ");"
                        , ""
                        , "create index if not exists widgets_id_idx"
                        , "    on app.widgets using hash (id);"
                        , ""
                        , "comment on table app.widgets is"
                        , "    'Things';"
                        , ""
                        , "comment on column app.widgets.id is"
                        , "    $comment$Line one"
                        , "line two$comment$;"
                        , ""
                        , "commit;"
                        ]
                     , []
                     )


noTable :: Assertion
noTable = do
    let file =
            T.unlines
                [ "begin;"
                , ""
                , "select 1;"
                , ""
                , "commit;"
                ]
        fromFile =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                ]
        live =
            Map.fromList
                [ tableRow plainTable{rowSecurity = True}
                , columnRow "id" "integer" notNull
                , indexRow "widgets_id_idx" "CREATE INDEX widgets_id_idx ON app.widgets USING btree (id)"
                , commentRow (Table widgets) "Things"
                ]
    rewrite fromFile live file
        `rewritesTo` ( file
                     ,
                         [ "table app.widgets: differs from the database"
                         , "table column app.widgets.id: differs from the database"
                         , "index app.widgets_id_idx: in the database, not in the file"
                         , "table app.widgets (comment): in the database, not in the file"
                         ]
                     )


settings :: Assertion
settings = do
    let file =
            T.unlines
                [ "begin;"
                , ""
                , "create table app.widgets ("
                , "    id bigint not null"
                , ");"
                , ""
                , "create index widgets_id_idx on app.widgets (id);"
                , ""
                , "commit;"
                ]
        fromFile =
            Map.fromList
                [ tableRow plainTable
                , columnRow "id" "bigint" notNull
                , indexRow "widgets_id_idx" "CREATE INDEX widgets_id_idx ON app.widgets USING btree (id)"
                ]
        live =
            Map.fromList
                [ tableRow plainTable{rowSecurity = True, forceRowSecurity = True, options = ["fillfactor=70"]}
                , columnRow "id" "bigint" notNull
                , indexRow "widgets_id_idx" "CREATE INDEX widgets_id_idx ON app.widgets USING btree (id)"
                ]
    rewrite fromFile live file
        `rewritesTo` ( T.unlines
                        [ "begin;"
                        , ""
                        , "create table app.widgets ("
                        , "    id bigint not null"
                        , ") with (fillfactor=70);"
                        , ""
                        , "alter table app.widgets enable row level security;"
                        , ""
                        , "alter table app.widgets force row level security;"
                        , ""
                        , "create index widgets_id_idx on app.widgets (id);"
                        , ""
                        , "commit;"
                        ]
                     , []
                     )
