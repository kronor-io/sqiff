{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Tests of the pure parts: quoting, name templates, script splitting, plan parsing,
-- ordering and diffing. A known defect that is not fixed yet is a test wrapped in
-- @expectFailBecause@, named @known bug:@.
module Unit (tests) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import RIO
import RIO.Directory (doesFileExist, listDirectory)
import RIO.FilePath ((</>))
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.App.Options (Comparison (..), MigrateOptions (..), Output (..), SameTypeSwap (..), TableFlags (..), TableMode (..), TableOptions (..), tableOptions)
import Sqiff.App.Table (forTable)
import Sqiff.App.Table.Rewrite (changesTableIn)
import Sqiff.Config (fill, match)
import Sqiff.ConnectionString (connectionString, fromPairs, libpqText, redacted)
import Sqiff.Definition hiding (columns)
import Sqiff.Dependencies (indexEdges, noDependencies)
import Sqiff.Diff (Change (..), affected, diff)
import Sqiff.Folder (Existing (..), exportManifest, writeFolder)
import Sqiff.Graph (schedule)
import Sqiff.Identifier (identifier, qname, quoteIdent, typeName)
import Sqiff.Model (Address (..), Aspect (..), Edge (..), ObjectId (..), ObjectKey (..), RoutineKind (..), fromAddress, toAddress)
import Sqiff.Plan (context, invert, topoOrder)
import Sqiff.Recipe (Hazard (..), Phase (..), Role (..), Step (..), Txn (..), Undo (..))
import Sqiff.Rewrite (fileDrift)
import Sqiff.Script (splitStatements, withoutTransactionControl)
import Sqiff.Sqitch.Check (Tagging (..), batchProblems, changeNameProblem, chooseTag, tagNameProblem)
import Sqiff.Sqitch.Layout (Layout (..), resolveLayout, sqitchConf)
import Sqiff.Sqitch.Plan (SqitchPlan (..), parsePlan)
import Sqiff.Sqitch.Units (Placement (..), Unit (..), UnitKind (..), renderScript, tidyIndex, units, verifyScript)
import Sqiff.Sql qualified as Sql
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)


tests :: TestTree
tests =
    testGroup
        "unit"
        [ quoting
        , sqlText
        , connections
        , addresses
        , templates
        , scripts
        , statementScanner
        , plans
        , ordering
        , diffing
        , columns
        , formatting
        , tableFlags
        , sqitchChecks
        , folderExport
        ]


sqlText :: TestTree
sqlText =
    testGroup
        "SQL text"
        [ testCase "replaceInCode leaves literals, quoted names and comments alone"
            $ Sql.replaceInCode " WHERE " " where " "x WHERE y = ' WHERE ' -- WHERE \n and \" WHERE \" WHERE z"
            @?= "x where y = ' WHERE ' -- WHERE \n and \" WHERE \" where z"
        , testCase "replaceInCode leaves dollar-quoted bodies alone"
            $ Sql.replaceInCode "concurrently " "" "create index concurrently i on t (x) where f($$concurrently $$)"
            @?= "create index i on t (x) where f($$concurrently $$)"
        , testCase "dollarQuoted picks a tag the body lacks" do
            Sql.dollarQuoted "select 1" @?= "$$select 1$$"
            Sql.dollarQuoted "a $$ b" @?= "$sqiff$a $$ b$sqiff$"
            Sql.dollarQuoted "$$ $sqiff$" @?= "$sqiff1$$$ $sqiff$$sqiff1$"
        , testCase "a body ending in a dollar does not close the quote early"
            $ Sql.dollarQuoted "x$"
            @?= "$sqiff$x$$sqiff$"
        ]


quoting :: TestTree
quoting =
    testGroup
        "quoteIdent"
        [ testCase "lower case stays bare" $ quoteIdent "payment_2" @?= "payment_2"
        , testCase "upper case is quoted" $ quoteIdent "Payment" @?= "\"Payment\""
        , testCase "reserved words are quoted" $ quoteIdent "user" @?= "\"user\""
        , testCase "unreserved keywords stay bare" $ quoteIdent "name" @?= "name"
        , testCase "a leading digit is quoted" $ quoteIdent "1a" @?= "\"1a\""
        , testCase "embedded quotes are doubled" $ quoteIdent "a\"b" @?= "\"a\"\"b\""
        , testCase "the empty name is quoted" $ quoteIdent "" @?= "\"\""
        ]


connections :: TestTree
connections =
    testGroup
        "connection strings"
        [ testCase "keyword form"
            $ redacted (connectionString "host=h password=s3cret dbname=d")
            @?= "host=h password=*** dbname=d"
        , testCase "quoted keyword value"
            $ redacted (connectionString "password='a \\' b' host=h")
            @?= "password=*** host=h"
        , testCase "spaces around the equals sign"
            $ redacted (connectionString "host=h password = s3cret")
            @?= "host=h password=***"
        , testCase "a key that only ends in password stays"
            $ redacted (connectionString "host=h xpassword=x")
            @?= "host=h xpassword=x"
        , testCase "URI user info"
            $ redacted (connectionString "postgresql://me:s3cret@h:5432/d")
            @?= "postgresql://me:***@h:5432/d"
        , testCase "URI query parameter"
            $ redacted (connectionString "postgres://h/d?sslmode=require&password=s3cret")
            @?= "postgres://h/d?sslmode=require&password=***"
        , testCase "the empty string is libpq's environment"
            $ libpqText (connectionString "")
            @?= ""
        , testCase "fromPairs quotes values"
            $ libpqText (fromPairs [("host", "h"), ("password", "it's")])
            @?= "host='h' password='it\\'s'"
        , testCase "show never prints the password"
            $ T.isInfixOf "s3cret" (T.pack (show (fromPairs [("password", "s3cret")])))
            @?= False
        ]


addresses :: TestTree
addresses =
    testGroup
        "catalog addresses"
        [ testProperty "toAddress inverts fromAddress" $ property do
            addr <- forAll address
            toAddress (fromAddress addr) === addr
        , testProperty "only unknown kinds stay unparsed" $ property do
            addr <- forAll address
            let parsed = fromAddress addr
                known = addr.objectType `elem` map fst kinds && shapeFits addr
            known === not (isUnknown parsed)
        , testCase "a column"
            $ fromAddress (Address "table column" ["s", "T", "c"] [])
            @?= Column (qname "s" "T") (identifier "c")
        , testCase "a function keeps its argument types"
            $ toAddress (fromAddress (Address "function" ["s", "f"] ["integer", "text"]))
            @?= Address "function" ["s", "f"] ["integer", "text"]
        ]
  where
    -- Each kind sqiff models, with how many names and arguments it has (Nothing: any).
    kinds :: [(Text, (Int, Maybe Int))]
    kinds =
        [ ("schema", (1, Just 0))
        , ("extension", (1, Just 0))
        , ("table", (2, Just 0))
        , ("view", (2, Just 0))
        , ("materialized view", (2, Just 0))
        , ("table column", (3, Just 0))
        , ("view column", (3, Just 0))
        , ("materialized view column", (3, Just 0))
        , ("composite type column", (3, Just 0))
        , ("foreign table column", (3, Just 0))
        , ("table constraint", (3, Just 0))
        , ("domain constraint", (1, Just 1))
        , ("index", (2, Just 0))
        , ("sequence", (2, Just 0))
        , ("type", (1, Just 0))
        , ("function", (2, Nothing))
        , ("procedure", (2, Nothing))
        , ("aggregate", (2, Nothing))
        , ("trigger", (3, Just 0))
        , ("policy", (3, Just 0))
        ]
    shapeFits addr = case lookup addr.objectType kinds of
        Just (names, args) -> length addr.objectNames == names && maybe True (== length addr.objectArgs) args
        Nothing -> False
    isUnknown = \case
        Unknown _ -> True
        _ -> False
    address :: Gen Address
    address = do
        kind <- Gen.choice [Gen.element (map fst kinds), Gen.element ["operator", "cast", "rule", "foreign table"]]
        names <- Gen.list (Range.linear 0 4) name
        args <- Gen.list (Range.linear 0 2) name
        pure (Address kind names args)
    name = Gen.text (Range.linear 0 6) (Gen.element ("aZ_.\" 1" :: String))


templates :: TestTree
templates =
    testGroup
        "name templates"
        [ testCase "fill" $ fill "{schema}/table/{name}" [("schema", "s"), ("name", "t")] @?= "s/table/t"
        , testCase "match" $ match "{schema}/table/{name}" "s/table/t" @?= Just [("schema", "s"), ("name", "t")]
        , testCase "a placeholder does not span a slash" $ match "{schema}/table/{name}" "s/table/a/b" @?= Nothing
        , testCase "a placeholder is not empty" $ match "{schema}/table/{name}" "/table/t" @?= Nothing
        , testProperty "match inverts fill" $ property do
            pattern <- forAll (Gen.element ["{schema}/table/{name}", "{schema}/trigger/{table}/{name}", "{schema}/table/{table}/index/{name}"])
            let keys = placeholders pattern
            values <- forAll (traverse (\k -> (k,) <$> segment) keys)
            match pattern (fill pattern values) === Just values
        , testCase "a value is not expanded again"
            $ fill "{schema}/{name}" [("schema", "{name}"), ("name", "t")]
            @?= "{name}/t"
        ]
  where
    placeholders p = [T.takeWhile (/= '}') rest | rest <- drop 1 (T.split (== '{') p)]
    segment :: Gen Text
    -- [allow:SqiffInterpolate] a set of characters to draw from, not text
    segment = Gen.text (Range.linear 1 12) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "_-."))


scripts :: TestTree
scripts =
    testGroup
        "scripts"
        [ testCase "splits on top-level semicolons"
            $ splitStatements "create table a (x int);\ncreate table b (y int);\n"
            @?= Right ["create table a (x int)", "create table b (y int)"]
        , testCase "keeps semicolons in quotes and comments"
            $ splitStatements "select ';' as \"a;b\"; -- c;\n/* d; */ select 1;"
            @?= Right ["select ';' as \"a;b\"", "-- c;\n/* d; */ select 1"]
        , testCase "keeps a dollar-quoted body whole"
            $ fmap length (splitStatements "create function f() returns int language plpgsql as $f$ begin return 1; end $f$;")
            @?= Right 1
        , testCase "keeps a BEGIN ATOMIC body whole"
            $ fmap length (splitStatements "create function f() returns int language sql begin atomic select 1; select 2; end;")
            @?= Right 1
        , testCase "an unclosed quote is an error"
            $ isLeft (splitStatements "select 'x")
            @?= True
        , testCase "a comment-only tail is no statement"
            $ splitStatements "select 1;\n-- the end\n"
            @?= Right ["select 1"]
        , testCase "drops transaction control"
            $ withoutTransactionControl ["begin", "create table a ()", "commit"]
            @?= Right ["create table a ()"]
        , testCase "keeps ROLLBACK TO"
            $ withoutTransactionControl ["rollback to sp"]
            @?= Right ["rollback to sp"]
        , testCase "refuses PREPARE TRANSACTION"
            $ isLeft (withoutTransactionControl ["prepare transaction 'x'"])
            @?= True
        , testCase "keeps ROLLBACK WORK TO"
            $ withoutTransactionControl ["rollback work to sp"]
            @?= Right ["rollback work to sp"]
        ]


-- | Exact outputs of 'splitStatements' for each construct its scanner tracks.
statementScanner :: TestTree
statementScanner =
    testGroup
        "statement scanner"
        [ testCase "a nested block comment closes at its outer end"
            $ splitStatements "select 1 /* a /* b; */ c; */; select 2;"
            @?= Right ["select 1 /* a /* b; */ c; */", "select 2"]
        , testCase "a dollar quote closes only at its own tag"
            $ splitStatements "do $f$ begin perform $$;$$; end $f$; select $a$x;$a$;"
            @?= Right ["do $f$ begin perform $$;$$; end $f$", "select $a$x;$a$"]
        , testCase "a positional parameter and a dollar inside a name are no dollar quote"
            $ splitStatements "select a$b$c, $1; select 2"
            @?= Right ["select a$b$c, $1", "select 2"]
        , testCase "an escape string keeps backslash-escaped and doubled quotes"
            $ splitStatements "select E'a\\';b', e'c'';d'; select 2;"
            @?= Right ["select E'a\\';b', e'c'';d'", "select 2"]
        , testCase "a backslash ends a standard string"
            $ splitStatements "select 'a\\'; select 2;"
            @?= Right ["select 'a\\'", "select 2"]
        , testCase "doubled quotes stay inside a string and a quoted name"
            $ splitStatements "select 'a'';b' as \"c\"\";d\"; select 2;"
            @?= Right ["select 'a'';b' as \"c\"\";d\"", "select 2"]
        , testCase "a BEGIN ATOMIC body ends at its END, past CASE ... END"
            $ splitStatements "create function f() returns int language sql BEGIN ATOMIC select case when true then 1 end; end; select 2;"
            @?= Right ["create function f() returns int language sql BEGIN ATOMIC select case when true then 1 end; end", "select 2"]
        , testCase "CASE ... END outside BEGIN ATOMIC does not hold the statement open"
            $ splitStatements "select case when true then 1 end; select 2;"
            @?= Right ["select case when true then 1 end", "select 2"]
        , testCase "a comment between BEGIN and ATOMIC still starts a body"
            $ splitStatements "begin/**/atomic select 1; end; select 2;"
            @?= Right ["begin/**/atomic select 1; end", "select 2"]
        , testCase "a quote between BEGIN and ATOMIC starts no body"
            $ splitStatements "begin 'x' atomic; select 2;"
            @?= Right ["begin 'x' atomic", "select 2"]
        , testCase "psql output commands are skipped"
            $ splitStatements "\\set ON_ERROR_STOP on\n\\pset pager off\n\\unset QUIET\nselect 1;"
            @?= Right ["select 1"]
        , testCase "other psql commands are refused"
            $ splitStatements "\\i other.sql\nselect 1;"
            @?= Left "psql's \\i cannot run outside psql"
        , testCase "\\set of a lowercase variable is refused"
            $ splitStatements "\\set foo bar\nselect 1;"
            @?= Left "psql's \\set cannot run outside psql"
        , testCase "a backslash after code is part of the statement"
            $ splitStatements "select 1 \\x\n;"
            @?= Right ["select 1 \\x"]
        , testCase "an unterminated last statement is kept"
            $ splitStatements "select 1; select 2 -- done"
            @?= Right ["select 1", "select 2 -- done"]
        , testCase "an unclosed dollar quote is an error"
            $ splitStatements "select $$x;"
            @?= Left "a quote, dollar quote or comment is not closed"
        , testCase "an unclosed nested comment is an error"
            $ splitStatements "select 1; /* a /* b */"
            @?= Left "a quote, dollar quote or comment is not closed"
        , testCase "an unclosed escape string is an error"
            $ splitStatements "select E'a\\'"
            @?= Left "a quote, dollar quote or comment is not closed"
        , testCase "a BEGIN ATOMIC body without END is an error"
            $ splitStatements "create function f() returns int language sql begin atomic select 1;"
            @?= Left "a BEGIN ATOMIC body has no END"
        , testCase "a BEGIN ATOMIC body whose END ends the input is closed"
            $ splitStatements "create function f() returns int language sql begin atomic select 1; end"
            @?= Right ["create function f() returns int language sql begin atomic select 1; end"]
        ]


plans :: TestTree
plans =
    testGroup
        "sqitch plan"
        [ testCase "reads the project, changes and tags" do
            let p =
                    parsePlan
                        . T.unlines
                        $ [ "%syntax-version=1.0.0"
                          , "%project=demo"
                          , ""
                          , "a 2024-01-01T00:00:00Z Me <me@example.com> # first"
                          , "b [a] 2024-01-02T00:00:00Z Me <me@example.com> # second"
                          , "@v1 2024-01-03T00:00:00Z Me <me@example.com> # tag"
                          , "c 2024-01-04T00:00:00Z Me <me@example.com> # third"
                          ]
            p.project @?= "demo"
            p.changes @?= ["a", "b", "c"]
            p.tagged @?= Set.fromList ["a", "b"]
        , testCase "a reworked change is tagged only if its last entry is" do
            let p = parsePlan (T.unlines ["%project=demo", "a x", "@v1 x", "a x"])
            p.changes @?= ["a", "a"]
            p.tagged @?= Set.empty
        ]


ordering :: TestTree
ordering =
    testGroup
        "topoOrder"
        [ testCase "puts a node after what it needs"
            $ topoOrder (Set.fromList "abc") [('a', 'b'), ('b', 'c')]
            @?= "cba"
        , testCase "breaks ties by the smaller node"
            $ topoOrder (Set.fromList "cab") []
            @?= "abc"
        , testCase "ignores pairs outside the nodes"
            $ topoOrder (Set.fromList "ab") [('a', 'z')]
            @?= "ab"
        , testCase "puts cyclic nodes last"
            $ topoOrder (Set.fromList "abc") [('a', 'b'), ('b', 'a')]
            @?= "cab"
        , testCase "schedule takes the smallest priority among the ready"
            $ scheduleChars [(2, 'a'), (1, 'b'), (3, 'c')] []
            @?= Right "bac"
        , testCase "schedule lets a constraint beat priority"
            $ scheduleChars [(1, 'f'), (2, 'v')] [('f', 'v')]
            @?= Right "vf"
        , testCase "schedule reports what a cycle blocks"
            $ scheduleChars [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')] [('a', 'b'), ('b', 'a'), ('d', 'a')]
            @?= Left ("c", "abd")
        , testProperty "every pair is respected in an acyclic graph" $ property do
            n <- forAll (Gen.int (Range.linear 1 30))
            pairs <- forAll (Gen.list (Range.linear 0 60) ((,) <$> Gen.int (Range.linear 0 (n - 1)) <*> Gen.int (Range.linear 0 (n - 1))))
            -- Only pairs from a higher node to a lower one, which cannot form a cycle.
            let acyclic = [(x, y) | (x, y) <- pairs, x > y]
                order = topoOrder (Set.fromList [0 .. n - 1]) acyclic
                position = Map.fromList (zip order [0 :: Int ..])
            length order === n
            [(x, y) | (x, y) <- acyclic, Map.lookup x position < Map.lookup y position] === []
        ]


diffing :: TestTree
diffing =
    testGroup
        "diff"
        [ testCase "finds added, dropped and changed definitions"
            $ diff (\_ a b -> a == b) (Map.fromList [(k "a", "1"), (k "b", "2"), (k "c", "3") :: (ObjectKey, Text)]) (Map.fromList [(k "b", "2"), (k "c", "4"), (k "d", "5")])
            @?= Map.fromList [(k "a", Dropped "1"), (k "c", Changed "3" "4"), (k "d", Added "5")]
        , testCase "a dependent whose comment changes still depends" do
            let column = Column (qname "public" "t") (identifier "a")
                v = View (qname "public" "v")
                changes = Map.fromList [(ObjectKey Definition column, Changed "int" "bigint"), (ObjectKey Comment v, Changed "old" "new" :: Change Text)]
            affected (Set.fromList [column, v]) (indexEdges [Edge v column]) changes @?= Map.fromList [(column, Set.singleton v)]
        , testCase "the sameness test can hide a change"
            $ diff (\_ _ _ -> True) (Map.fromList [(k "a", "1") :: (ObjectKey, Text)]) (Map.fromList [(k "a", "2")])
            @?= Map.empty
        , testProperty "invert is an involution" $ property do
            c <- forAll (Gen.choice [Added <$> txt, Dropped <$> txt, Changed <$> txt <*> txt])
            invert (invert c) === c
        ]
  where
    k n = ObjectKey Definition (fromAddress (Address "table" ["public", n] []))
    txt = Gen.text (Range.linear 0 5) Gen.alpha


columns :: TestTree
columns =
    testGroup
        "definitions"
        [ testCase "a column decodes and prints as before" do
            c <- decode @ColumnDef "{\"type\": \"integer\", \"collation\": \"C\", \"not_null\": {\"validated\": true, \"name\": \"x_nn\"}, \"expression\": \"42\"}"
            printed (ColumnDefinition c) @?= T.intercalate "\n" ["type integer", "collate \"C\"", "not null constraint x_nn", "default 42"]
        , testCase "NOT VALID, identity and its options" do
            c <- decode @ColumnDef "{\"type\": \"bigint\", \"not_null\": {\"validated\": false}, \"identity\": \"by default\", \"identity_options\": [\"increment by 5\"]}"
            c.nullability @?= NotNull NotValidated Nothing
            c.value @?= IdentityColumn ByDefault ["increment by 5"]
            printed (ColumnDefinition c) @?= "type bigint\nnot null not valid\nidentity by default\nidentity options increment by 5"
        , testCase "a generated column" do
            c <- decode @ColumnDef "{\"type\": \"int\", \"generated\": \"stored\", \"expression\": \"(a + 1)\"}"
            c.value @?= GeneratedColumn Stored "(a + 1)"
        , testCase "a multi-line default stays whole" do
            c <- decode @ColumnDef "{\"type\": \"text\", \"expression\": \"'a\\ntype b'::text\"}"
            c.colType @?= typeName "text"
            c.value @?= PlainColumn (Just "'a\ntype b'::text")
        , testCase "identity and generated together are refused"
            $ isLeft (Aeson.eitherDecodeStrict @ColumnDef "{\"type\": \"int\", \"identity\": \"always\", \"generated\": \"stored\", \"expression\": \"1\"}")
            @?= True
        , testCase "a role name with a space stays whole" do
            acl <- decode @Acl "[{\"grantee\": \"report readers\", \"privilege\": \"SELECT\", \"grantable\": false}, {\"grantee\": null, \"privilege\": \"USAGE\", \"grantable\": true}]"
            acl @?= ExplicitAcl (Set.fromList [Grant (Role (identifier "report readers")) "SELECT" False ByOwner, Grant Public "USAGE" True ByOwner])
            printed (AclDefinition acl) @?= "public USAGE with grant option\nreport readers SELECT"
        , testCase "drift ignores validity but not the words \"not valid\" in a comment" do
            let key o = ObjectKey Comment o
                table = Table (qname "public" "t")
                column validity = ColumnDefinition (plainColumn "int"){nullability = NotNull validity Nothing}
                columnKey = ObjectKey Definition (Column (qname "public" "t") (identifier "c"))
            Map.keys (fileDrift Set.empty (Map.fromList [(key table, Printed "is not valid")]) (Map.fromList [(key table, Printed "is")])) @?= [key table]
            fileDrift Set.empty (Map.fromList [(columnKey, column NotValidated)]) (Map.fromList [(columnKey, column Validated)]) @?= Map.empty
        , testCase "a foreign key knows its columns and target" do
            c <- decode @ConstraintDef "{\"kind\": \"f\", \"validated\": false, \"columns\": [\"Parent Id\"], \"references\": {\"schema\": \"a.b\", \"table\": \"t\", \"columns\": [\"id\"]}, \"clause\": \"FOREIGN KEY (\\\"Parent Id\\\") REFERENCES \\\"a.b\\\".t(id) NOT VALID\"}"
            c.kind @?= ForeignKey
            fmap (.table) c.references @?= Just (qname "a.b" "t")
            withoutValidity c @?= c{validity = Validated, clause = "FOREIGN KEY (\"Parent Id\") REFERENCES \"a.b\".t(id)"}
        , testCase "a table's settings print one per line" do
            t <- decode @TableDef "{\"unlogged\": true, \"partition_by\": \"RANGE (at)\", \"row_security\": true, \"force_row_security\": false, \"options\": [\"fillfactor=70\", \"toast.autovacuum_enabled=false\"]}"
            tableText t @?= "unlogged\npartition by RANGE (at)\nrow level security\nwith (fillfactor=70, toast.autovacuum_enabled=false)"
        , testCase "literal matches quote_literal" do
            Sql.literal "it's" @?= "'it''s'"
            Sql.literal "a\\, b" @?= "E'a\\\\, b'"
        , testCase "no ACL is the default, an empty one is not" do
            decode "null" >>= (@?= DefaultAcl)
            decode "[]" >>= (@?= ExplicitAcl Set.empty)
        ]
  where
    decode :: FromJSON a => ByteString -> IO a
    decode = either (assertFailure . ("does not decode: " <>)) pure . Aeson.eitherDecodeStrict


formatting :: TestTree
formatting =
    testGroup
        "tidyIndex"
        [ testCase "lowers keywords and drops USING btree"
            $ tidyIndex "CREATE INDEX t_x ON public.t USING btree (x) WHERE (x > 0)"
            @?= "create index t_x on public.t (x) where (x > 0)"
        , testCase "keeps a non-default method"
            $ tidyIndex "CREATE INDEX t_x ON public.t USING gin (x)"
            @?= "create index t_x on public.t using gin (x)"
        , testCase "leaves string literals alone"
            $ tidyIndex "CREATE INDEX t_x ON public.t USING btree (x) WHERE (y <> ' WHERE ')"
            @?= "create index t_x on public.t (x) where (y <> ' WHERE ')"
        ]


tableFlags :: TestTree
tableFlags =
    testGroup
        "table options"
        [ testCase "the file comparison by default, with its renames"
            $ describeMode (tableOptions flags{renames = [rename]})
            @?= Right "base file, 1 renames, ask if rename"
        , testCase "--against-database with --allow-drops"
            $ describeMode (tableOptions flags{againstDatabase = True, allowDrops = True})
            @?= Right "live table"
        , testCase "--rewrite-file with --live-from"
            $ describeMode (tableOptions flags{rewriteFile = True, liveFrom = Just (connectionString "dbname=live")})
            @?= Right "rewrite from dbname=live"
        , testCase "refuses flags that do not apply together" do
            let refused f = either (const True) (const False) (tableOptions f)
            for_
                [ flags{rewriteFile = True, renames = [rename]}
                , flags{rewriteFile = True, againstDatabase = True}
                , flags{rewriteFile = True, allowDrops = True}
                , flags{rewriteFile = True, dropAndAdd = True}
                , flags{againstDatabase = True, renames = [rename]}
                , flags{liveFrom = Just (connectionString "dbname=live")}
                , flags{allowDrops = True}
                ]
                \f -> refused f @?= True
        ]
  where
    rename = (identifier "a", identifier "b")
    flags =
        TableFlags
            { output = PrintMigration
            , againstDatabase = False
            , allowDrops = False
            , base = "master"
            , project = "."
            , top = Nothing
            , planPath = Nothing
            , tableTag = Nothing
            , renames = []
            , dropAndAdd = False
            , rewriteFile = False
            , liveFrom = Nothing
            }
    describeMode :: Either Text TableOptions -> Either Text Text
    describeMode = fmap \o -> case o.mode of
        MigrateToFile m ->
            let swap = if m.sameTypeSwap == AskIfRename then "ask if rename" else "drop and add" :: Text
             in case m.comparison of
                    WithBaseFile rs -> [i|base file, #{tshow (length rs)} renames, #{swap}|]
                    WithLiveTable _ -> "live table"
        RewriteFile live -> "rewrite from " <> maybe "--db" redacted live


sqitchChecks :: TestTree
sqitchChecks =
    testGroup
        "sqitch checks"
        [ testCase "change names sqitch and the file system accept" do
            for_ ["public/function/f", "migrations/2026/11/2026-11-18-120000-add-x", "_x", "v1.2+1"] \n ->
                changeNameProblem n @?= Nothing
        , testCase "change names they refuse" do
            for_ ["", "a b", "a:b", "a@b", "a#b", "a\\b", "-a", "a.", "a^2", "a~12", "/abs", "a/../b", "a//b", "./a"] \n ->
                isJust (changeNameProblem n) @?= True
        , testCase "tags cannot have a slash" do
            isJust (tagNameProblem "a/b") @?= True
            tagNameProblem "2026-11-18_sqiff" @?= Nothing
        , testCase "the day's tag, or the next free one" do
            let at = Tagging{given = Nothing, now = UTCTime (fromGregorian 2026 11 18) 0}
            chooseTag (plan "") at @?= Right "2026-11-18_sqiff"
            chooseTag (plan "a 2026-01-01T00:00:00Z x <x@x> # a\n@2026-11-18_sqiff 2026-01-01T00:00:00Z x <x@x> # t\n") at @?= Right "2026-11-18_sqiff_2"
        , testCase "a given tag must be new" do
            let at = Tagging{given = Just "v1", now = UTCTime (fromGregorian 2026 11 18) 0}
            isLeft (chooseTag (plan "a 2026-01-01T00:00:00Z x <x@x> # a\n@v1 2026-01-01T00:00:00Z x <x@x> # t\n") at) @?= True
            chooseTag (plan "") at @?= Right "v1"
        , testCase "the plan's tags" do
            (plan "a 2026-01-01T00:00:00Z x <x@x> # a\n@v1 2026-01-01T00:00:00Z x <x@x> # t\nb [a] 2026-01-01T00:00:00Z x <x@x> # b\n@v2 2026-01-01T00:00:00Z x <x@x> # t\n").tags
                @?= Set.fromList ["v1", "v2"]
        , testCase "a batch with a collision, an existing add or a rework of nothing" do
            let p = plan "a 2026-01-01T00:00:00Z x <x@x> # a\n"
            length (batchProblems p [unitNamed AddChange "b", unitNamed AddChange "b"]) @?= 1
            length (batchProblems p [unitNamed AddChange "a"]) @?= 1
            length (batchProblems p [unitNamed ReworkChange "c"]) @?= 1
            batchProblems p [unitNamed ReworkChange "a", unitNamed AddChange "b"] @?= []
        , testCase "a column's position does not make it differ" do
            let at n = ColumnDefinition (plainColumn "int"){position = Position n}
                key = ObjectKey Definition (Column (qname "public" "t") (identifier "a"))
            diff (\_ a b -> a == b) (Map.fromList [(key, at 1)]) (Map.fromList [(key, at 3)]) @?= Map.empty
        , testCase "verify checks a column's full type, not only its base type" do
            let col ty = ColumnDefinition (plainColumn ty)
                object = Column (qname "public" "t") (identifier "a")
                retype =
                    (createStep object "alter table public.t alter column a type character varying(20)")
                        { rank = 6
                        , change = Changed (col "character varying(10)") (col "character varying(20)")
                        }
                script = verifyScript "-- verify" [retype]
            ("format_type(atttypid, atttypmod)" `T.isInfixOf` script, "= 'character varying(20)'" `T.isInfixOf` script) @?= (True, True)
        , testCase "verify checks routines, types, schemas and extensions" do
            let made object = (createStep object "create"){rank = 1, change = Added (Printed "x")}
                script = verifyScript "-- verify" (map made [Routine Function (qname "app" "f") [typeName "integer"], Type (typeName "app.mood"), Schema (identifier "app"), Extension (identifier "pgcrypto"), Table (qname "app" "t")])
            for_ ["to_regprocedure('app.f(integer)') is not null", "to_regtype('app.mood') is not null", "nspname = 'app'", "extname = 'pgcrypto'", "to_regclass('app.t') is not null"] \check ->
                (check, check `T.isInfixOf` script) @?= (check, True)
        , testCase "verify does not expect an object dropped to be created again to be gone" do
            let object = MaterializedView (qname "app" "mv")
                drop' =
                    (createStep object "drop materialized view app.mv")
                        { phase = DropPhase
                        , role = Drops
                        , rank = 1
                        , change = Changed (Printed "a") (Printed "b")
                        }
            ("still exists" `T.isInfixOf` verifyScript "-- verify" [drop']) @?= False
        , testCase "a step sqiff cannot write stops the deploy" do
            let object = Type (typeName "app.mood")
                manual =
                    (createStep object "-- only adding enum values is automated")
                        { rank = 8
                        , hazards = [Manual "only adding enum values is automated"]
                        , change = Changed (Printed "a") (Printed "b")
                        }
            ("raise exception" `T.isInfixOf` renderScript "-- deploy" Migration [manual]) @?= True
        , testCase "a table's migrations are found by its name as migration names write it" do
            let names = either error id (Aeson.eitherDecode "{}")
                ours = forTable names (qname "App" "Big Orders")
            ours "migrations/2026/11/2026-11-18-120000-add-column-app.big-orders.total" @?= True
            ours "migrations/2026/11/2026-11-18-120000-add-column-app.big-ordersx" @?= False
            ours "App/table/Big Orders/index/total_idx" @?= True
        , testCase "which statements change a table" do
            let changes = changesTableIn (qname "app" "orders")
            for_
                [ "alter table app.orders add column x int"
                , "ALTER TABLE IF EXISTS ONLY app.orders drop column x"
                , "drop table if exists app.other, app.orders cascade"
                , "drop table orders"
                , "alter table \"app\".\"orders\" add column y int"
                , "grant select on table app.orders to reader"
                , "revoke all on app.orders from reader"
                , "comment on column app.orders.total is 'x'"
                , "create index orders_total on app.orders (total)"
                , "create table if not exists app.orders (id int)"
                , "do $ begin perform 1 from app.orders; end $"
                ]
                \statement -> (statement, changes statement) @?= (statement, True)
            for_
                [ "alter table app.orders_archive add column x int"
                , "drop table other.orders"
                , "create view app.v as select * from app.orders"
                , "grant select on app.order_lines to reader"
                , "do $ begin perform 1 from app.orders_archive; end $"
                ]
                \statement -> (statement, changes statement) @?= (statement, False)
        , testCase "words and lists keep quoted names whole" do
            Sql.codeWords "()," "\"a, b\" int unique, -- note\n c text" @?= ["\"a, b\"", "int", "unique", "c", "text"]
            Sql.parenthesised "unique (\"a)\", (b)) x" @?= Just "\"a)\", (b)"
            Sql.splitTopLevel ',' "\"a, b\", f(c, d), 'e,f'" @?= ["\"a, b\"", "f(c, d)", "'e,f'"]
        , testCase "overloads of a new function make one change" do
            let names = either error id (Aeson.eitherDecode "{}")
                ctx = context Map.empty Map.empty Map.empty noDependencies Map.empty
                overload arg =
                    let object = Routine Function (qname "public" "f") [typeName arg]
                     in (createStep object [i|create function public.f(#{arg})|]){rank = 9, change = Added (Printed arg)}
                written = units names (const True) ctx Map.empty (plan "") (UTCTime (fromGregorian 2026 11 18) 0) [overload "int", overload "text"]
            map (\u -> (u.changeName, length u.deploy)) written @?= [("public/function/f", 2)]
        , testCase "the layout: the target's setting beats the engine's, which beats the core's" do
            let merged =
                    [ ("core.engine", "pg")
                    , ("core.top_dir", "core")
                    , ("engine.pg.top_dir", "engine")
                    , ("engine.pg.target", "prod")
                    , ("target.prod.deploy_dir", "elsewhere/deploy")
                    ]
            l <- either (assertFailure . T.unpack) pure (resolveLayout "/p" Nothing Nothing merged [])
            (l.topDir, l.planFile, l.deployDir, l.revertDir, l.extension) @?= ("engine", "engine/sqitch.plan", "elsewhere/deploy", "engine/revert", "sql")
        , testCase "the layout: --top-dir moves the scripts and the plan" do
            l <- either (assertFailure . T.unpack) pure (resolveLayout "/p" (Just "alt") Nothing [("core.top_dir", "db"), ("core.deploy_dir", "db/d")] [])
            (l.topDir, l.planFile, l.deployDir) @?= ("alt", "alt/sqitch.plan", "alt/deploy")
        , testCase "sqitch runs with the layout pinned for the core, the engine and the target" do
            let settings = [("core.engine", "pg"), ("core.top_dir", "db"), ("engine.pg.target", "prod"), ("target.prod.uri", "db:pg://h/x")]
            l <- either (assertFailure . T.unpack) pure (resolveLayout "/p" (Just "alt") Nothing settings settings)
            let conf = sqitchConf l
            for_ ["[core]", "[engine \"pg\"]", "[target \"prod\"]", "\ttop_dir = \"alt\"", "\turi = \"db:pg://h/x\""] \line ->
                (line, line `T.isInfixOf` conf) @?= (line, True)
            ("top_dir = \"db\"" `T.isInfixOf` conf) @?= False
        , testCase "a rework copies the scripts next to them, not under top_dir twice" do
            l <- either (assertFailure . T.unpack) pure (resolveLayout "/p" Nothing Nothing [("core.engine", "pg"), ("core.top_dir", "database")] [])
            let conf = sqitchConf l
            for_ ["\treworked_deploy_dir = \"database/deploy\"", "\treworked_revert_dir = \"database/revert\"", "\treworked_verify_dir = \"database/verify\""] \line ->
                (line, line `T.isInfixOf` conf) @?= (line, True)
        ]
  where
    plan entries = parsePlan ("%syntax-version=1.0.0\n%project=p\n\n" <> entries)
    unitNamed kind changeName = Unit{kind, changeName, note = "", placement = Migration, deploy = [], revert = []}


folderExport :: TestTree
folderExport =
    testGroup
        "describe --out"
        [ testCase "names that differ give files that differ" do
            let paths = Map.keys (exportManifest (Map.fromList [(table n, Printed n) | n <- ["a b", "a-b", "a/b", "a%2Fb", ".a", "%2Ea"]]))
            length paths @?= 6
        , testCase "names that differ only in case get a hash, as some file systems fold case" do
            let paths = Map.keys (exportManifest (Map.fromList [(table n, Printed n) | n <- ["Orders", "orders", "other"]]))
                folded = Set.fromList (map (T.toLower . T.pack) paths)
            (length paths, Set.size folded) @?= (3, 3)
            ("table/public/other.sql" `elem` paths) @?= True
        , testCase "a kind of two words is a folder with a dash" do
            let paths = Map.keys (exportManifest (Map.fromList [(ObjectKey Definition (Column (qname "public" "orders") (identifier "amount")), Printed "integer")]))
            paths @?= ["table-column/public/orders/amount.sql"]
        , testCase "an empty schema writes the folder and its marker" do
            withSystemTempDirectory "sqiff-folder" \dir -> do
                writeFolder KeepExisting (dir </> "out") Map.empty
                doesFileExist (dir </> "out" </> ".sqiff") >>= (@?= True)
        , testCase "--replace swaps in the new export, and keeps the old one when writing fails" do
            withSystemTempDirectory "sqiff-folder" \dir -> do
                let out = dir </> "out"
                writeFolder KeepExisting out (Map.fromList [(table "old", Printed "old")])
                writeFolder ReplaceOwn out (Map.fromList [(table "new", Printed "new")])
                listDirectory (out </> "table" </> "public") >>= (@?= ["new.sql"])
                -- The staging folder cannot be made where a file of its name is.
                writeFileUtf8 (out <> ".sqiff-staging") "in the way"
                result <- tryAny (writeFolder ReplaceOwn out (Map.fromList [(table "newer", Printed "newer")]))
                isLeft result @?= True
                listDirectory (out </> "table" </> "public") >>= (@?= ["new.sql"])
        ]
  where
    table n = ObjectKey Definition (Table (qname "public" n))


-- | 'schedule' over characters keyed by themselves, with the priorities given.
scheduleChars :: [(Int, Char)] -> [(Char, Char)] -> Either (String, String) String
scheduleChars items pairs = bimap (bimap (map snd) (map snd)) (map snd) (schedule snd fst items pairs)


-- | A shared create-phase step that creates @object@ with @sql@ and is undone by its
-- inverse; tests change the fields they are about.
createStep :: ObjectId -> Text -> Step
createStep object sql =
    Step
        { phase = CreatePhase
        , role = Creates
        , rank = 0
        , object
        , sql
        , txn = Shared
        , hazards = []
        , key = ObjectKey Definition object
        , change = Added (Printed sql)
        , undo = UndoObject
        }


-- | A nullable column of type @ty@, first in its table, with nothing else set.
plainColumn :: Text -> ColumnDef
plainColumn ty =
    ColumnDef
        { colType = typeName ty
        , collation = Nothing
        , nullability = Nullable
        , value = PlainColumn Nothing
        , rewritesOnAdd = False
        , position = Position 1
        , inheritedOnly = False
        }
