{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Tests of the pure parts: quoting, name templates, script splitting, plan parsing,
-- ordering and diffing. Tests named @known bug:@ pin down defects from the review
-- (docs/codex-review.md); they are expected to fail until the bug is fixed.
module Unit (tests) where

import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.Config (fill, match)
import Sqiff.ConnectionString (connectionString, fromPairs, libpqText, redacted)
import Sqiff.Diff (Change (..), diff)
import Sqiff.Identifier (identifier, qname, quoteIdent)
import Sqiff.Model (Address (..), Aspect (..), ObjectId (..), ObjectKey (..), fromAddress, toAddress)
import Sqiff.Plan (invert, topoOrder)
import Sqiff.Recipe (ColumnDef (..), parseColumn)
import Sqiff.Script (splitStatements, withoutTransactionControl)
import Sqiff.Sqitch (SqitchPlan (..), parsePlan, tidyIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)


tests :: TestTree
tests =
    testGroup
        "unit"
        [ quoting
        , connections
        , addresses
        , templates
        , scripts
        , plans
        , ordering
        , diffing
        , columns
        , formatting
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
        , expectFailBecause "fill substitutes one key at a time, so a value that looks like a placeholder is expanded again"
            $ testCase "known bug: a value is not expanded again"
            $ fill "{schema}/{name}" [("schema", "{name}"), ("name", "t")]
            @?= "{name}/t"
        ]
  where
    placeholders p = [T.takeWhile (/= '}') rest | rest <- drop 1 (T.split (== '{') p)]
    segment :: Gen Text
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
        , expectFailBecause "only ROLLBACK TO is recognised, not ROLLBACK WORK TO or ROLLBACK TRANSACTION TO"
            $ testCase "known bug: keeps ROLLBACK WORK TO"
            $ withoutTransactionControl ["rollback work to sp"]
            @?= Right ["rollback work to sp"]
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
            $ diff (\_ a b -> a == b) (Map.fromList [(k "a", "1"), (k "b", "2"), (k "c", "3")]) (Map.fromList [(k "b", "2"), (k "c", "4"), (k "d", "5")])
            @?= Map.fromList [(k "a", Dropped "1"), (k "c", Changed "3" "4"), (k "d", Added "5")]
        , testCase "the sameness test can hide a change"
            $ diff (\_ _ _ -> True) (Map.fromList [(k "a", "1")]) (Map.fromList [(k "a", "2")])
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
        "parseColumn"
        [ testCase "reads every field" do
            let c = parseColumn (T.unlines ["type integer", "collate \"C\"", "not null constraint t_x_not_null", "default 42"])
            c.colType @?= "integer"
            c.collation @?= Just "\"C\""
            c.notNull @?= True
            c.notNullValid @?= True
            c.notNullName @?= Just "t_x_not_null"
            c.defaultExpr @?= Just "42"
        , testCase "reads NOT VALID"
            $ (parseColumn "type text\nnot null not valid").notNullValid
            @?= False
        , expectFailBecause "the description is newline-delimited, so a default with a newline is cut and its rest read as fields"
            $ testCase "known bug: a multi-line default survives" do
                let c = parseColumn (T.unlines ["type text", "default 'a\ntype b'::text"])
                c.colType @?= "text"
                c.defaultExpr @?= Just "'a\ntype b'::text"
        ]


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
        , expectFailBecause "keywords are replaced anywhere, string literals included"
            $ testCase "known bug: leaves string literals alone"
            $ tidyIndex "CREATE INDEX t_x ON public.t USING btree (x) WHERE (y <> ' WHERE ')"
            @?= "create index t_x on public.t (x) where (y <> ' WHERE ')"
        ]
