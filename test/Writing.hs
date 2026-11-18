{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Writing changes with the real sqitch, in throwaway projects: the layout sqiff
-- resolves is the one sqitch writes to, a failure part-way leaves the project as it was,
-- and a rework on a day that already has sqiff's tag gets a new one.
module Writing (setUpSqitch, tests) where

import Data.String.Interpolate (i)
import Data.Time (UTCTime (..), fromGregorian)
import RIO
import RIO.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getPermissions, setOwnerWritable, setPermissions)
import RIO.FilePath ((</>))
import RIO.List (sort)
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.Connection (UserError (..))
import Sqiff.Definition (Definition (..))
import Sqiff.Diff (Change (..))
import Sqiff.Identifier (qname)
import Sqiff.Model (Aspect (..), ObjectId (..), ObjectKey (..))
import Sqiff.Recipe (Phase (..), Step (..), Txn (..))
import Sqiff.Sqitch (withProject, writeUnits)
import Sqiff.Sqitch.Check (Tagging (..))
import Sqiff.Sqitch.Layout (findLayout)
import Sqiff.Sqitch.Plan (SqitchPlan (..), readPlan)
import Sqiff.Sqitch.Units (Placement (..), Unit (..), UnitKind (..))
import System.Environment (lookupEnv, setEnv)
import System.Process.Typed (proc, readProcess, readProcess_, setWorkingDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))


-- | Puts SQIFF_TEST_SQITCH_BIN on PATH and gives sqitch an identity and no user
-- configuration; whether sqitch is then available.
setUpSqitch :: IO Bool
setUpSqitch = do
    lookupEnv "SQIFF_TEST_SQITCH_BIN" >>= traverse_ \bin -> do
        path <- fromMaybe "" <$> lookupEnv "PATH"
        setEnv "PATH" (bin <> ":" <> path)
    setEnv "SQITCH_USER_CONFIG" "/nonexistent/sqiff-test/sqitch.conf"
    setEnv "SQITCH_FULLNAME" "sqiff test"
    setEnv "SQITCH_EMAIL" "sqiff@example.invalid"
    isJust <$> findExecutable "sqitch"


tests :: Bool -> TestTree
tests available =
    testGroup "sqitch writing"
        $ if not available
            then [testCase "sqitch is available" (assertFailure "sqitch is neither on PATH nor in SQIFF_TEST_SQITCH_BIN")]
            else
                [ testCase "writes where --top-dir and --plan-file say, not where sqitch.conf does" customLayout
                , testCase "a failure part-way leaves the plan and the scripts as they were" rollsBack
                , testCase "a second rework on the same day gets a new tag" secondTag
                , testCase "a batch that would collide writes nothing" refusesCollisions
                ]


-- | A new sqitch project whose sqitch.conf puts everything under @db@.
withSqitchProject :: (FilePath -> IO a) -> IO a
withSqitchProject action = withSystemTempDirectory "sqiff-writing" \dir -> do
    let root = dir </> "project"
    createDirectoryIfMissing True root
    void (readProcess_ (setWorkingDir root (proc "sqitch" ["init", "proj", "--engine", "pg", "--top-dir", "db"])))
    action root


step :: Text -> Text -> Step
step name sql =
    let object = Table (qname "public" name)
     in Step CreatePhase 4 object sql Shared [] (ObjectKey Definition object) (Added (Printed sql))


unit :: UnitKind -> Text -> Text -> Unit
unit kind changeName sql = Unit{kind, changeName, note = "sqiff: test", placement = Migration, deploy = [step changeName sql], revert = []}


tagging :: Tagging
tagging = Tagging{given = Nothing, now = UTCTime (fromGregorian 2026 11 18) 0}


-- | Every file under @dir@, relative to it.
filesIn :: FilePath -> IO [FilePath]
filesIn dir = do
    (_, out, _) <- readProcess (setWorkingDir dir (proc "find" [".", "-type", "f"]))
    pure (sort (lines (T.unpack (decodeUtf8Lenient (toStrictBytes out)))))


customLayout :: Assertion
customLayout = withSqitchProject \root -> do
    createDirectoryIfMissing True (root </> "alt")
    writeFileUtf8 (root </> "alt/other.plan") "%syntax-version=1.0.0\n%project=proj\n\n"
    layout <- findLayout root (Just "alt") (Just "alt/other.plan")
    plan <- readPlan (root </> "alt/other.plan")
    withProject layout \project -> writeUnits project Nothing plan tagging [unit AddChange "one" "create table public.one ()"]
    deploy <- readFileUtf8 (root </> "alt/deploy/one.sql")
    assertBool "the deploy script has the statement" ("create table public.one ()" `T.isInfixOf` deploy)
    after <- readPlan (root </> "alt/other.plan")
    after.changes @?= ["one"]
    defaultPlan <- readPlan (root </> "db/sqitch.plan")
    defaultPlan.changes @?= []
    exists <- doesFileExist (root </> "db/deploy/one.sql")
    exists @?= False


rollsBack :: Assertion
rollsBack = withSqitchProject \root -> do
    -- sqitch cannot write the second change's deploy script into a read-only directory.
    let planFile = root </> "db/sqitch.plan"
        readOnly = root </> "db/deploy/locked"
    createDirectoryIfMissing True readOnly
    setPermissions readOnly . setOwnerWritable False =<< getPermissions readOnly
    before <- (,) <$> readFileUtf8 planFile <*> filesIn root
    layout <- findLayout root Nothing Nothing
    plan <- readPlan planFile
    result <- try (withProject layout \project -> writeUnits project Nothing plan tagging [unit AddChange "one" "create table public.one ()", unit AddChange "locked/two" "create table public.two ()"])
    case result of
        Left (UserError message) -> assertBool (T.unpack message) ("nothing was written" `T.isInfixOf` message)
        Right () -> assertFailure "writing into a read-only directory succeeded"
    after <- (,) <$> readFileUtf8 planFile <*> filesIn root
    after @?= before


secondTag :: Assertion
secondTag = withSqitchProject \root -> do
    let planFile = root </> "db/sqitch.plan"
    layout <- findLayout root Nothing Nothing
    let write units = do
            plan <- readPlan planFile
            withProject layout \project -> writeUnits project Nothing plan tagging units
    write [unit AddChange "one" "create table public.one ()"]
    write [unit ReworkChange "one" "create table public.one (a int)"]
    write [unit ReworkChange "one" "create table public.one (a int, b int)"]
    plan <- readPlan planFile
    plan.tags @?= Set.fromList ["2026-11-18_sqiff", "2026-11-18_sqiff_2"]


refusesCollisions :: Assertion
refusesCollisions = withSqitchProject \root -> do
    let planFile = root </> "db/sqitch.plan"
    before <- (,) <$> readFileUtf8 planFile <*> filesIn root
    layout <- findLayout root Nothing Nothing
    plan <- readPlan planFile
    result <-
        try
            ( withProject layout \project ->
                writeUnits project Nothing plan tagging [unit AddChange "one" "select 1", unit AddChange "one" "select 2", unit AddChange "../out" "select 3"]
            )
    case result of
        Left (UserError message) -> do
            assertBool (T.unpack message) ([i|"one" would be written 2 times|] `T.isInfixOf` message)
            assertBool (T.unpack message) ("\"../out\"" `T.isInfixOf` message)
        Right () -> assertFailure "a colliding batch was written"
    after <- (,) <$> readFileUtf8 planFile <*> filesIn root
    after @?= before
