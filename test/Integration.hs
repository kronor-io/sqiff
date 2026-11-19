{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | End-to-end tests against a throwaway PostgreSQL cluster, one per directory under
-- @test/fixtures@:
--
-- * @from.sql@ and @to.sql@ build the two databases;
-- * @diff.golden@ and @plan.golden@ hold what @sqiff diff@ and @sqiff plan@ print, as a
--   record of the current behaviour (refresh with @--accept@);
-- * the round trip applies the plan to @from@, statement by statement, and expects
--   @sqiff diff@ to find nothing left;
-- * the sqitch round trip writes the plan as sqitch changes into a new project, has
--   sqitch deploy and verify them on a fresh copy of @from@, and expects the same;
-- * after its round trip, each migration is reverted, deployed again and reverted again,
--   last first, and reverting them all must give back @from@;
-- * a @known-bug@ file marks a fixture whose round trips fail today, and says why;
-- * a @revert-gap@ file marks one whose changes PostgreSQL cannot fully revert, and why.
module Integration (tests) where

import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Hasql.TH (singletonStatement)
import Postgres
import RIO
import RIO.ByteString.Lazy qualified as BL
import RIO.Char (isAlphaNum)
import RIO.Directory (doesFileExist, listDirectory)
import RIO.FilePath ((<.>), (</>))
import RIO.List (sort)
import RIO.Text qualified as T
import Sqiff.Connection (withConnections, withSharedSnapshot)
import Sqiff.ConnectionString (connectionString)
import Sqiff.Script (splitStatements)
import Sqiff.Sqitch.Plan (SqitchPlan (..), readPlan)
import System.Environment (lookupEnv, setEnv)
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))


fixturesDir :: FilePath
fixturesDir = "test/fixtures"


tests :: Bool -> IO TestTree
tests sqitchFound = do
    names <- sort <$> listDirectory fixturesDir
    fixtures <- traverse readFixture names
    findBinaries >>= \case
        Left why -> pure (testGroup "integration" [testCase "PostgreSQL is available" (assertFailure (T.unpack why))])
        Right bin -> do
            -- sqitch runs psql from PATH.
            path <- fromMaybe "" <$> lookupEnv "PATH"
            setEnv "PATH" [i|#{bin}:#{path}|]
            pure $ withResource (startCluster bin) stopCluster \getCluster ->
                let shared = Shared{getCluster, sqitchFound}
                 in testGroup "integration" (sharedSnapshot getCluster : deepViewChains getCluster : map (fixture shared) fixtures)


-- | Connections that share a snapshot do not see what another commits after it.
sharedSnapshot :: IO Cluster -> TestTree
sharedSnapshot getCluster = testCase "connections in a shared snapshot see one state" do
    cluster <- getCluster
    createDatabase cluster "snapshot"
    let db = connectionString (conninfo cluster "snapshot")
        tableCount c = Connection.use c (Session.statement () [singletonStatement| select count(*)::int8 from pg_class where relname = 'late' |]) >>= either (assertFailure . show) pure
    withConnections db 2 \conns@(a :| bs) -> do
        seen <- withSharedSnapshot conns do
            runScript cluster "snapshot" "create table late ()" >>= either (assertFailure . T.unpack) pure
            (,) <$> tableCount a <*> traverse tableCount bs
        seen @?= (0, [0])
        after <- tableCount a
        after @?= 1


-- | Finding what depends on a retyped column takes time in proportion to what it
-- reaches, not to the size of the catalog at every step: three chains of 100 nested
-- views over 300 tables used to take several seconds.
deepViewChains :: IO Cluster -> TestTree
deepViewChains getCluster = testCase "deep view chains are planned in reasonable time" do
    cluster <- getCluster
    for_ [("deep_from", "int"), ("deep_to", "bigint")] \(db, retyped) -> do
        createDatabase cluster db
        runScript cluster db (schema retyped) >>= either (assertFailure . T.unpack) pure
    started <- getMonotonicTime
    (code, out, err) <- runSqiff cluster ["plan"] "deep_from" "deep_to"
    elapsed <- subtract started <$> getMonotonicTime
    assertBool (T.unpack err) (code == ExitSuccess)
    length (filter ("drop view" `T.isPrefixOf`) (T.lines out)) @?= chains * depth
    assertBool [i|sqiff plan took #{elapsed} s|] (elapsed < 3)
  where
    chains, depth, tables :: Int
    chains = 3
    depth = 100
    tables = 300
    schema :: Text -> Text
    schema retyped =
        T.unlines
            $ [ [i|create table t#{n} (id int primary key, a #{if n < chains then retyped else "int"}, b text#{reference n});|]
              | n <- [0 .. tables - 1]
              ]
            <> [ [i|create view c#{c}_v#{j} as #{viewBody c j};|]
               | c <- [0 .. chains - 1]
               , j <- [0 .. depth - 1]
               ]
    reference :: Int -> Text
    reference n = if n == 0 then "" else [i|, p int references t#{n - 1}(id)|]
    viewBody :: Int -> Int -> Text
    viewBody c j
        | j == 0 = [i|select id, a, b from t#{c}|]
        | otherwise = [i|select v.id, v.a, v.b from c#{c}_v#{j - 1} v join t#{chains + (c * depth + j) `mod` (tables - chains)} t on t.id = v.id|]


-- | Runs sqiff's @command@ from @fromDb@ to @toDb@, outside the repository so no
-- sqiff.yaml is read.
-- [allow:SqiffNamedTuples] pre-existing at adoption
runSqiff :: Cluster -> [String] -> Text -> Text -> IO (ExitCode, Text, Text)
runSqiff cluster command fromDb toDb = do
    exe <- fromMaybe "sqiff" <$> lookupEnv "SQIFF_EXE"
    let args = command <> ["--from", T.unpack (conninfo cluster fromDb), "--to", T.unpack (conninfo cluster toDb)]
    (code, out, err) <- readProcess (setWorkingDir "/" (proc exe args))
    pure (code, decodeOutput out, decodeOutput err)


-- | What a process printed, as text.
decodeOutput :: BL.ByteString -> Text
decodeOutput = decodeUtf8Lenient . BL.toStrict


-- | What every fixture's tests share.
data Shared = Shared
    { getCluster :: IO Cluster
    , sqitchFound :: Bool
    -- ^ Whether sqitch is available: without it, every sqitch round trip fails.
    }


-- | One directory under @test/fixtures@, with the reasons its marker files give.
data Fixture = Fixture
    { name :: FilePath
    , knownBug :: Maybe Text
    -- ^ Why its round trips fail today, so they are expected to.
    , revertGap :: Maybe Text
    -- ^ Why PostgreSQL cannot fully revert its changes, so they are not reverted.
    }


-- | The fixture in the directory @name@.
readFixture :: FilePath -> IO Fixture
readFixture name = do
    knownBug <- reasonIn name "known-bug"
    revertGap <- reasonIn name "revert-gap"
    pure Fixture{name, knownBug, revertGap}


-- | The reason in one of a fixture's marker files, if it has it.
reasonIn :: FilePath -> FilePath -> IO (Maybe Text)
reasonIn name file = do
    let path = fixturesDir </> name </> file
    doesFileExist path >>= \case
        True -> Just . T.strip <$> readFileUtf8 path
        False -> pure Nothing


-- | Where the fixture's files are.
fixtureDir :: Fixture -> FilePath
fixtureDir f = fixturesDir </> f.name


-- | The databases of one fixture's tests, named after it so fixtures do not collide.
data Databases = Databases
    { from :: Text
    -- ^ Built from @from.sql@; the round trip applies the plan to it.
    , to :: Text
    -- ^ Built from @to.sql@.
    , sqitch :: Text
    -- ^ A fresh copy of @from@ for sqitch to deploy to.
    , original :: Text
    -- ^ Another copy of @from@, to compare with once sqitch has reverted every change.
    }


-- | The fixture's databases.
fixtureDatabases :: Fixture -> Databases
fixtureDatabases f =
    Databases
        { from = base <> "_from"
        , to = base <> "_to"
        , sqitch = base <> "_sqitch"
        , original = base <> "_original"
        }
  where
    base = "f_" <> T.map (\c -> if isAlphaNum c then c else '_') (T.pack f.name)


-- | What sqiff printed for one fixture before the round trip changed @from@.
data Before = Before
    { diffOut :: Text
    , planOut :: Text
    }


-- | The fixture's goldens and round trips, which share one run of 'prepare'.
fixture :: Shared -> Fixture -> TestTree
fixture shared f = withResource (prepare shared f) (const (pure ())) \getBefore ->
    testGroup
        f.name
        [ golden f "diff" (.diffOut) getBefore
        , golden f "plan" (.planOut) getBefore
        , withKnownBug f (roundTrip shared f getBefore)
        , withKnownBug f (sqitchRoundTrip shared f getBefore)
        ]


-- | Expects @test@ to fail while the fixture has a known bug.
withKnownBug :: Fixture -> TestTree -> TestTree
withKnownBug f test = maybe test (\why -> expectFailBecause (T.unpack why) test) f.knownBug


-- | Builds the fixture's @from@ and @to@, and records what sqiff prints for them.
prepare :: Shared -> Fixture -> IO Before
prepare shared f = do
    cluster <- shared.getCluster
    let databases = fixtureDatabases f
    for_ [(databases.from, "from.sql"), (databases.to, "to.sql")] \(db, file) -> do
        createDatabase cluster db
        sql <- readFileUtf8 (fixtureDir f </> file)
        runScript cluster db sql >>= either (\e -> throwString [i|#{file}: #{e}|]) pure
    (_, diffOut, _) <- runSqiff cluster ["diff"] databases.from databases.to
    (planCode, planOut, planErr) <- runSqiff cluster ["plan"] databases.from databases.to
    unless (planCode == ExitSuccess) $ throwString [i|sqiff plan failed: #{planErr}|]
    pure Before{diffOut, planOut}


-- | Compares one of sqiff's outputs with the fixture's @label.golden@.
golden :: Fixture -> String -> (Before -> Text) -> IO Before -> TestTree
golden f label field getBefore =
    goldenVsStringDiff label (\ref new -> ["diff", "-u", ref, new]) (fixtureDir f </> label <.> "golden")
        $ BL.fromStrict
        . encodeUtf8
        . field
        <$> getBefore


-- | The plan, applied to @from@ statement by statement, leaves nothing for diff to find.
roundTrip :: Shared -> Fixture -> IO Before -> TestTree
roundTrip shared f getBefore = testCase "round trip" do
    before <- getBefore
    cluster <- shared.getCluster
    let databases = fixtureDatabases f
    statements <- either (assertFailure . T.unpack) pure (splitStatements before.planOut)
    failures <- fmap catMaybes . for statements $ \s ->
        runScript cluster databases.from s <&> either (\e -> Just [i|#{T.strip s}\n  => #{e}|]) (const Nothing)
    unless (null failures)
        $ assertFailure (T.unpack (T.unlines ("the plan did not apply:" : failures)))
    (code, out, err) <- runSqiff cluster ["diff"] databases.from databases.to
    unless (code == ExitSuccess)
        $ assertFailure (T.unpack [i|after the plan, diff still finds:\n#{out}#{err}|])


-- | The plan written as sqitch changes, deployed and verified by sqitch on a fresh copy
-- of from, leaves nothing for diff to find.
sqitchRoundTrip :: Shared -> Fixture -> IO Before -> TestTree
sqitchRoundTrip shared f getBefore = testCase "sqitch round trip" do
    unless shared.sqitchFound $ assertFailure "sqitch is neither on PATH nor in SQIFF_TEST_SQITCH_BIN"
    before <- getBefore
    cluster <- shared.getCluster
    let databases = fixtureDatabases f
    createDatabase cluster databases.sqitch
    fromSql <- readFileUtf8 (fixtureDir f </> "from.sql")
    runScript cluster databases.sqitch fromSql >>= either (assertFailure . T.unpack) pure
    withSystemTempDirectory "sqiff-sqitch-round-trip" \project -> do
        let env = SqitchProject{cluster, databases, fromSql, project}
        runSqitch env "sqitch init" ["init", "proj", "--engine", "pg"]
        writeChanges env
        -- Nothing to write leaves sqitch an empty plan, which it refuses to deploy.
        unless (T.null (T.strip before.planOut)) do
            assertDeployedMatches env
            -- Each migration's revert returns to the state after the one before: it can
            -- be deployed again from there, and reverting them all gives back from.
            changes <- (.changes) <$> readPlan (project </> "sqitch.plan")
            manual <- hasManualReverts project changes
            unless (manual || isJust f.revertGap) do
                exerciseReverts env changes
                assertFullyReverted env


-- | Where a fixture's sqitch round trip runs: the project sqiff writes the plan into,
-- and the databases sqitch deploys it to.
data SqitchProject = SqitchProject
    { cluster :: Cluster
    , databases :: Databases
    , fromSql :: Text
    -- ^ The fixture's @from.sql@.
    , project :: FilePath
    }


-- | Runs sqitch in the project; if it fails, so does the test, with what it printed.
runSqitch :: SqitchProject -> Text -> [String] -> Assertion
runSqitch env what args = do
    (code, out, err) <- readProcess (setWorkingDir env.project (proc "sqitch" args))
    unless (code == ExitSuccess)
        $ assertFailure (T.unpack [i|#{what} failed:\n#{decodeOutput out}#{decodeOutput err}|])


-- | The sqitch target of the database sqitch deploys to.
target :: SqitchProject -> String
target env = T.unpack (sqitchTarget env.cluster env.databases.sqitch)


-- | Has sqiff write the plan from sqitch's copy of @from@ to @to@ into the project.
writeChanges :: SqitchProject -> Assertion
writeChanges env = do
    (code, out, err) <- runSqiff env.cluster ["sqitch", "--project", env.project] env.databases.sqitch env.databases.to
    unless (code == ExitSuccess) $ assertFailure (T.unpack [i|sqiff sqitch failed:\n#{out}#{err}|])


-- | sqitch deploys and verifies every change, after which diff finds nothing left.
assertDeployedMatches :: SqitchProject -> Assertion
assertDeployedMatches env = do
    runSqitch env "sqitch deploy" ["deploy", "--verify", target env]
    runSqitch env "sqitch verify" ["verify", target env]
    (code, out, err) <- runSqiff env.cluster ["diff", "--exclude-schema", "sqitch"] env.databases.sqitch env.databases.to
    unless (code == ExitSuccess)
        $ assertFailure (T.unpack [i|after sqitch deploy, diff still finds:\n#{out}#{err}|])


-- | Whether the revert of one of the project's @changes@ stops at a step sqiff cannot
-- write, which is left to a person.
hasManualReverts :: FilePath -> [Text] -> IO Bool
hasManualReverts project changes = do
    reverts <- for changes \name -> readFileUtf8 (project </> "revert" </> T.unpack name <> ".sql")
    pure (any ("sqiff could not write this step" `T.isInfixOf`) reverts)


-- | Reverts each of the deployed @changes@, last first, deploys it again and reverts it
-- again.
exerciseReverts :: SqitchProject -> [Text] -> Assertion
exerciseReverts env changes = for_ (reverse (zip [0 ..] changes)) \(k, name) -> do
    runSqitch env [i|sqitch revert of #{name}|] (["revert", "-y"] <> revertTo k <> [target env])
    runSqitch env [i|sqitch deploy of #{name} after its revert|] ["deploy", "--verify", "--to", T.unpack name, target env]
    runSqitch env [i|sqitch revert of #{name} again|] (["revert", "-y"] <> revertTo k <> [target env])
  where
    -- Reverting the @k@th change goes back to the one before it, or to nothing.
    revertTo :: Int -> [String]
    revertTo k = case drop (k - 1) changes of
        previous : _ | k > 0 -> ["--to", T.unpack previous]
        _ -> []


-- | With every change reverted, diff finds nothing between sqitch's database and a fresh
-- copy of @from@.
assertFullyReverted :: SqitchProject -> Assertion
assertFullyReverted env = do
    createDatabase env.cluster env.databases.original
    runScript env.cluster env.databases.original env.fromSql >>= either (assertFailure . T.unpack) pure
    (code, out, err) <- runSqiff env.cluster ["diff", "--exclude-schema", "sqitch"] env.databases.sqitch env.databases.original
    unless (code == ExitSuccess)
        $ assertFailure (T.unpack [i|after reverting every change, diff with from finds:\n#{out}#{err}|])
