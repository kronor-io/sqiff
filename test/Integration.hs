{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))


fixturesDir :: FilePath
fixturesDir = "test/fixtures"


tests :: Bool -> IO TestTree
tests sqitch = do
    names <- sort <$> listDirectory fixturesDir
    fixtures <- for names \name -> (name,) <$> ((,) <$> reasonIn name "known-bug" <*> reasonIn name "revert-gap")
    findBinaries >>= \case
        Left why -> pure (testGroup "integration" [testCase "PostgreSQL is available" (assertFailure (T.unpack why))])
        Right bin -> do
            -- sqitch runs psql from PATH.
            path <- fromMaybe "" <$> lookupEnv "PATH"
            setEnv "PATH" (bin <> ":" <> path)
            pure $ withResource (startCluster bin) stopCluster \cluster ->
                testGroup "integration" (sharedSnapshot cluster : map (uncurry (fixture sqitch cluster)) fixtures)


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


-- | The reason in one of a fixture's marker files, if it has it.
reasonIn :: FilePath -> FilePath -> IO (Maybe Text)
reasonIn name file = do
    let path = fixturesDir </> name </> file
    doesFileExist path >>= \case
        True -> Just . T.strip <$> readFileUtf8 path
        False -> pure Nothing


-- | What sqiff printed for one fixture before the round trip changed @from@.
data Before = Before
    { diffOut :: Text
    , planOut :: Text
    }


fixture :: Bool -> IO Cluster -> FilePath -> (Maybe Text, Maybe Text) -> TestTree
fixture sqitch getCluster name (bug, revertGap) = withResource prepare (const (pure ())) \getBefore ->
    testGroup
        name
        [ golden "diff" (.diffOut) getBefore
        , golden "plan" (.planOut) getBefore
        , roundTrip getBefore
        , sqitchRoundTrip getBefore
        ]
  where
    dir = fixturesDir </> name
    base = "f_" <> T.map (\c -> if isAlphaNum c then c else '_') (T.pack name)
    fromDb = base <> "_from"
    toDb = base <> "_to"
    sqitchDb = base <> "_sqitch"
    originalDb = base <> "_original"

    prepare = do
        cluster <- getCluster
        for_ [(fromDb, "from.sql"), (toDb, "to.sql")] \(db, file) -> do
            createDatabase cluster db
            sql <- readFileUtf8 (dir </> file)
            runScript cluster db sql >>= either (\e -> throwString [i|#{file}: #{e}|]) pure
        (_, diffOut, _) <- sqiff cluster ["diff"]
        (planCode, planOut, planErr) <- sqiff cluster ["plan"]
        unless (planCode == ExitSuccess) $ throwString [i|sqiff plan failed: #{planErr}|]
        pure Before{diffOut, planOut}

    sqiff cluster command = sqiffFrom cluster fromDb command

    sqiffFrom cluster from = sqiffBetween cluster from toDb

    sqiffBetween cluster from to command = do
        exe <- fromMaybe "sqiff" <$> lookupEnv "SQIFF_EXE"
        let args = command <> ["--from", T.unpack (conninfo cluster from), "--to", T.unpack (conninfo cluster to)]
        -- Run outside the repository, so no sqiff.yaml is read.
        (code, out, err) <- readProcess (setWorkingDir "/" (proc exe args))
        pure (code, decode out, decode err)

    decode = decodeUtf8Lenient . BL.toStrict

    golden label field getBefore =
        goldenVsStringDiff label (\ref new -> ["diff", "-u", ref, new]) (dir </> label <.> "golden")
            $ BL.fromStrict
            . encodeUtf8
            . field
            <$> getBefore

    roundTrip getBefore = withKnownBug $ testCase "round trip" do
        before <- getBefore
        cluster <- getCluster
        statements <- either (assertFailure . T.unpack) pure (splitStatements before.planOut)
        failures <- fmap catMaybes . for statements $ \s ->
            runScript cluster fromDb s <&> either (\e -> Just [i|#{T.strip s}\n  => #{e}|]) (const Nothing)
        unless (null failures)
            $ assertFailure (T.unpack (T.unlines ("the plan did not apply:" : failures)))
        (code, out, err) <- sqiff cluster ["diff"]
        unless (code == ExitSuccess)
            $ assertFailure (T.unpack [i|after the plan, diff still finds:\n#{out}#{err}|])

    -- The plan written as sqitch changes, deployed and verified by sqitch on a fresh copy
    -- of from, leaves nothing for diff to find.
    sqitchRoundTrip getBefore = withKnownBug $ testCase "sqitch round trip" do
        unless sqitch $ assertFailure "sqitch is neither on PATH nor in SQIFF_TEST_SQITCH_BIN"
        before <- getBefore
        cluster <- getCluster
        createDatabase cluster sqitchDb
        fromSql <- readFileUtf8 (dir </> "from.sql")
        runScript cluster sqitchDb fromSql >>= either (assertFailure . T.unpack) pure
        withSystemTempDirectory "sqiff-sqitch-round-trip" \project -> do
            let run :: Text -> FilePath -> [String] -> IO ()
                run what program args = do
                    (code, out, err) <- readProcess (setWorkingDir project (proc program args))
                    unless (code == ExitSuccess)
                        $ assertFailure (T.unpack [i|#{what} failed:\n#{decode out}#{decode err}|])
                target = T.unpack (sqitchTarget cluster sqitchDb)
            run "sqitch init" "sqitch" ["init", "proj", "--engine", "pg"]
            (code, out, err) <- sqiffFrom cluster sqitchDb ["sqitch", "--project", project]
            unless (code == ExitSuccess) $ assertFailure (T.unpack [i|sqiff sqitch failed:\n#{out}#{err}|])
            -- Nothing to write leaves sqitch an empty plan, which it refuses to deploy.
            unless (T.null (T.strip before.planOut)) do
                run "sqitch deploy" "sqitch" ["deploy", "--verify", target]
                run "sqitch verify" "sqitch" ["verify", target]
                (code, out, err) <- sqiffFrom cluster sqitchDb ["diff", "--exclude-schema", "sqitch"]
                unless (code == ExitSuccess)
                    $ assertFailure (T.unpack [i|after sqitch deploy, diff still finds:\n#{out}#{err}|])
                -- Each migration's revert returns to the state after the one before: it can
                -- be deployed again from there, and reverting them all gives back from.
                changes <- (.changes) <$> readPlan (project </> "sqitch.plan")
                -- A revert that stops at a step sqiff cannot write is left to a person.
                reverts <- for changes \name -> readFileUtf8 (project </> "revert" </> T.unpack name <> ".sql")
                let byHand = any ("sqiff could not write this step" `T.isInfixOf`) reverts || isJust revertGap
                let revertTo k = case drop (k - 1) changes of
                        previous : _ | k > 0 -> ["--to", T.unpack previous]
                        _ -> []
                unless byHand $ for_ (reverse (zip [0 :: Int ..] changes)) \(k, name) -> do
                    run [i|sqitch revert of #{name}|] "sqitch" (["revert", "-y"] <> revertTo k <> [target])
                    run [i|sqitch deploy of #{name} after its revert|] "sqitch" ["deploy", "--verify", "--to", T.unpack name, target]
                    run [i|sqitch revert of #{name} again|] "sqitch" (["revert", "-y"] <> revertTo k <> [target])
            unless (T.null (T.strip before.planOut)) do
                byHand <- (isJust revertGap ||) . any ("sqiff could not write this step" `T.isInfixOf`) <$> (traverse (\n -> readFileUtf8 (project </> "revert" </> T.unpack n <> ".sql")) . (.changes) =<< readPlan (project </> "sqitch.plan"))
                unless byHand do
                    createDatabase cluster originalDb
                    runScript cluster originalDb fromSql >>= either (assertFailure . T.unpack) pure
                    (code, out, err) <- sqiffBetween cluster sqitchDb originalDb ["diff", "--exclude-schema", "sqitch"]
                    unless (code == ExitSuccess)
                        $ assertFailure (T.unpack [i|after reverting every change, diff with from finds:\n#{out}#{err}|])

    withKnownBug test = maybe test (\why -> expectFailBecause (T.unpack why) test) bug
