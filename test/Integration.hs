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
-- * a @known-bug@ file marks a fixture whose round trip fails today, and says why.
module Integration (tests) where

import Data.String.Interpolate (i)
import RIO
import RIO.ByteString.Lazy qualified as BL
import RIO.Char (isAlphaNum)
import RIO.Directory (doesFileExist, listDirectory)
import RIO.FilePath ((<.>), (</>))
import RIO.List (sort)
import RIO.Text qualified as T
import Postgres
import Sqiff.Script (splitStatements)
import System.Environment (lookupEnv)
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (assertFailure, testCase)


fixturesDir :: FilePath
fixturesDir = "test/fixtures"


tests :: IO TestTree
tests = do
    names <- sort <$> listDirectory fixturesDir
    fixtures <- for names \name -> (name,) <$> knownBug name
    findBinaries <&> \case
        Left why -> testGroup "integration" [testCase "PostgreSQL is available" (assertFailure (T.unpack why))]
        Right bin ->
            withResource (startCluster bin) stopCluster \cluster ->
                testGroup "integration" (map (uncurry (fixture cluster)) fixtures)


-- | The reason in a fixture's @known-bug@ file, if it has one.
knownBug :: FilePath -> IO (Maybe Text)
knownBug name = do
    let path = fixturesDir </> name </> "known-bug"
    doesFileExist path >>= \case
        True -> Just . T.strip <$> readFileUtf8 path
        False -> pure Nothing


-- | What sqiff printed for one fixture before the round trip changed @from@.
data Before = Before
    { diffOut :: Text
    , planOut :: Text
    }


fixture :: IO Cluster -> FilePath -> Maybe Text -> TestTree
fixture getCluster name bug = withResource prepare (const (pure ())) \getBefore ->
    testGroup
        name
        [ golden "diff" (.diffOut) getBefore
        , golden "plan" (.planOut) getBefore
        , roundTrip getBefore
        ]
  where
    dir = fixturesDir </> name
    base = "f_" <> T.map (\c -> if isAlphaNum c then c else '_') (T.pack name)
    fromDb = base <> "_from"
    toDb = base <> "_to"

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

    sqiff cluster command = do
        exe <- fromMaybe "sqiff" <$> lookupEnv "SQIFF_EXE"
        let args = command <> ["--from", T.unpack (conninfo cluster fromDb), "--to", T.unpack (conninfo cluster toDb)]
        -- Run outside the repository, so no sqiff.yaml is read.
        (code, out, err) <- readProcess (setWorkingDir "/" (proc exe args))
        pure (code, decode out, decode err)

    decode = decodeUtf8Lenient . BL.toStrict

    golden label field getBefore =
        goldenVsStringDiff label (\ref new -> ["diff", "-u", ref, new]) (dir </> label <.> "golden") $
            BL.fromStrict . encodeUtf8 . field <$> getBefore

    roundTrip getBefore = withKnownBug $ testCase "round trip" do
        before <- getBefore
        cluster <- getCluster
        statements <- either (assertFailure . T.unpack) pure (splitStatements before.planOut)
        failures <- fmap catMaybes . for statements $ \s ->
            runScript cluster fromDb s <&> either (\e -> Just [i|#{T.strip s}\n  => #{e}|]) (const Nothing)
        unless (null failures) $
            assertFailure (T.unpack (T.unlines ("the plan did not apply:" : failures)))
        (code, out, err) <- sqiff cluster ["diff"]
        unless (code == ExitSuccess) $
            assertFailure (T.unpack [i|after the plan, diff still finds:\n#{out}#{err}|])

    withKnownBug test = maybe test (\why -> expectFailBecause (T.unpack why) test) bug


