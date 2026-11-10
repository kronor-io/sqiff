{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A scratch PostgreSQL holding the schema a revision's sqitch plan produces. It runs
-- the repository's own @postgres@ service from a jj workspace at that revision, in a
-- Docker Compose project of its own.
module Kronor.SchemaDiff.Baseline (ensureBaseline) where

import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Kronor.SchemaDiff.Catalog (failWith)
import RIO
import RIO.Directory (XdgDirectory (..), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getXdgDirectory, removeFile)
import RIO.FilePath (takeDirectory, (</>))
import RIO.Text qualified as T
import System.Environment (getEnvironment)
import System.Process.Typed (ProcessConfig, nullStream, proc, readProcess, runProcess, setEnv, setStdin, setStdout, setWorkingDir, useHandleOpen)


composeProject :: Text
composeProject = "schema-diff-baseline"


workspaceName :: String
workspaceName = "schema-diff-baseline"


port :: Int
port = 5542


connInfo :: Text
connInfo = [i|host=localhost port=#{port} user=kronor dbname=kronor|]


data Paths = Paths
    { checkout :: FilePath
    , stateFile :: FilePath
    }


-- | Makes sure the baseline database holds the schema of @rev@'s sqitch plan and
-- returns its connection string.
ensureBaseline :: Text -> IO Text
ensureBaseline rev = do
    for_ ["docker", "jj"] \tool -> do
        found <- findExecutable tool
        when (isNothing found) $ failWith [i|#{tool} is not on PATH; the baseline database needs it|]
    cache <- getXdgDirectory XdgCache "schema-diff"
    let p = Paths{checkout = cache </> "checkout", stateFile = cache </> "baseline-commit"}
    commit <- resolve rev
    recorded <- doesFileExist p.stateFile >>= bool (pure Nothing) (Just . T.strip <$> readFileUtf8 p.stateFile)
    hasCheckout <- doesDirectoryExist (p.checkout </> ".jj")
    running <- if hasCheckout then isRunning p else pure False
    if recorded == Just commit && running
        then pure connInfo
        else do
            note [i|bringing the baseline database to #{T.take 12 commit} (#{rev})|]
            whenM (doesFileExist p.stateFile) $ removeFile p.stateFile
            checkoutAt p commit
            start p
            stale <- staleDeploy p
            when stale do
                note "the deployed changes are not all in this revision's plan; rebuilding from scratch"
                compose p ["down", "-v"]
                start p
            hasStatecharts <- doesFileExist (p.checkout </> "statecharts" </> "sqitch.plan")
            when hasStatecharts $ sqitchIn p ["--chdir", "statecharts", "deploy"]
            sqitchIn p ["deploy", "--mode", "change", "kronor"]
            sqitchIn p ["deploy", "--mode", "change", "dev"]
            writeFileUtf8 p.stateFile (commit <> "\n")
            pure connInfo


note :: Text -> IO ()
note message = TIO.hPutStrLn stderr ("schema-diff: " <> message)


resolve :: Text -> IO Text
resolve rev = do
    (code, out, err) <- readProcess (quiet (proc "jj" ["--ignore-working-copy", "log", "--no-graph", "--limit", "2", "-r", T.unpack rev, "-T", "commit_id ++ \"\\n\""]))
    case (code, T.lines (decode out)) of
        (ExitSuccess, [commit]) -> pure commit
        (ExitSuccess, []) -> failWith [i|#{rev} names no commit|]
        (ExitSuccess, _) -> failWith [i|#{rev} names more than one commit|]
        (ExitFailure _, _) -> failWith [i|jj log -r #{rev}: #{decode err}|]


-- | Creates the jj workspace at @commit@, or moves it there.
checkoutAt :: Paths -> Text -> IO ()
checkoutAt p commit =
    doesDirectoryExist (p.checkout </> ".jj") >>= \case
        True -> do
            void $ readProcess (quiet (setWorkingDir p.checkout (proc "jj" ["workspace", "update-stale"])))
            (_, out, _) <- readProcess (quiet (setWorkingDir p.checkout (proc "jj" ["log", "--no-graph", "-r", "@-", "-T", "commit_id"])))
            unless (decode out == commit) $ jj (Just p.checkout) ["new", T.unpack commit]
        False -> do
            createDirectoryIfMissing True (takeDirectory p.checkout)
            void $ readProcess (quiet (proc "jj" ["workspace", "forget", workspaceName]))
            jj Nothing ["workspace", "add", "--name", workspaceName, "-r", T.unpack commit, p.checkout]


jj :: Maybe FilePath -> [String] -> IO ()
jj dir args = do
    code <- runProcess (toStderr (maybe id setWorkingDir dir (proc "jj" args)))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [i|jj #{unwords args} failed with exit code #{n}|]


-- | Starts the server and waits until it accepts connections over TCP, which it does
-- once the init scripts are done.
start :: Paths -> IO ()
start p = do
    compose p ["up", "-d", "postgres"]
    awaitReady (0 :: Int)
  where
    awaitReady n = do
        (code, _, _) <- readCompose p ["exec", "-T", "postgres", "pg_isready", "-q", "-h", "127.0.0.1"]
        unless (code == ExitSuccess) do
            (_, state, _) <- readCompose p ["ps", "-a", "--format", "{{.State}}", "postgres"]
            when (T.strip state `elem` ["exited", "dead"] || n >= 900)
                $ failWith [i|the baseline server did not come up; see docker compose -p #{composeProject} logs postgres|]
            when (n == 5) $ note [i|waiting for the baseline server (docker compose -p #{composeProject} logs -f postgres)|]
            threadDelay 2_000_000
            awaitReady (n + 1)


-- | Whether sqitch reports deployed changes that the checkout's plan does not have.
staleDeploy :: Paths -> IO Bool
staleDeploy p = do
    (_, out, err) <- readCompose p ["exec", "-T", "postgres", "sqitch", "status"]
    pure ("Cannot find" `T.isInfixOf` (out <> err))


sqitchIn :: Paths -> [String] -> IO ()
sqitchIn p args = do
    env <- composeEnv
    code <- runProcess (toStderr (setEnv env (composeProc p (["exec", "-T", "postgres", "sqitch"] <> args))))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [i|sqitch #{unwords args} failed in the baseline container with exit code #{n}|]


isRunning :: Paths -> IO Bool
isRunning p = do
    (code, out, _) <- readCompose p ["ps", "--status", "running", "-q", "postgres"]
    pure (code == ExitSuccess && not (T.null (T.strip out)))


compose :: Paths -> [String] -> IO ()
compose p args = do
    env <- composeEnv
    code <- runProcess (toStderr (setEnv env (composeProc p args)))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [i|docker compose -p #{composeProject} #{unwords args} failed with exit code #{n}|]


readCompose :: Paths -> [String] -> IO (ExitCode, Text, Text)
readCompose p args = do
    env <- composeEnv
    (code, out, err) <- readProcess (quiet (setEnv env (composeProc p args)))
    pure (code, decode out, decode err)


-- | The checkout's own compose file, so image, command, init scripts and the @/repo@
-- mount all come from the revision being built.
composeProc :: Paths -> [String] -> ProcessConfig () () ()
composeProc p args = proc "docker" (["compose", "-p", T.unpack composeProject, "-f", p.checkout </> "docker-compose.yaml"] <> args)


-- | The caller's environment with the baseline's port and without a dev slot's compose
-- settings. pg_partman's background worker is off, as in CI, so its maintenance does not
-- take locks that the deploy then times out on.
composeEnv :: IO [(String, String)]
composeEnv = do
    env <- getEnvironment
    let ours = [("KRONOR_PG_PORT", show port), ("KRONOR_PG_PARTMAN_BGW_DBNAME", "")]
        dropped = map fst ours <> ["COMPOSE_FILE", "COMPOSE_PROJECT_NAME", "COMPOSE_PROFILES"]
    pure (ours <> filter ((`notElem` dropped) . fst) env)


quiet :: ProcessConfig i o e -> ProcessConfig () o e
quiet = setStdin nullStream


toStderr :: ProcessConfig i o e -> ProcessConfig () () e
toStderr = setStdout (useHandleOpen stderr) . quiet


decode :: LByteString -> Text
decode = T.strip . decodeUtf8Lenient . toStrictBytes
