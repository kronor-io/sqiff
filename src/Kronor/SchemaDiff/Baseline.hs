{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A scratch PostgreSQL holding the schema a revision's sqitch plan produces, run from a
-- git worktree at that revision.
module Kronor.SchemaDiff.Baseline (ensureBaseline) where

import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Kronor.SchemaDiff.Catalog (failWith)
import Kronor.SchemaDiff.Git (Repo, findRepo, git, resolveCommit)
import RIO
import RIO.Directory (XdgDirectory (..), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, findExecutable, getXdgDirectory, removeDirectoryRecursive, removeFile)
import RIO.FilePath (takeDirectory, (</>))
import RIO.Text qualified as T
import System.Environment (getEnvironment)
import System.Process.Typed (ProcessConfig, nullStream, proc, readProcess, runProcess, setEnv, setStdin, setStdout, useHandleOpen)


composeProject :: Text
composeProject = "schema-diff-baseline"


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
    for_ ["docker", "git"] \tool -> do
        found <- findExecutable tool
        when (isNothing found) $ failWith [i|#{tool} is not on PATH; the baseline database needs it|]
    cache <- getXdgDirectory XdgCache "schema-diff"
    let p = Paths{checkout = cache </> "checkout", stateFile = cache </> "baseline-commit"}
    repo <- findRepo "."
    commit <- resolveCommit repo rev
    recorded <- doesFileExist p.stateFile >>= bool (pure Nothing) (Just . T.strip <$> readFileUtf8 p.stateFile)
    hasCheckout <- doesPathExist (p.checkout </> ".git")
    running <- if hasCheckout then isRunning p else pure False
    if recorded == Just commit && running
        then pure connInfo
        else do
            note [i|bringing the baseline database to #{T.take 12 commit} (#{rev})|]
            whenM (doesFileExist p.stateFile) $ removeFile p.stateFile
            checkoutAt repo p commit
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


-- | Creates the git worktree at @commit@, or moves it there. A checkout that is not a
-- git worktree is stopped and replaced; the database volume stays.
checkoutAt :: Repo -> Paths -> Text -> IO ()
checkoutAt repo p commit =
    doesPathExist (p.checkout </> ".git") >>= \case
        True -> do
            let args = ["-C", p.checkout, "checkout", "--quiet", "--force", "--detach", T.unpack commit]
            code <- runProcess (toStderr (proc "git" args))
            unless (code == ExitSuccess) $ failWith [i|git #{unwords args} failed|]
        False -> do
            let checkout = p.checkout
            whenM (doesDirectoryExist checkout) do
                note [i|replacing #{checkout} with a git worktree|]
                whenM (doesFileExist (p.checkout </> "docker-compose.yaml")) $ compose p ["down"]
                removeDirectoryRecursive p.checkout
            createDirectoryIfMissing True (takeDirectory p.checkout)
            git repo ["worktree", "prune"]
            git repo ["worktree", "add", "--quiet", "--detach", p.checkout, T.unpack commit]


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


-- | The caller's environment with the baseline's port, without a dev slot's compose
-- settings, and with pg_partman's background worker off.
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
