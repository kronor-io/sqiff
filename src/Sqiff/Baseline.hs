{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A scratch PostgreSQL holding the schema a revision's sqitch plan produces, run from a
-- git worktree at that revision with the revision's own compose file.
module Sqiff.Baseline (withBaseline) where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.Char (isDigit)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.IO.Handle.Lock (LockMode (..), hLock, hTryLock, hUnlock)
import RIO
import RIO.Directory (XdgDirectory (..), canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, findExecutable, getXdgDirectory, removeDirectoryRecursive, removeFile)
import RIO.FilePath (takeDirectory, (</>))
import RIO.Map qualified as Map
import Sqiff.Config (BaselineConfig (..), DeployStep (..), Password (..), SqitchPlace (..), fill)
import Sqiff.Connection (failWith)
import Sqiff.ConnectionString (ConnectionString, fromPairs)
import Sqiff.Git (Repo, commonDir, findRepo, git, resolveCommit)
import System.Environment (getEnvironment, lookupEnv)
import System.Process.Typed (ProcessConfig, nullStream, proc, readProcess, runProcess, setEnv, setStdin, setStdout, setWorkingDir, useHandleOpen)


-- | Where one repository's baseline lives; its worktrees and jj workspaces share it.
data Paths = Paths
    { project :: Text
    , checkout :: FilePath
    , stateFile :: FilePath
    }


-- | Runs @action@ with the baseline database holding the schema of @rev@'s sqitch plan.
--
-- A lock in the cache keeps runs apart: the database is brought up to date under an
-- exclusive lock, then used under a shared one, so runs that need the same revision share
-- it and a run that needs another waits until they are done.
withBaseline :: BaselineConfig -> Text -> (ConnectionString -> IO a) -> IO a
withBaseline cfg rev action = do
    for_ ["docker", "git"] \tool -> do
        found <- findExecutable tool
        when (isNothing found) $ failWith [i|#{tool} is not on PATH; the baseline database needs it|]
    repo <- findRepo "."
    common <- commonDir repo
    let key = T.take 12 (tshow (hashWith SHA256 (encodeUtf8 (T.pack common))))
    cache <- getXdgDirectory XdgCache ("sqiff" </> T.unpack key)
    createDirectoryIfMissing True cache
    let p = Paths{project = "sqiff-" <> key, checkout = cache </> "checkout", stateFile = cache </> "baseline-state"}
    commit <- resolveCommit repo rev
    let wanted = BaselineState{commit, settings = settingsFingerprint cfg}
    withBinaryFile (cache </> "lock") ReadWriteMode \lock -> do
        let locked :: LockMode -> Text -> IO ()
            locked mode what = do
                free <- hTryLock lock mode
                unless free do
                    note [i|waiting for another sqiff run #{what} the baseline database|]
                    hLock lock mode
            ready = do
                state <- readState p
                running <- doesPathExist (p.checkout </> ".git") >>= bool (pure False) (isRunning cfg p)
                pure (state == Just wanted && running)
            loop = do
                locked ExclusiveLock "using"
                ( do
                        isReady <- ready
                        unless isReady $ bringTo cfg repo common p rev wanted
                    )
                    `finally` hUnlock lock
                -- Another run may bring it elsewhere between the two locks.
                locked SharedLock "changing"
                isReady <- ready `onException` hUnlock lock
                if isReady
                    then (connInfo cfg p >>= action) `finally` hUnlock lock
                    else hUnlock lock >> loop
        loop


-- | What the baseline database was built from: the commit, and the settings that shape the
-- deploy.
data BaselineState = BaselineState
    { commit :: Text
    , settings :: Text
    }
    deriving stock (Eq)


readState :: Paths -> IO (Maybe BaselineState)
readState p =
    doesFileExist p.stateFile >>= \case
        False -> pure Nothing
        True ->
            T.lines <$> readFileUtf8 p.stateFile <&> \case
                [commit, settings] -> Just BaselineState{commit, settings}
                _ -> Nothing


-- | A hash of the settings that decide what the deploy produces, without the password.
settingsFingerprint :: BaselineConfig -> Text
settingsFingerprint cfg =
    T.take 16
        . tshow
        . hashWith SHA256
        . encodeUtf8
        . T.unlines
        $ [ T.pack cfg.composeFile
          , cfg.service
          , tshow cfg.containerPort
          , cfg.user
          , cfg.database
          , case cfg.sqitch of OnHost -> "host"; InService -> "service"
          , tshow (Map.toList cfg.environment)
          , tshow [(step.args, step.ifExists) | step <- cfg.deploy]
          , tshow cfg.status
          ]


-- | Brings the baseline database to @wanted@: moves the checkout and deploys what the plan
-- adds, or starts from an empty volume when the deploy settings changed or the deployed
-- changes are not all in the plan.
bringTo :: BaselineConfig -> Repo -> FilePath -> Paths -> Text -> BaselineState -> IO ()
bringTo cfg repo common p rev wanted = do
    let commit = wanted.commit
    note [i|bringing the baseline database to #{T.take 12 commit} (#{rev})|]
    previous <- readState p
    whenM (doesFileExist p.stateFile) $ removeFile p.stateFile
    checkoutAt cfg repo common p commit
    when (maybe False ((/= wanted.settings) . (.settings)) previous) do
        note "the baseline settings in sqiff.yaml changed; rebuilding from scratch"
        compose cfg p ["down", "-v"]
    start cfg p
    stale <- staleDeploy cfg p
    when stale do
        note "the deployed changes are not all in this revision's plan; rebuilding from scratch"
        compose cfg p ["down", "-v"]
        start cfg p
    for_ cfg.deploy \step -> do
        present <- maybe (pure True) (doesPathExist . (p.checkout </>)) step.ifExists
        when present $ sqitchIn cfg p step.args
    writeFileUtf8 p.stateFile (commit <> "\n" <> wanted.settings <> "\n")


note :: Text -> IO ()
note message = TIO.hPutStrLn stderr ("sqiff: " <> message)


-- | The connection string for the port Docker published the service on.
connInfo :: BaselineConfig -> Paths -> IO ConnectionString
connInfo cfg p = do
    port <- publishedPort cfg p
    password <- passwordOf cfg
    let pairs = [("host", "localhost"), ("port", port), ("user", cfg.user), ("dbname", cfg.database)] <> [("password", pw) | pw <- maybeToList password]
    pure (fromPairs pairs)


passwordOf :: BaselineConfig -> IO (Maybe Text)
passwordOf cfg = case cfg.password of
    Nothing -> pure Nothing
    Just (PasswordText pw) -> pure (Just pw)
    Just (PasswordVariable var) -> lookupEnv (T.unpack var) >>= maybe (failWith [i|baseline.password-variable names #{var}, which is not set|]) (pure . Just . T.pack)


publishedPort :: BaselineConfig -> Paths -> IO Text
publishedPort cfg p = do
    let containerPort = cfg.containerPort
    (code, out, err) <- readCompose cfg p ["port", T.unpack cfg.service, show containerPort]
    case T.breakOnEnd ":" out of
        (_, port) | code == ExitSuccess, not (T.null port), T.all isDigit port -> pure port
        _ -> failWith [i|the baseline service publishes no port for #{containerPort}: #{out}#{err}|]


-- | Creates the git worktree at @commit@, or moves it there. A checkout that is not a
-- worktree of this repository is stopped and replaced; the database volume stays.
checkoutAt :: BaselineConfig -> Repo -> FilePath -> Paths -> Text -> IO ()
checkoutAt cfg repo common p commit =
    ours >>= \case
        True -> do
            let args = ["-C", p.checkout, "checkout", "--quiet", "--force", "--detach", T.unpack commit]
            code <- runProcess (toStderr (proc "git" args))
            unless (code == ExitSuccess) $ failWith [i|git #{unwords args} failed|]
        False -> do
            let checkout = p.checkout
            whenM (doesDirectoryExist checkout) do
                note [i|replacing #{checkout} with a git worktree|]
                whenM (doesFileExist (p.checkout </> cfg.composeFile)) $ compose cfg p ["down"]
                removeDirectoryRecursive p.checkout
            createDirectoryIfMissing True (takeDirectory p.checkout)
            git repo ["worktree", "prune"]
            git repo ["worktree", "add", "--quiet", "--detach", p.checkout, T.unpack commit]
  where
    ours =
        doesPathExist (p.checkout </> ".git") >>= \case
            False -> pure False
            True -> do
                (code, out, _) <- readProcess (quiet (proc "git" ["-C", p.checkout, "rev-parse", "--path-format=absolute", "--git-common-dir"]))
                case code of
                    ExitSuccess -> (== common) <$> canonicalizePath (T.unpack (decode out))
                    ExitFailure _ -> pure False


-- | Starts the server and waits until it accepts connections over TCP, which it does
-- once the init scripts are done: the image's temporary server for them listens on the
-- Unix socket only. The check runs in the container, so it asks its loopback, on the
-- port the server listens on there.
start :: BaselineConfig -> Paths -> IO ()
start cfg p = do
    compose cfg p ["up", "-d", service]
    awaitReady (0 :: Int)
  where
    service = T.unpack cfg.service
    project = p.project
    awaitReady n = do
        (code, _, _) <- readCompose cfg p ["exec", "-T", service, "pg_isready", "-q", "-h", "127.0.0.1", "-p", show cfg.containerPort]
        unless (code == ExitSuccess) do
            (_, state, _) <- readCompose cfg p ["ps", "-a", "--format", "{{.State}}", service]
            when (T.strip state `elem` ["exited", "dead"] || n >= 900)
                $ failWith [i|the baseline server did not come up; see docker compose -p #{project} logs #{service}|]
            when (n == 5) $ note [i|waiting for the baseline server (docker compose -p #{project} logs -f #{service})|]
            threadDelay 2_000_000
            awaitReady (n + 1)


-- | Whether sqitch reports deployed changes that the checkout's plan does not have.
staleDeploy :: BaselineConfig -> Paths -> IO Bool
staleDeploy cfg p = do
    (_, out, err) <- readProcess . quiet =<< sqitchProc cfg p cfg.status
    pure ("Cannot find" `T.isInfixOf` decode (out <> err))


sqitchIn :: BaselineConfig -> Paths -> [Text] -> IO ()
sqitchIn cfg p args = do
    code <- runProcess . toStderr =<< sqitchProc cfg p args
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [i|sqitch #{T.unwords args} failed for the baseline database with exit code #{n}|]


-- | sqitch in the service, or on this machine in the checkout with libpq's variables
-- pointing at the published port. @{host}@, @{port}@, @{user}@ and @{database}@ in the
-- arguments are where that sqitch reaches the baseline.
sqitchProc :: BaselineConfig -> Paths -> [Text] -> IO (ProcessConfig () () ())
sqitchProc cfg p args = do
    password <- passwordOf cfg
    let withPassword = [("PGPASSWORD", T.unpack pw) | pw <- maybeToList password]
        filled port = map (\a -> fill a [("host", "localhost"), ("port", port), ("user", cfg.user), ("database", cfg.database)]) args
    case cfg.sqitch of
        InService -> do
            env <- composeEnv cfg
            let passOn = concat [["-e", "PGPASSWORD"] | isJust password]
                exec = ["exec", "-T"] <> passOn <> [T.unpack cfg.service, "sqitch"]
            pure (setEnv (withPassword <> env) (composeProc cfg p (exec <> map T.unpack (filled (tshow cfg.containerPort)))))
        OnHost -> do
            port <- publishedPort cfg p
            env <- getEnvironment
            let ours = [("PGHOST", "localhost"), ("PGPORT", T.unpack port), ("PGUSER", T.unpack cfg.user), ("PGDATABASE", T.unpack cfg.database)] <> withPassword
                others = filter ((`notElem` (["PGSERVICE", "PGHOSTADDR"] <> map fst ours)) . fst) env
            pure (setEnv (ours <> others) (setWorkingDir p.checkout (quiet (proc "sqitch" (map T.unpack (filled port))))))


isRunning :: BaselineConfig -> Paths -> IO Bool
isRunning cfg p = do
    (code, out, _) <- readCompose cfg p ["ps", "--status", "running", "-q", T.unpack cfg.service]
    pure (code == ExitSuccess && not (T.null (T.strip out)))


compose :: BaselineConfig -> Paths -> [String] -> IO ()
compose cfg p args = do
    env <- composeEnv cfg
    code <- runProcess (toStderr (setEnv env (composeProc cfg p args)))
    let project = p.project
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [i|docker compose -p #{project} #{unwords args} failed with exit code #{n}|]


-- [allow:SqiffNamedTuples] pre-existing at adoption
readCompose :: BaselineConfig -> Paths -> [String] -> IO (ExitCode, Text, Text)
readCompose cfg p args = do
    env <- composeEnv cfg
    (code, out, err) <- readProcess (quiet (setEnv env (composeProc cfg p args)))
    pure (code, decode out, decode err)


-- | The checkout's own compose file, so image, command, init scripts and mounts all come
-- from the revision being built.
composeProc :: BaselineConfig -> Paths -> [String] -> ProcessConfig () () ()
composeProc cfg p args = proc "docker" (["compose", "-p", T.unpack p.project, "-f", p.checkout </> cfg.composeFile] <> args)


-- | The caller's environment with the configured variables, the port variable (0 lets
-- Docker pick a free port), and without a dev stack's compose settings.
composeEnv :: BaselineConfig -> IO [(String, String)]
composeEnv cfg = do
    env <- getEnvironment
    let ours = [(T.unpack k, T.unpack v) | (k, v) <- Map.toList cfg.environment] <> [(T.unpack v, show cfg.port) | v <- maybeToList cfg.portVariable]
        dropped = map fst ours <> ["COMPOSE_FILE", "COMPOSE_PROJECT_NAME", "COMPOSE_PROFILES"]
    pure (ours <> filter ((`notElem` dropped) . fst) env)


quiet :: ProcessConfig i o e -> ProcessConfig () o e
quiet = setStdin nullStream


toStderr :: ProcessConfig i o e -> ProcessConfig () () e
toStderr = setStdout (useHandleOpen stderr) . quiet


decode :: LByteString -> Text
decode = T.strip . decodeUtf8Lenient . toStrictBytes
