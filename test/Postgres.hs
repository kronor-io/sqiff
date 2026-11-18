{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A throwaway PostgreSQL cluster for the integration tests: initdb in a temporary
-- directory, listening on a Unix socket only, removed when the tests end.
--
-- The binaries come from @SQIFF_TEST_PG_BIN@ when it is set, else from @PATH@.
module Postgres (
    Cluster,
    findBinaries,
    startCluster,
    stopCluster,
    conninfo,
    createDatabase,
    runScript,
) where

import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import RIO
import RIO.Directory (createDirectory, findExecutable, removeDirectoryRecursive)
import RIO.FilePath ((</>))
import RIO.Text qualified as T
import Sqiff.Catalog (connect, errorText)
import System.Environment (lookupEnv)
import System.IO.Temp (createTempDirectory)
import System.Process.Typed (proc, readProcess)


data Cluster = Cluster
    { binDir :: FilePath
    , root :: FilePath
    }


-- | The directory with @initdb@ and @pg_ctl@, or why there is none.
findBinaries :: IO (Either Text FilePath)
findBinaries =
    lookupEnv "SQIFF_TEST_PG_BIN" >>= \case
        Just dir -> pure (Right dir)
        Nothing ->
            findExecutable "initdb" <&> \case
                Just path -> Right (takeDirectoryOf path)
                Nothing -> Left "initdb is neither on PATH nor in SQIFF_TEST_PG_BIN"
  where
    takeDirectoryOf = reverse . drop 1 . dropWhile (/= '/') . reverse


-- | Makes and starts a cluster. The socket lives under /tmp: a Unix socket path must stay
-- under about 100 bytes, which a nested temporary directory can exceed.
startCluster :: FilePath -> IO Cluster
startCluster binDir = do
    root <- createTempDirectory "/tmp" "sqiff-test"
    let cluster = Cluster{binDir, root}
    createDirectory (root </> "socket")
    run cluster "initdb" ["-D", root </> "data", "-U", "sqiff", "--auth=trust", "--no-sync", "-E", "UTF8", "--locale=C"]
    run
        cluster
        "pg_ctl"
        [ "-D"
        , root </> "data"
        , "-l"
        , root </> "log"
        , "-w"
        , "-o"
        , [i|-k #{root </> "socket"} -c listen_addresses='' -c fsync=off -c synchronous_commit=off -c full_page_writes=off -c max_connections=300|]
        , "start"
        ]
    pure cluster


stopCluster :: Cluster -> IO ()
stopCluster cluster = do
    run cluster "pg_ctl" ["-D", root cluster </> "data", "-m", "immediate", "-w", "stop"]
    removeDirectoryRecursive (root cluster)


run :: Cluster -> FilePath -> [String] -> IO ()
run cluster program args = do
    (code, out, err) <- readProcess (proc (binDir cluster </> program) args)
    unless (code == ExitSuccess)
        $ throwString [i|#{program} failed: #{decodeUtf8Lenient (toStrictBytes out)}#{decodeUtf8Lenient (toStrictBytes err)}|]


-- | The connection string for one of the cluster's databases.
conninfo :: Cluster -> Text -> Text
conninfo cluster db = [i|host=#{root cluster </> "socket"} user=sqiff dbname=#{db}|]


-- | Drops @db@ if it is there and creates it again, empty.
createDatabase :: Cluster -> Text -> IO ()
createDatabase cluster db = do
    _ <- runScript cluster "postgres" [i|drop database if exists #{db} with (force)|]
    runScript cluster "postgres" [i|create database #{db}|] >>= either (throwString . T.unpack) pure


-- | Runs @sql@ on @db@ as one simple-protocol script, so in one implicit transaction.
runScript :: Cluster -> Text -> Text -> IO (Either Text ())
runScript cluster db sql =
    bracket (connect (conninfo cluster db)) Connection.release \conn ->
        first errorText <$> Connection.use conn (Session.script sql)
