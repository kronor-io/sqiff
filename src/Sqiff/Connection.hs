{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Connections to PostgreSQL, opened and released in scopes, and the one-line failures
-- the command line reports.
module Sqiff.Connection (
    UserError (..),
    failWith,
    connect,
    withConnection,
    withConnections,
    inRolledBackTransaction,
    run,
    qualifiedNames,
    errorText,
    serverError,
) where

import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TH (singletonStatement)
import Pqi.Ffi qualified
import RIO
import RIO.Text qualified as T
import Sqiff.ConnectionString (ConnectionString, libpqText)


-- | A failure the command line reports as one line.
newtype UserError = UserError Text
    deriving stock (Show)


instance Exception UserError


failWith :: MonadIO m => Text -> m a
failWith = throwIO . UserError . T.unwords . T.words


connect :: ConnectionString -> IO Connection.Connection
connect conninfo =
    Connection.acquire Pqi.Ffi.adapter (Settings.connectionString (libpqText conninfo))
        >>= either (\e -> failWith ("could not connect: " <> firstLine (reason e))) pure
  where
    reason = \case
        Errors.NetworkingConnectionError t -> t
        Errors.AuthenticationConnectionError t -> t
        Errors.CompatibilityConnectionError t -> t
        Errors.OtherConnectionError t -> t
    firstLine = T.takeWhile (/= '\n')


-- | Runs @action@ in a transaction that is always rolled back; @label@ names it in errors.
-- When @action@ succeeds the rollback must too, as the connection's state is unknown
-- otherwise. When @action@ fails, its error is the one that counts.
inRolledBackTransaction :: Connection.Connection -> Text -> IO a -> IO a
inRolledBackTransaction conn label action = do
    script "begin" "could not start"
    result <- action `onException` tryAny (Connection.use conn (Session.script "rollback"))
    script "rollback" "could not be rolled back"
    pure result
  where
    script :: Text -> Text -> IO ()
    script sql failed = Connection.use conn (Session.script sql) >>= either (\e -> failWith [i|#{label} #{failed}: #{errorText e}|]) pure


-- | Runs @action@ with a connection that is released when it returns or fails.
withConnection :: MonadUnliftIO m => ConnectionString -> (Connection.Connection -> m a) -> m a
withConnection db = bracket (liftIO (connect db)) (liftIO . Connection.release)


-- | Runs @action@ with @n@ connections, opened at the same time, at least one; all that
-- opened are released when it returns or fails, also when another failed to open.
withConnections :: MonadUnliftIO m => ConnectionString -> Int -> (NonEmpty Connection.Connection -> m a) -> m a
withConnections db n action =
    bracket (newIORef []) (liftIO . (traverse_ Connection.release <=< readIORef)) \opened -> do
        let open = liftIO $ mask_ do
                c <- connect db
                atomicModifyIORef' opened (\cs -> (c : cs, ()))
                pure c
        (c, cs) <- concurrently open (replicateConcurrently (max 0 (n - 1)) open)
        action (c :| cs)


run :: Connection.Connection -> params -> Statement.Statement params a -> IO a
run conn params statement =
    Connection.use conn (Session.statement params statement)
        >>= either (\e -> failWith [i|query failed: #{errorText e}|]) pure


-- | Runs @action@ with an empty @search_path@, which schema-qualifies the names the catalog
-- functions print.
qualifiedNames :: Connection.Connection -> IO a -> IO a
qualifiedNames conn action = do
    previous <- run conn () [singletonStatement| select current_setting('search_path') :: text |]
    void (run conn "" setPath)
    action `finally` void (Connection.use conn (Session.statement previous setPath))
  where
    setPath = [singletonStatement| select set_config('search_path', $1 :: text, false) :: text |]


-- | The message of an error the server raised; 'Nothing' for connection and client errors.
serverError :: Errors.SessionError -> Maybe Text
serverError e = case e of
    Errors.ScriptSessionError _ _ -> Just (errorText e)
    Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError _) -> Just (errorText e)
    _ -> Nothing


errorText :: Errors.SessionError -> Text
errorText = \case
    Errors.ScriptSessionError _ (Errors.ServerError _ message detail _ _) -> message <> maybe "" ("; " <>) detail
    Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError _ message detail _ _)) -> message <> maybe "" ("; " <>) detail
    e -> tshow e
