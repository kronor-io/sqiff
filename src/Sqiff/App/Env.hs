{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What every command runs with: the project's configuration and what to read of the
-- databases, and where its output goes.
module Sqiff.App.Env (
    AppEnv (..),
    loadAppEnv,
    Scope (..),
    BodyChecks (..),
    Source (..),
    source,
    say,
    note,
) where

import Data.Text.IO qualified as TIO
import RIO
import RIO.Text qualified as T
import Sqiff.Catalog (Partitions (..))
import Sqiff.Config (Config (..), loadConfig)
import Sqiff.ConnectionString (ConnectionString, connectionString)


data AppEnv = AppEnv
    { config :: Config
    , scope :: Scope
    -- ^ With the configuration's excluded schemas.
    }


-- | The project's configuration in @root@, and @scope@ with the schemas it excludes.
loadAppEnv :: FilePath -> Scope -> IO AppEnv
loadAppEnv root scope = do
    config <- loadConfig root
    pure AppEnv{config, scope = scope{excluded = config.excludeSchemas <> scope.excluded}}


-- | What a command reads of a database.
data Scope = Scope
    { excluded :: [Text]
    , partitions :: Partitions
    , bodyCheck :: BodyChecks
    }


-- | Whether SQL-standard function bodies are recreated on @--to@ to compare them.
data BodyChecks = CheckBodies | SkipBodyChecks
    deriving stock (Eq)


-- | What @--from@ names: a connection string, or the baseline database built from a revision.
data Source
    = Database ConnectionString
    | BaselineAt Text


source :: Text -> Source
source s
    | s == "master" = BaselineAt s
    | Just rev <- T.stripPrefix "rev:" s = BaselineAt rev
    | otherwise = Database (connectionString s)


-- | A line of the command's output.
say :: MonadIO m => Text -> m ()
say = liftIO . TIO.putStrLn


-- | A line on stderr, beside the output.
note :: MonadIO m => Text -> m ()
note = liftIO . TIO.hPutStrLn stderr
