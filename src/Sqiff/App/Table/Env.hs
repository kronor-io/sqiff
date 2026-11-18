{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What the table command works on, and the helpers its parts share.
module Sqiff.App.Table.Env (
    TableEnv (..),
    parseFile,
    atBase,
    renameSql,
    withoutLeadingComments,
    withScratch,
) where

import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import RIO
import RIO.Text qualified as T
import Sqiff.App.Options (TableOptions (..))
import Sqiff.Config (Config (..), Layout (..))
import Sqiff.Connection (failWith, withConnection)
import Sqiff.ConnectionString (ConnectionString, redacted)
import Sqiff.Git (Repo)
import Sqiff.Identifier
import Sqiff.Script (splitStatements, withoutTransactionControl)
import Sqiff.TableEdit (requireScratch)


-- | What the table command works on: its options, the deploy file before and after the
-- edit, and where they live.
data TableEnv = TableEnv
    { opts :: TableOptions
    , db :: ConnectionString
    , file :: FilePath
    , root :: FilePath
    , repo :: Repo
    , config :: Config
    , layout :: Layout
    , sqitchChange :: Text
    , table :: QName
    , statements :: [Text]
    , original :: Text
    }


-- | Runs @action@ with a connection to the database the table command edits in
-- rolled-back transactions; released when it returns or fails.
withScratch :: MonadUnliftIO m => ConnectionString -> (Connection.Connection -> m a) -> m a
withScratch db action = withConnection db \conn -> do
    liftIO (requireScratch (redacted db) conn)
    action conn


-- | A deploy file's statements; @label@ names the version in a parse error.
parseFile :: MonadIO m => FilePath -> Text -> Text -> m [Text]
parseFile file label = either (\e -> failWith [i|#{file}#{label}: #{e}|]) pure . (withoutTransactionControl <=< splitStatements)


atBase :: TableOptions -> Text
atBase opts = let base = opts.base in [i| at #{base}|]


renameSql :: QName -> (Identifier, Identifier) -> Text
renameSql table (old, new) = [i|alter table #{qualified table} rename column #{quoted old} to #{quoted new}|]


-- | A statement from its first line of code.
withoutLeadingComments :: Text -> Text
withoutLeadingComments = T.stripStart . T.unlines . dropWhile (\l -> T.null (T.strip l) || "--" `T.isPrefixOf` T.stripStart l) . T.lines
