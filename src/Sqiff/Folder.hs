{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Writes a schema as one file per object, for example
-- @table-column/public/payment/amount.sql@.
module Sqiff.Folder (Existing (..), writeFolder) where

import Crypto.Hash qualified as Hash
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import RIO.FilePath (joinPath, takeDirectory, (<.>), (</>))
import RIO.Map qualified as Map
import Sqiff.Connection (failWith)
import Sqiff.Definition (printed)
import Sqiff.Model (Address (..), Aspect (..), ObjectKey (..), Schema, toAddress)


-- | What to do with a folder that is not empty.
data Existing = KeepExisting | ReplaceOwn
    deriving stock (Eq, Show)


-- | Writes into an empty or new folder. With 'ReplaceOwn', first clears a folder this
-- function wrote before, which it recognises by its marker file.
writeFolder :: Existing -> FilePath -> Schema -> IO ()
writeFolder existing root schema = do
    exists <- doesDirectoryExist root
    entries <- if exists then listDirectory root else pure []
    unless (null entries) do
        when (existing == KeepExisting) $ failWith (T.pack root <> " is not empty; pass --replace to rewrite it, or choose another folder")
        written <- doesFileExist (root </> marker)
        unless written $ failWith (T.pack root <> " was not written by sqiff, so --replace leaves it alone")
        removeDirectoryRecursive root
    for_ (Map.toList schema) \(key, definition) -> do
        let path = root </> objectPath key
        createDirectoryIfMissing True (takeDirectory path)
        writeFileUtf8 path (printed definition <> "\n")
    writeFileUtf8 (root </> marker) "Written by sqiff describe --out; --replace clears this folder.\n"


marker :: FilePath
marker = ".sqiff"


objectPath :: ObjectKey -> FilePath
objectPath key = segment addr.objectType </> joinPath (map segment parents) </> (segment name <> args) <.> extension
  where
    addr = toAddress key.object

    (parents, name) = case reverse addr.objectNames of
        [] -> ([], "_")
        lastName : rest -> (reverse rest, lastName)

    args
        | null addr.objectArgs = ""
        | T.length signature <= 120 = segment signature
        | otherwise = [i|(#{shortHash signature})|]
      where
        signature = [i|(#{T.intercalate ", " (objectArgs addr)})|]

    extension = case key.aspect of
        Definition -> "sql"
        Privileges -> "privileges"
        Comment -> "comment"


shortHash :: Text -> Text
shortHash = T.take 12 . tshow . Hash.hashWith Hash.SHA256 . encodeUtf8


segment :: Text -> FilePath
segment = T.unpack . T.replace "/" "%2F" . T.replace " " "-" . dotless
  where
    dotless t = if "." `T.isPrefixOf` t then "%2E" <> T.drop 1 t else t
