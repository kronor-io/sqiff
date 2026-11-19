{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Writes a schema as one file per object, for example
-- @table-column/public/payment/amount.sql@.
module Sqiff.Folder (Existing (..), writeFolder, exportManifest) where

import Crypto.Hash qualified as Hash
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, renameDirectory)
import RIO.FilePath (dropTrailingPathSeparator, joinPath, splitExtension, takeDirectory, (<.>), (</>))
import RIO.Map qualified as Map
import Sqiff.Connection (failWith)
import Sqiff.Definition (printed)
import Sqiff.Model (Address (..), Aspect (..), ObjectKey (..), Schema, toAddress)


-- | What to do with a folder that is not empty.
data Existing = KeepExisting | ReplaceOwn
    deriving stock (Eq, Show)


-- | Writes into an empty or new folder. With 'ReplaceOwn', replaces a folder this
-- function wrote before, which it recognises by its marker file. The new export is written
-- beside it first, so a failure leaves the old one as it was.
writeFolder :: Existing -> FilePath -> Schema -> IO ()
writeFolder existing root schema = do
    exists <- doesDirectoryExist root
    entries <- if exists then listDirectory root else pure []
    unless (null entries) do
        when (existing == KeepExisting) $ failWith (T.pack root <> " is not empty; pass --replace to rewrite it, or choose another folder")
        written <- doesFileExist (root </> marker)
        unless written $ failWith (T.pack root <> " was not written by sqiff, so --replace leaves it alone")
    let staging = dropTrailingPathSeparator root <> ".sqiff-staging"
    whenM (doesDirectoryExist staging) $ removeDirectoryRecursive staging
    ( do
            createDirectoryIfMissing True staging
            for_ (Map.toList (exportManifest schema)) \(path, text) -> do
                createDirectoryIfMissing True (takeDirectory (staging </> path))
                writeFileUtf8 (staging </> path) text
            writeFileUtf8 (staging </> marker) "Written by sqiff describe --out; --replace clears this folder.\n"
        )
        `onException` tryAny (removeDirectoryRecursive staging)
    when exists $ removeDirectoryRecursive root
    renameDirectory staging root


-- | Each object's file and contents, relative to the folder. Paths are distinct even
-- where file names compare without case: those that would clash get a hash of the
-- object's name.
exportManifest :: Schema -> Map FilePath Text
exportManifest schema =
    Map.fromList
        [ (if clashes path then withHash key path else path, printed definition <> "\n")
        | (key, definition, path) <- files
        ]
  where
    files = [(key, definition, objectPath key) | (key, definition) <- Map.toList schema]
    folded = Map.fromListWith (+) [(T.toLower (T.pack path), 1 :: Int) | (_, _, path) <- files]
    clashes path = Map.findWithDefault 0 (T.toLower (T.pack path)) folded > 1
    withHash key path =
        let (base, extension) = splitExtension path
         in base <> "~" <> T.unpack (shortHash (tshow key)) <> extension


marker :: FilePath
marker = ".sqiff"


objectPath :: ObjectKey -> FilePath
objectPath key = kindSegment addr.objectType </> joinPath (map segment parents) </> (segment name <> args) <.> extension
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


-- | A kind of object as a folder name, @table column@ as @table-column@. Kinds are
-- PostgreSQL's words for them, letters and spaces only, so the dashes cannot make two
-- kinds alike.
kindSegment :: Text -> FilePath
kindSegment = T.unpack . T.replace " " "-"


shortHash :: Text -> Text
shortHash = T.take 12 . tshow . Hash.hashWith Hash.SHA256 . encodeUtf8


-- | A name as one path segment. Percent signs are escaped too, so different names give
-- different segments.
segment :: Text -> FilePath
segment = T.unpack . dotless . T.concatMap escape
  where
    escape = \case
        '%' -> "%25"
        '/' -> "%2F"
        ' ' -> "%20"
        c -> T.singleton c
    dotless t = if "." `T.isPrefixOf` t then "%2E" <> T.drop 1 t else t
