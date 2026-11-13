{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Reading revisions with git, also from a jj workspace that has no @.git@ of its own.
module Kronor.SchemaDiff.Git (Repo (..), findRepo, showFile, resolveCommit, git) where

import Data.String.Interpolate (i)
import Kronor.SchemaDiff.Catalog (failWith)
import RIO
import RIO.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, makeAbsolute)
import RIO.FilePath (makeRelative, takeDirectory, (</>))
import RIO.Text qualified as T
import System.Process.Typed (ProcessConfig, nullStream, proc, readProcess, setStdin, setWorkingDir)


-- | A git directory and the top of the working tree that paths are relative to.
data Repo = Repo
    { gitDir :: FilePath
    , top :: FilePath
    }


-- | The repository @dir@ is in.
findRepo :: FilePath -> IO Repo
findRepo dir = do
    (code, out, _) <- readProcess (setStdin nullStream (setWorkingDir dir (proc "git" ["rev-parse", "--absolute-git-dir", "--show-toplevel"])))
    case (code, T.lines (decode out)) of
        (ExitSuccess, [gitDir, top]) -> pure Repo{gitDir = T.unpack gitDir, top = T.unpack top}
        _ -> makeAbsolute dir >>= jjWorkspace >>= maybe (failWith [i|#{dir} is not in a git repository|]) pure


-- | A jj workspace's top and the git store its repo is backed by.
jjWorkspace :: FilePath -> IO (Maybe Repo)
jjWorkspace dir =
    doesDirectoryExist (dir </> ".jj") >>= \case
        True -> do
            let jjDir = dir </> ".jj"
            pointer <- doesFileExist (jjDir </> "repo")
            repoDir <- if pointer then (jjDir </>) . T.unpack . T.strip <$> readFileUtf8 (jjDir </> "repo") else pure (jjDir </> "repo")
            let store = repoDir </> "store"
            target <- T.unpack . T.strip <$> readFileUtf8 (store </> "git_target")
            gitDir <- canonicalizePath (store </> target)
            pure (Just Repo{gitDir, top = dir})
        False
            | takeDirectory dir == dir -> pure Nothing
            | otherwise -> jjWorkspace (takeDirectory dir)


-- | A file as it is at @rev@.
showFile :: Repo -> Text -> FilePath -> IO Text
showFile repo rev path = do
    let relative = makeRelative repo.top path
        spec = [i|#{rev}:#{relative}|] :: String
    (code, out, err) <- readProcess (gitProc repo ["show", spec])
    case code of
        ExitSuccess -> pure (decodeUtf8Lenient (toStrictBytes out))
        ExitFailure _ -> failWith [i|git show #{spec}: #{decode err}|]


-- | The commit @rev@ names.
resolveCommit :: Repo -> Text -> IO Text
resolveCommit repo rev = do
    (code, out, _) <- readProcess (gitProc repo ["rev-parse", "--verify", "--quiet", T.unpack rev <> "^{commit}"])
    case code of
        ExitSuccess -> pure (decode out)
        ExitFailure _ -> failWith [i|#{rev} names no commit|]


-- | Runs git on the repository; its error output goes into the failure.
git :: Repo -> [String] -> IO ()
git repo args = do
    (code, _, err) <- readProcess (gitProc repo args)
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [i|git #{unwords args} failed with exit code #{n}: #{decode err}|]


gitProc :: Repo -> [String] -> ProcessConfig () () ()
gitProc repo args = setStdin nullStream (proc "git" (("--git-dir=" <> repo.gitDir) : args))


decode :: LByteString -> Text
decode = T.strip . decodeUtf8Lenient . toStrictBytes
