{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Where a sqitch project keeps its plan and scripts, resolved once from sqitch's
-- configuration and the command line, and the configuration file that makes every sqitch
-- command sqiff runs use exactly that layout.
module Sqiff.Sqitch.Layout (
    Layout (..),
    Script (..),
    findLayout,
    resolveLayout,
    planPath,
    scriptPath,
    scriptDirs,
    sqitchConf,
) where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO hiding (local)
import RIO.FilePath (normalise, (<.>), (</>))
import RIO.List qualified as L
import RIO.Map qualified as Map
import Sqiff.Connection (failWith)
import System.Process.Typed (nullStream, proc, readProcess, setStdin, setWorkingDir)


-- | The project's layout. The directories and the plan file are relative to @root@, as
-- sqitch's configuration writes them.
data Layout = Layout
    { root :: FilePath
    , topDir :: FilePath
    , planFile :: FilePath
    , deployDir :: FilePath
    , revertDir :: FilePath
    , verifyDir :: FilePath
    , reworkedDeployDir :: FilePath
    , reworkedRevertDir :: FilePath
    , reworkedVerifyDir :: FilePath
    -- ^ Where @sqitch rework@ copies the previous scripts.
    , extension :: String
    , local :: [(Text, Text)]
    -- ^ The project's own settings, as @sqitch config --local --list@ prints them.
    , engine :: Text
    , target :: Maybe Text
    }


data Script = Deploy | Revert | Verify
    deriving stock (Eq, Show, Enum, Bounded)


planPath :: Layout -> FilePath
planPath l = l.root </> l.planFile


-- | Where a change's script is.
scriptPath :: Layout -> Script -> Text -> FilePath
scriptPath l script change = l.root </> dir </> T.unpack change <.> l.extension
  where
    dir = case script of
        Deploy -> l.deployDir
        Revert -> l.revertDir
        Verify -> l.verifyDir


-- | Every directory sqitch writes scripts to, absolute.
scriptDirs :: Layout -> [FilePath]
scriptDirs l = nubOrd (map (normalise . (l.root </>)) [l.deployDir, l.revertDir, l.verifyDir, l.reworkedDeployDir, l.reworkedRevertDir, l.reworkedVerifyDir])


-- | The layout of the project in @root@, with @--top-dir@ and @--plan-file@ when they
-- are given, from @sqitch config@.
findLayout :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO Layout
findLayout root top planFile = do
    merged <- configList ["--list"]
    local <- configList ["--local", "--list"]
    either failWith pure (resolveLayout root top planFile merged local)
  where
    configList args = do
        (code, out, err) <- readProcess (setStdin nullStream (setWorkingDir root (proc "sqitch" ("config" : args))))
        case code of
            ExitSuccess -> pure [(T.toLower k, T.drop 1 v) | l <- T.lines (decode out), let (k, v) = T.breakOn "=" l, not (T.null v)]
            -- An empty file or none: sqitch config exits 1 with nothing to list.
            ExitFailure 1 | T.null (decode err) -> pure []
            ExitFailure _ -> failWith [i|sqitch config #{T.unwords (map T.pack args)} in #{root} failed: #{decode err}|]
    decode = T.strip . decodeUtf8Lenient . toStrictBytes


-- | The layout from sqitch's settings, all of them (@merged@) and the project's own
-- (@local@). A setting for the target wins over one for the engine, which wins over the
-- core one, as in sqitch.
resolveLayout :: FilePath -> Maybe FilePath -> Maybe FilePath -> [(Text, Text)] -> [(Text, Text)] -> Either Text Layout
resolveLayout root top planGiven merged local = do
    let settings = Map.fromList merged
        engine = fromMaybe "pg" (Map.lookup "core.engine" settings)
        target = Map.lookup [i|engine.#{engine}.target|] settings <|> Map.lookup "core.target" settings
        named :: Text -> Maybe Text
        named key =
            listToMaybe
                ( catMaybes
                    [ target >>= \t -> Map.lookup [i|target.#{t}.#{key}|] settings
                    , Map.lookup [i|engine.#{engine}.#{key}|] settings
                    , Map.lookup [i|core.#{key}|] settings
                    ]
                )
        path :: Text -> Maybe FilePath
        path = fmap T.unpack . named
        topDir = fromMaybe (fromMaybe "." (path "top_dir")) top
        dirFor key name = fromMaybe (topDir </> name) (guard (isNothing top) *> path key)
        reworked = fromMaybe topDir (path "reworked_dir")
        reworkedDirFor key name = fromMaybe (reworked </> name) (path key)
        deployDir = dirFor "deploy_dir" "deploy"
        revertDir = dirFor "revert_dir" "revert"
        verifyDir = dirFor "verify_dir" "verify"
    when (any (T.any (== '\n') . snd) local) $ Left "sqitch.conf has a multi-line value, which sqiff cannot pass on to sqitch"
    pure
        Layout
            { root
            , topDir
            , planFile = fromMaybe (fromMaybe (topDir </> "sqitch.plan") (guard (isNothing top) *> path "plan_file")) planGiven
            , deployDir
            , revertDir
            , verifyDir
            , reworkedDeployDir = reworkedDirFor "reworked_deploy_dir" "deploy"
            , reworkedRevertDir = reworkedDirFor "reworked_revert_dir" "revert"
            , reworkedVerifyDir = reworkedDirFor "reworked_verify_dir" "verify"
            , extension = maybe "sql" T.unpack (named "extension")
            , local
            , engine
            , target
            }


-- | The project configuration sqiff gives sqitch, as @SQITCH_CONFIG@: the project's own
-- settings, with the layout set for the core, the engine and the target, so no other
-- setting can move a script or the plan.
sqitchConf :: Layout -> Text
sqitchConf l = T.unlines (concatMap section (Map.toList sections))
  where
    layoutKeys = ["top_dir", "plan_file", "deploy_dir", "revert_dir", "verify_dir", "reworked_dir", "reworked_deploy_dir", "reworked_revert_dir", "reworked_verify_dir", "extension"]
    -- sqitch puts a reworked directory left unset under top_dir, so a pinned relative
    -- deploy_dir would land in top_dir twice.
    pinned =
        [ ("top_dir", T.pack l.topDir)
        , ("plan_file", T.pack l.planFile)
        , ("deploy_dir", T.pack l.deployDir)
        , ("revert_dir", T.pack l.revertDir)
        , ("verify_dir", T.pack l.verifyDir)
        , ("reworked_deploy_dir", T.pack l.reworkedDeployDir)
        , ("reworked_revert_dir", T.pack l.reworkedRevertDir)
        , ("reworked_verify_dir", T.pack l.reworkedVerifyDir)
        , ("extension", T.pack l.extension)
        ]
    levels = [("core", Nothing), ("engine", Just l.engine)] <> [("target", Just t) | Just t <- [l.target]]
    own = [(splitKey k, v) | (k, v) <- l.local, let (_, _, name) = splitKey k, name `notElem` layoutKeys]
    sections =
        Map.fromListWith
            (flip (<>))
            ( [((s, sub), [(name, v)]) | ((s, sub, name), v) <- own]
                <> [(level, pinned) | level <- levels]
            )
    -- section.name, or section.subsection.name where the subsection may hold dots.
    splitKey k = case T.splitOn "." k of
        [s, name] -> (s, Nothing, name)
        s : rest@(_ : _ : _) -> (s, Just (T.intercalate "." (take (length rest - 1) rest)), fromMaybe "" (L.lastMaybe rest))
        _ -> (k, Nothing, "")
    section ((s, sub), entries) =
        maybe [i|[#{s}]|] (\n -> [i|[#{s} "#{escape n}"]|]) sub
            : [[i|\t#{name} = "#{escape v}"|] | (name, v) <- entries]
    escape = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"
