{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Writes changes with sqitch's own commands.
module Sqiff.Sqitch (
    PreviousFile (..),
    requireSqitch,
    writeUnits,
    reworkEdited,
    defaultTag,
) where

import Data.String.Interpolate qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import RIO
import RIO.Directory (doesDirectoryExist, findExecutable)
import RIO.FilePath ((<.>), (</>))
import RIO.Set qualified as Set
import Sqiff.Connection (failWith)
import Sqiff.Identifier
import Sqiff.Recipe
import Sqiff.Sqitch.Plan (SqitchPlan (..))
import Sqiff.Sqitch.Units
import Sqiff.Timing (timed)
import System.Process.Typed (proc, runProcess, setWorkingDir)


-- | Whether the file a rework replaces can still run on the edited table, as its revert.
data PreviousFile = PreviousFileRuns | PreviousFileFails
    deriving stock (Eq, Show)


requireSqitch :: IO ()
requireSqitch = do
    found <- findExecutable "sqitch"
    when (isNothing found) $ failWith "sqitch is not on PATH; load the nix environment (direnv) in this shell first"


-- | Writes each change with @sqitch add@ or @sqitch rework@, from create-migration's
-- templates in @templateDir@ when it is set, adding a tag first when a rework needs one.
writeUnits :: Maybe FilePath -> FilePath -> FilePath -> SqitchPlan -> Text -> [Unit] -> IO ()
writeUnits templateDir' root topDir sqitchPlan tag us = do
    requireSqitch
    templates <- maybe (pure False) doesDirectoryExist templateDir
    when (any (\u -> u.kind == ReworkChange && u.changeName `Set.notMember` sqitchPlan.tagged) us)
        $ sqitch ["tag", tag, "-n", "sqiff: before the changes it writes"]
    for_ us \u -> do
        let template = if templates && u.kind == AddChange then templateOf u else Nothing
            project = sqitchPlan.project
            name = u.changeName
            deploy = script "deploy" [S.i|-- Deploy #{project}:#{name} to pg|] u.deploy u
            revert = script "revert" [S.i|-- Revert #{project}:#{name} from pg|] u.revert u
            verify = writeFileUtf8 (scriptPath "verify" u) (verifyScript [S.i|-- Verify #{project}:#{name} on pg|] (if u.placement == Migration then u.deploy else []))
        case u.kind of
            AddChange -> sqitch (["add", u.changeName, "-n", u.note] <> foldMap templateArgs template)
            ReworkChange -> sqitch ["rework", u.changeName, "-n", u.note]
        case (template, shapeOf u) of
            (Just _, ValidateShape _ _) -> pure ()
            (Just _, AddColumnShape _ _ options) -> do
                rendered <- readFileUtf8 (scriptPath "deploy" u)
                let hazards = T.unlines [[S.i|-- #{hazardText h}|] | s <- u.deploy, h <- s.hazards]
                writeFileUtf8 (scriptPath "deploy" u)
                    . T.replace "__your_type_here__ null" options
                    . T.replace "\nalter table " [S.i|\n#{hazards}alter table |]
                    $ rendered
            (Just (_, vars), AddIndexShape t index)
                | any (\s -> EveryPartition `elem` s.hazards) u.deploy -> do
                    deploy >> revert
                    writeFileUtf8 (scriptPath "verify" u) (verifyScript [S.i|-- Verify #{project}:#{name} on pg|] u.deploy)
                | otherwise -> do
                    deploy
                    let ix = nameText index
                        templated = indexPrefix t <> fromMaybe ix (lookup "index_name" vars)
                    for_ ["revert", "verify"] \dir -> do
                        rendered <- readFileUtf8 (scriptPath dir u)
                        writeFileUtf8 (scriptPath dir u) (T.replace templated ix rendered)
            (Just _, OtherShape) -> deploy >> revert >> verify
            (Nothing, _) -> do
                deploy
                when (u.kind == AddChange) (revert >> verify)
  where
    sqitch = runSqitch root
    templateDir = (root </>) <$> templateDir'
    templateArgs (template, vars) =
        ["--template", template] <> foldMap (\d -> ["--template-directory", T.pack d]) templateDir <> concat [["-s", [S.i|#{k}=#{v}|]] | (k, v) <- vars]
    scriptPath dir u = root </> topDir </> dir </> T.unpack u.changeName <.> "sql"
    script dir header steps u
        | null steps = writeFileUtf8 (scriptPath dir u) [S.i|#{header}\n\n-- Nothing to revert.\n|]
        | otherwise = writeFileUtf8 (scriptPath dir u) (renderScript header u.placement steps)


-- | Reworks a change whose deploy script was already edited in place, putting the edit
-- back whatever happens; with 'PreviousFileFails' its revert does nothing.
reworkEdited :: FilePath -> FilePath -> SqitchPlan -> Text -> Text -> Text -> Text -> PreviousFile -> IO ()
reworkEdited root topDir sqitchPlan tag changeName note original previousFile = do
    let path = root </> topDir </> "deploy" </> T.unpack changeName <.> "sql"
        revertPath = root </> topDir </> "revert" </> T.unpack changeName <.> "sql"
    edited <- readFileUtf8 path
    ( do
            writeFileUtf8 path original
            unless (changeName `Set.member` sqitchPlan.tagged)
                $ runSqitch root ["tag", tag, "-n", "sqiff: before the changes it writes"]
            runSqitch root ["rework", changeName, "-n", note]
        )
        `finally` writeFileUtf8 path edited
    let project = sqitchPlan.project
    when (previousFile == PreviousFileFails)
        $ writeFileUtf8
            revertPath
            [S.i|-- Revert #{project}:#{changeName} from pg

-- The migrations before this change revert the table; the previous file fails on the edited table.
|]


runSqitch :: FilePath -> [Text] -> IO ()
runSqitch root args = do
    let command = T.unwords (take 1 args)
    code <- timed [S.i|sqitch #{command}|] $ runProcess (setWorkingDir root (proc "sqitch" (map T.unpack args)))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [S.i|sqitch #{T.unwords args} failed with exit code #{n}; changes before it were written|]


-- | The tag a rework gets when none is named: today's date with @_sqiff@.
defaultTag :: UTCTime -> Text
defaultTag now = let day = formatTime defaultTimeLocale "%Y-%m-%d" now in [S.i|#{day}_sqiff|]
