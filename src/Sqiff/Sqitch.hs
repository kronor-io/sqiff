{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Writes changes to a sqitch project with sqitch's own commands. Every write happens
-- inside 'withProject': sqitch runs with the project's resolved layout, the batch is
-- checked before the first write, and a failure puts the plan and the scripts back as
-- they were.
module Sqiff.Sqitch (
    PreviousFile (..),
    requireSqitch,
    Project,
    withProject,
    writeUnits,
    reworkEdited,
    moveToEnd,
    writeTracked,
) where

import Data.String.Interpolate qualified as S
import Data.Text qualified as T
import RIO
import RIO.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory, removeDirectory, removeFile)
import RIO.FilePath ((</>))
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Connection (UserError (..), failWith)
import Sqiff.Identifier
import Sqiff.Recipe
import Sqiff.Sqitch.Check (Tagging, batchProblems, chooseTag)
import Sqiff.Sqitch.Layout (Layout (..), Script (..), planPath, scriptDirs, scriptPath, sqitchConf)
import Sqiff.Sqitch.Plan (SqitchPlan (..), entryName, readPlan)
import Sqiff.Sqitch.Units
import Sqiff.Timing (timed)
import System.Environment (getEnvironment)
import System.Process.Typed (proc, runProcess, setEnv, setWorkingDir)


-- | Whether the file a rework replaces can still run on the edited table, as its revert.
data PreviousFile = PreviousFileRuns | PreviousFileFails
    deriving stock (Eq, Show)


requireSqitch :: IO ()
requireSqitch = do
    found <- findExecutable "sqitch"
    when (isNothing found) $ failWith "sqitch is not on PATH; load the nix environment (direnv) in this shell first"


-- | A sqitch project being written, inside 'withProject'.
data Project = Project
    { layout :: Layout
    , conf :: FilePath
    -- ^ The configuration sqitch runs with, from 'sqitchConf'.
    , originals :: IORef (Map FilePath (Maybe Text))
    -- ^ What each file sqiff wrote held before, if it existed.
    }


-- | Runs @action@ on the project. If it fails, the plan, the files sqiff wrote and the
-- scripts sqitch made are put back as they were, and the error says so.
withProject :: Layout -> (Project -> IO a) -> IO a
withProject layout action = withSystemTempDirectory "sqiff-sqitch" \tmp -> do
    let conf = tmp </> "sqitch.conf"
    writeFileUtf8 conf (sqitchConf layout)
    plan <- readFileUtf8 (planPath layout)
    before <- foldMap entriesUnder (scriptDirs layout)
    originals <- newIORef Map.empty
    let restore = do
            readIORef originals >>= traverse_ (uncurry putBack) . Map.toList
            -- The plan last: sqitch changed it, and sqiff may have rewritten it since.
            writeFileUtf8 (planPath layout) plan
            (files, dirs) <- foldMap entriesUnder (scriptDirs layout)
            traverse_ removeFile (Set.toList (files `Set.difference` fst before))
            -- Deepest first, so a new directory is empty when its turn comes.
            traverse_ removeDirectory (L.sortOn (Down . length) (Set.toList (dirs `Set.difference` snd before)))
        putBack path = maybe (removeFile path) (writeFileUtf8 path)
        rolledBack e = do
            restored <- tryAny restore
            case (restored, fromException e) of
                (Right (), Just (UserError message)) -> throwIO (UserError (message <> "; nothing was written, the project is as it was"))
                (Right (), Nothing) -> throwIO e
                (Left failure, _) -> throwIO (UserError [S.i|#{displayException e}; putting the project back failed too (#{displayException failure}), so check git status|])
    action Project{layout, conf, originals} `catchAny` rolledBack


-- | The files and directories under @dir@, recursively.
entriesUnder :: FilePath -> IO (Set FilePath, Set FilePath)
entriesUnder dir =
    doesDirectoryExist dir >>= \case
        False -> pure mempty
        True -> do
            names <- listDirectory dir
            fold <$> for names \n -> do
                let path = dir </> n
                isDir <- doesDirectoryExist path
                if isDir
                    then (\(fs, ds) -> (fs, Set.insert path ds)) <$> entriesUnder path
                    else pure (Set.singleton path, Set.empty)


-- | Writes a file, keeping what it held first so 'withProject' can put it back.
writeTracked :: Project -> FilePath -> Text -> IO ()
writeTracked project path text = do
    seen <- Map.member path <$> readIORef project.originals
    unless seen do
        exists <- doesFileExist path
        old <- if exists then Just <$> readFileUtf8 path else pure Nothing
        modifyIORef' project.originals (Map.insert path old)
    writeFileUtf8 path text


-- | Runs sqitch in the project with its resolved layout.
runSqitch :: Project -> [Text] -> IO ()
runSqitch project args = do
    let command = T.unwords (take 1 args)
    inherited <- getEnvironment
    let env = ("SQITCH_CONFIG", project.conf) : filter ((/= "SQITCH_CONFIG") . fst) inherited
    code <- timed [S.i|sqitch #{command}|] $ runProcess (setEnv env (setWorkingDir project.layout.root (proc "sqitch" (map T.unpack args))))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [S.i|sqitch #{T.unwords args} failed with exit code #{n}|]


-- | Adds the tag a rework needs, unless @change@ already has one after it.
tagBefore :: Project -> SqitchPlan -> Tagging -> Text -> IO ()
tagBefore project plan tagging change =
    unless (change `Set.member` plan.tagged) do
        tag <- either failWith pure (chooseTag plan tagging)
        runSqitch project ["tag", tag, "-n", "sqiff: before the changes it writes"]


-- | Writes each change with @sqitch add@ or @sqitch rework@, from create-migration's
-- templates in @templateDir@ when it is set, adding a tag first when a rework needs one.
-- Nothing is written when 'batchProblems' finds a problem.
writeUnits :: Project -> Maybe FilePath -> SqitchPlan -> Tagging -> [Unit] -> IO ()
writeUnits project templateDir' sqitchPlan tagging us = do
    requireSqitch
    case batchProblems sqitchPlan us of
        [] -> pure ()
        problems -> failWith (T.intercalate "; " problems)
    templateDir <- case (project.layout.root </>) <$> templateDir' of
        Just dir -> (\exists -> if exists then Just dir else Nothing) <$> doesDirectoryExist dir
        Nothing -> pure Nothing
    for_ (L.find (\u -> u.kind == ReworkChange && u.changeName `Set.notMember` sqitchPlan.tagged) us) \u ->
        tagBefore project sqitchPlan tagging u.changeName
    for_ us (writeUnit WriteContext{project, sqitchPlan, templateDir})


-- | What each change of a batch is written with, in 'writeUnits'.
data WriteContext = WriteContext
    { project :: Project
    , sqitchPlan :: SqitchPlan
    , templateDir :: Maybe FilePath
    -- ^ create-migration's templates, when the project has the directory.
    }


-- | Adds or reworks one change with sqitch, then writes its scripts over the ones sqitch made.
writeUnit :: WriteContext -> Unit -> IO ()
writeUnit ctx u = do
    let template = if isJust ctx.templateDir && u.kind == AddChange then templateOf u else Nothing
    case u.kind of
        AddChange -> runSqitch ctx.project (["add", u.changeName, "-n", u.note] <> foldMap (templateArgs ctx) template)
        ReworkChange -> runSqitch ctx.project ["rework", u.changeName, "-n", u.note]
    writeUnitScripts ctx template u


-- | The arguments that make @sqitch add@ start from a create-migration template.
templateArgs :: WriteContext -> TemplateUse -> [Text]
templateArgs ctx template =
    ["--template", template.name]
        <> foldMap (\d -> ["--template-directory", T.pack d]) ctx.templateDir
        <> concat [["-s", [S.i|#{k}=#{v}|]] | (k, v) <- template.variables]


-- | Writes a change's scripts once sqitch has made them. From a template, sqiff patches
-- what the template rendered or writes over it; without one, it writes the deploy, and
-- the revert and verify only for a new change, so a rework keeps the ones sqitch copied.
writeUnitScripts :: WriteContext -> Maybe TemplateUse -> Unit -> IO ()
writeUnitScripts ctx template u =
    case (template, shapeOf u) of
        (Just _, ValidateShape _ _) -> pure ()
        (Just _, AddColumnShape _ _ options) -> do
            rendered <- readFileUtf8 (path Deploy)
            let hazards = T.unlines [[S.i|-- #{hazardText h}|] | s <- u.deploy, h <- s.hazards]
            write (path Deploy)
                . T.replace "__your_type_here__ null" options
                . T.replace "\nalter table " [S.i|\n#{hazards}alter table |]
                $ rendered
        (Just use, AddIndexShape t index)
            | any (\s -> EveryPartition `elem` s.hazards) u.deploy -> do
                deploy >> revert
                write (path Verify) (verifyScript [S.i|-- Verify #{projectName}:#{name} on #{engine}|] u.deploy)
            | otherwise -> do
                deploy
                let ix = nameText index
                    templated = indexPrefix t <> fromMaybe ix (lookup "index_name" use.variables)
                for_ [Revert, Verify] \script' -> do
                    rendered <- readFileUtf8 (path script')
                    write (path script') (T.replace templated ix rendered)
        (Just _, OtherShape) -> deploy >> revert >> verify
        (Nothing, _) -> do
            deploy
            when (u.kind == AddChange) (revert >> verify)
  where
    layout = ctx.project.layout
    write = writeTracked ctx.project
    path script' = scriptPath layout script' u.changeName
    projectName = ctx.sqitchPlan.project
    engine = layout.engine
    name = u.changeName
    deploy = script Deploy [S.i|-- Deploy #{projectName}:#{name} to #{engine}|] u.deploy
    revert = script Revert [S.i|-- Revert #{projectName}:#{name} from #{engine}|] u.revert
    verify = write (path Verify) (verifyScript [S.i|-- Verify #{projectName}:#{name} on #{engine}|] u.deploy)
    script script' header steps
        | null steps = write (path script') [S.i|#{header}\n\n-- Nothing to revert.\n|]
        | otherwise = write (path script') (renderScript header u.placement steps)


-- | Reworks a change whose deploy script was already edited in place, putting the edit
-- back whatever happens; with 'PreviousFileFails' its revert does nothing.
reworkEdited :: Project -> Tagging -> Text -> Text -> Text -> PreviousFile -> IO ()
reworkEdited project tagging changeName note original previousFile = do
    let layout = project.layout
        path = scriptPath layout Deploy changeName
        revertPath = scriptPath layout Revert changeName
        engine = layout.engine
    sqitchPlan <- readPlan (planPath layout)
    edited <- readFileUtf8 path
    ( do
            writeTracked project path original
            tagBefore project sqitchPlan tagging changeName
            runSqitch project ["rework", changeName, "-n", note]
        )
        `finally` writeTracked project path edited
    let projectName = sqitchPlan.project
    when (previousFile == PreviousFileFails)
        $ writeTracked
            project
            revertPath
            [S.i|-- Revert #{projectName}:#{changeName} from #{engine}

-- The migrations before this change revert the table; the previous file fails on the edited table.
|]


-- | Moves the last plan entry for @name@, one the branch added, to the end of the plan.
moveToEnd :: Project -> Text -> IO ()
moveToEnd project name = do
    let path = planPath project.layout
    text <- readFileUtf8 path
    let ls = reverse (dropWhile (T.null . T.strip) (reverse (T.lines text)))
        isEntry l = entryName l == name
    case L.lastMaybe [k | (k, l) <- zip [0 :: Int ..] ls, isEntry l] of
        Just k | k < length ls - 1 -> writeTracked project path (T.unlines (take k ls <> drop (k + 1) ls <> take 1 (drop k ls)))
        _ -> pure ()
