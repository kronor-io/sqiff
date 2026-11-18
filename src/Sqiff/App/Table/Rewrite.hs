{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The table command's @--rewrite-file@: the deploy file rewritten from the table as a
-- database has it.
module Sqiff.App.Table.Rewrite (
    rewriteFromDatabase,
    changesTableIn,
) where

import Data.Char (isAlphaNum)
import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Hasql.Connection qualified as Connection
import RIO
import RIO.Directory (doesFileExist)
import RIO.FilePath ((<.>), (</>))
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import RIO.Text.Partial qualified as TP
import Sqiff.App.Env (say)
import Sqiff.App.Options (Output (..), TableOptions (..))
import Sqiff.App.Table.Env
import Sqiff.Config (Config (..), Layout (..), Names (..), fill)
import Sqiff.Connection (failWith, withConnection)
import Sqiff.ConnectionString (ConnectionString, redacted)
import Sqiff.Git (showFile)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Rewrite (Rewrite (..), fileDrift, rewriteFile)
import Sqiff.Script (splitStatements)
import Sqiff.Sqitch (PreviousFile (..), defaultTag, reworkEdited)
import Sqiff.Sqitch.Plan (SqitchPlan (..), deployedChanges, parsePlan, plannedChanges, readPlan)
import Sqiff.TableEdit (Baseline (..), Drops (..), Live (..), TableEdit (..), columnOrder, compareTable, readOnly)
import Sqiff.Timing (timed)


-- | Prints the file rewritten to create the table as the database has it, or writes it
-- and reworks the change.
--
-- The table is read from @liveFrom@, read-only, when it is given.
rewriteFromDatabase :: Maybe ConnectionString -> RIO TableEnv ()
rewriteFromDatabase liveFrom = do
    env <- ask
    current <- readFileUtf8 env.file
    withScratch env.db \conn -> case liveFrom of
        Nothing -> rewriteFrom liveFrom current conn (SameDatabase, conn)
        Just name -> withConnection name \c -> do
            liftIO (readOnly c)
            rewriteFrom liveFrom current conn (OtherDatabase c, c)


-- | 'rewriteFromDatabase' on the scratch connection, with the table as @live@ has it.
rewriteFrom :: Maybe ConnectionString -> Text -> Connection.Connection -> (Live, Connection.Connection) -> RIO TableEnv ()
rewriteFrom liveFrom current conn (live, liveConn) = do
    env <- ask
    let opts = env.opts
        table = env.table
        deployDir = env.root </> env.layout.topDir </> "deploy"
        liveDb = redacted (fromMaybe env.db liveFrom)
    order <- liftIO (columnOrder liveConn table)
    when (null order) $ failWith [i|#{qualified table} is not in #{liveDb}; there is nothing to rewrite the file from|]
    for_ liveFrom \_ -> do
        pending <- undeployedTouching liveConn
        let n = length pending
            names = T.intercalate ", " pending
        unless (null pending) $ failWith [i|#{liveDb} has not deployed #{n} planned changes that touch #{qualified table}, and a rewrite from it would undo them: #{names}|]
    edit <- timed "rewrite.compare" $ liftIO (compareTable conn (LiveTable live ReportDrops) table env.statements)
    ownFiles <-
        fmap Set.fromList
            . filterM (\n -> doesFileExist (deployDir </> T.unpack (fill env.config.names.index [("schema", nameText table.schema), ("table", nameText table.name), ("name", nameText n)]) <.> "sql"))
            $ [q.name | k <- Map.keys edit.current, Index q <- [k.object]]
    let path = env.file
    rewrite <- either (\e -> failWith [i|#{path}: #{e}|]) pure (rewriteFile table ownFiles order edit.current edit.desired current)
    remaining <-
        if rewrite.text == current
            then pure (fileDrift ownFiles edit.desired edit.current)
            else do
                rewritten <- parseFile env.file " as rewritten" rewrite.text
                check <- timed "rewrite.check" $ liftIO (compareTable conn (LiveTable live ReportDrops) table rewritten)
                pure (fileDrift ownFiles check.desired check.current)
    let still = [render k.object <> (if k.aspect == Comment then " (comment)" else "") | k <- Map.keys remaining]
    for_ still \s -> liftIO (TIO.hPutStrLn stderr [i|-- still differs: #{s}|])
    case opts.output of
        PrintMigration -> do
            case (rewrite.text == current, null still) of
                (True, True) -> say "-- the file matches the table"
                (True, False) -> say "-- the file is unchanged; the differences above are left to you"
                (False, _) -> liftIO (TIO.putStr rewrite.text)
            unless (null still) $ exitWith (ExitFailure 1)
        WriteMigration
            | not (null still) -> let n = length still in failWith [i|the rewritten file still differs from the table in #{n} places; nothing written|]
            | rewrite.text == current -> say "nothing to write: the file matches the table"
            | otherwise -> do
                sqitchPlan <- liftIO (readPlan (env.root </> env.layout.planFile))
                basePlan <- liftIO (parsePlan <$> showFile env.repo opts.base (env.root </> env.layout.planFile))
                now <- liftIO getCurrentTime
                let changeName = env.sqitchChange
                    entries p = length (filter (== changeName) p.changes)
                    tagName = fromMaybe (defaultTag now) opts.tableTag
                writeFileUtf8 env.file rewrite.text
                if entries sqitchPlan > entries basePlan
                    then say [i|rewrote #{changeName}; the branch already reworks it|]
                    else do
                        liftIO $ reworkEdited env.root env.layout.topDir sqitchPlan tagName changeName [i|sqiff: #{changeName} as the database has it|] current PreviousFileRuns
                        say [i|rewrote and reworked #{changeName}|]


-- | The planned changes @live@'s sqitch registry lacks whose deploy scripts name the table.
undeployedTouching :: Connection.Connection -> RIO TableEnv [Text]
undeployedTouching live = do
    env <- ask
    let deployDir = env.root </> env.layout.topDir </> "deploy"
        needle = T.toLower (qualified env.table)
        touching name = do
            let path = deployDir </> T.unpack name <.> "sql"
            exists <- doesFileExist path
            if not exists
                then pure (Just [i|#{name} (no deploy file)|])
                else
                    splitStatements <$> readFileUtf8 path <&> \case
                        Left _ -> Just [i|#{name} (its statements could not be read)|]
                        Right statements -> bool Nothing (Just name) (any (changesTableIn needle) statements)
    planText <- readFileUtf8 (env.root </> env.layout.planFile)
    deployed <- liftIO (deployedChanges live (parsePlan planText).project)
    when (Set.null deployed) $ failWith "the live database's sqitch registry has no changes of this project"
    let pending = nubOrd [name | c@(name, _) <- plannedChanges planText, c `Set.notMember` deployed]
    catMaybes <$> traverse touching pending


-- | Whether @statement@ changes the table @needle@ (lower-case, qualified): creates or alters
-- it, or creates or comments on something of its own. A do block counts if it names the table.
changesTableIn :: Text -> Text -> Bool
changesTableIn needle statement = case ws of
    "alter" : "table" : rest -> target (dropWhile (`elem` ["if", "exists", "only"]) rest)
    "comment" : "on" : "column" : n : _ -> T.dropWhileEnd (/= '.') (bare n) == needle <> "."
    "comment" : "on" : "table" : rest -> target rest
    "comment" : "on" : _ -> afterOn
    "create" : rest
        | any (`elem` ["function", "procedure", "view", "materialized"]) (take 3 rest) -> False
        | "table" `elem` take 3 rest -> target (dropWhile (`elem` ["if", "not", "exists"]) (drop 1 (dropWhile (/= "table") rest)))
        | otherwise -> afterOn
    "do" : _ -> named
    _ -> False
  where
    ws = T.words (T.toLower (withoutLeadingComments statement))
    bare = T.takeWhile (`notElem` ("(;," :: String))
    target = \case
        n : _ -> bare n == needle
        [] -> False
    afterOn = target (dropWhile (== "only") (drop 1 (dropWhile (/= "on") ws)))
    named = any (\(_, rest) -> maybe True (\(c, _) -> not (isAlphaNum c || c == '_')) (T.uncons (T.drop (T.length needle) rest))) (TP.breakOnAll needle (T.toLower statement))
