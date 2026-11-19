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
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import RIO.Text.Partial qualified as TP
import Sqiff.App.Env (say)
import Sqiff.App.Options (Output (..), TableOptions (..))
import Sqiff.App.Table.Env
import Sqiff.Config (Config (..), Names (..), fill)
import Sqiff.Connection (failWith, withConnection)
import Sqiff.ConnectionString (ConnectionString, redacted)
import Sqiff.Git (showFile)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Rewrite (Rewrite (..), fileDrift, rewriteFile)
import Sqiff.Script (splitStatements)
import Sqiff.Sqitch (PreviousFile (..), reworkEdited, withProject, writeTracked)
import Sqiff.Sqitch.Check (Tagging (..))
import Sqiff.Sqitch.Layout (Script (..), planPath, scriptPath)
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
            . filterM (\n -> doesFileExist (scriptPath env.layout Deploy (fill env.config.names.index [("schema", nameText table.schema), ("table", nameText table.name), ("name", nameText n)])))
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
                sqitchPlan <- liftIO (readPlan (planPath env.layout))
                basePlan <- liftIO (parsePlan <$> showFile env.repo opts.base (planPath env.layout))
                now <- liftIO getCurrentTime
                let changeName = env.sqitchChange
                    entries p = length (filter (== changeName) p.changes)
                    tagging = Tagging{given = opts.tableTag, now}
                    alreadyReworked = entries sqitchPlan > entries basePlan
                liftIO $ withProject env.layout \project -> do
                    writeTracked project env.file rewrite.text
                    unless alreadyReworked
                        $ reworkEdited project tagging changeName [i|sqiff: #{changeName} as the database has it|] current PreviousFileRuns
                say
                    ( if alreadyReworked
                        then [i|rewrote #{changeName}; the branch already reworks it|]
                        else [i|rewrote and reworked #{changeName}|]
                    )


-- | The planned changes @live@'s sqitch registry lacks whose deploy scripts name the table.
undeployedTouching :: Connection.Connection -> RIO TableEnv [Text]
undeployedTouching live = do
    env <- ask
    let table = env.table
        touching name = do
            let path = scriptPath env.layout Deploy name
            exists <- doesFileExist path
            if not exists
                then pure (Just [i|#{name} (no deploy file)|])
                else
                    splitStatements <$> readFileUtf8 path <&> \case
                        Left _ -> Just [i|#{name} (its statements could not be read)|]
                        Right statements -> bool Nothing (Just name) (any (changesTableIn table) statements)
    planText <- readFileUtf8 (planPath env.layout)
    deployed <- liftIO (deployedChanges live (parsePlan planText).project)
    when (Set.null deployed) $ failWith "the live database's sqitch registry has no changes of this project"
    let pending = nubOrd [name | c@(name, _) <- plannedChanges planText, c `Set.notMember` deployed]
    catMaybes <$> traverse touching pending


-- | Whether @statement@ changes @table@: creates, alters, drops or grants on it, or creates
-- or comments on something of its own. A do block counts if it names the table. An
-- unqualified name counts as the table when its name matches: a change that may touch
-- it is treated as one that does.
changesTableIn :: QName -> Text -> Bool
changesTableIn table statement = case ws of
    "alter" : "table" : rest -> startsWithTable (dropWhile (`elem` ["if", "exists", "only"]) rest)
    "drop" : "table" : rest -> anyIn (dropWhile (`elem` ["if", "exists"]) rest)
    "comment" : "on" : "column" : n : _ -> isTable (T.dropWhileEnd (== '.') (T.dropWhileEnd (/= '.') (bare n)))
    "comment" : "on" : "table" : rest -> startsWithTable rest
    "comment" : "on" : _ -> afterOn
    verb : _ | verb `elem` ["grant", "revoke"] -> anyIn (dropWhile (== "table") (drop 1 (dropWhile (/= "on") ws)))
    "create" : rest
        | any (`elem` ["function", "procedure", "view", "materialized"]) (take 3 rest) -> False
        | "table" `elem` take 3 rest -> startsWithTable (dropWhile (`elem` ["if", "not", "exists"]) (drop 1 (dropWhile (/= "table") rest)))
        | otherwise -> afterOn
    "do" : _ -> named
    _ -> False
  where
    ws = T.words (T.toLower (withoutLeadingComments statement))
    bare = T.takeWhile (`notElem` ("(;," :: String))
    unquoted = T.filter (/= '"')
    qualifiedName = T.toLower (dotted table)
    plainName = T.toLower (nameText table.name)
    isTable n = let n' = unquoted (bare n) in n' == qualifiedName || (not ("." `T.isInfixOf` n') && n' == plainName)
    startsWithTable = \case
        n : _ -> isTable n
        [] -> False
    -- A list of names, up to the word after it.
    anyIn = any isTable . concatMap (filter (not . T.null) . T.split (== ',')) . takeWhile (`notElem` ["to", "from", "cascade", "restrict"])
    afterOn = startsWithTable (dropWhile (== "only") (drop 1 (dropWhile (/= "on") ws)))
    named = any mentions (nubOrd [qualifiedName, T.toLower (qualified table), plainName])
    mentions needle = any (\(before, rest) -> boundary (T.takeEnd 1 before) && boundary (T.take 1 (T.drop (T.length needle) rest))) (TP.breakOnAll needle (T.toLower statement))
    boundary t = maybe True (\(c, _) -> not (isAlphaNum c || c == '_' || c == '.')) (T.uncons t)
