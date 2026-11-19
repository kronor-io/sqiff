{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The table command: the migration that turns a table into what its edited deploy file
-- creates, printed or written with sqitch.
module Sqiff.App.Table (
    tableCommand,
    forTable,
    replayable,
) where

import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Hasql.Connection qualified as Connection
import RIO
import RIO.Directory (makeAbsolute)
import RIO.FilePath (makeRelative, normalise, (</>))
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.App.Dependents (Dependents (..), dependentsIn, dependentsScope)
import Sqiff.App.Env (say)
import Sqiff.App.Options (Comparison (..), MigrateOptions (..), Output (..), TableMode (..), TableOptions (..), renamesOf)
import Sqiff.App.Read (Connected (..), routineWords, withDatabase)
import Sqiff.App.Table.Env
import Sqiff.App.Table.Findings
import Sqiff.App.Table.Rewrite
import Sqiff.Catalog.PlpgsqlCheck (bodyErrors, notChecked)
import Sqiff.Config (Config (..), Names (..), fill, loadConfig, match)
import Sqiff.Connection (failWith)
import Sqiff.ConnectionString (ConnectionString)
import Sqiff.Definition hiding (DomainType)
import Sqiff.Dependencies (noDependencies)
import Sqiff.Diff (BodyText (..), Change (..))
import Sqiff.Git (findRepo, showFile)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (context, cycleMessage, invert, renderStep)
import Sqiff.Recipe (Step (..))
import Sqiff.Recreate (standardBody)
import Sqiff.Script (splitStatements, withoutTransactionControl)
import Sqiff.Sqitch (PreviousFile (..), moveToEnd, requireSqitch, reworkEdited, withProject, writeUnits)
import Sqiff.Sqitch.Check (Tagging (..))
import Sqiff.Sqitch.Layout (Layout (..), Script (..), findLayout, planPath, scriptPath)
import Sqiff.Sqitch.Plan (SqitchPlan (..), parsePlan, readPlan)
import Sqiff.Sqitch.Units (Unit (..), slug, units)
import Sqiff.Sql (replaceInCode)
import Sqiff.TableEdit (Baseline (..), Live (..), TableEdit (..), TypeChange (..), aside, compareTable, firstFailure, foreignKeyError, typeChangeFacts, withEditedTable)
import Sqiff.Timing (measure, timed)


-- | Prints the migration that turns the table into what its edited file creates, or
-- writes it with sqitch.
tableCommand :: FilePath -> ConnectionString -> TableOptions -> IO ()
tableCommand file db opts = do
    requireSqitch
    root <- makeAbsolute opts.project
    config <- loadConfig root
    layout <- findLayout root opts.top opts.planPath
    (absFile, changeName, table) <- locateTable config.names layout root file
    statements <- parseFile file "" =<< readFileUtf8 absFile
    let envWith repo original = TableEnv{opts, db, file, root, repo, config, layout, sqitchChange = changeName, table, statements, original}
    case opts.mode of
        RewriteFile liveFrom -> do
            repo <- findRepo root
            original <- timed "table.baseFile" $ showFile repo opts.base absFile
            runRIO (envWith repo original) (rewriteFromDatabase liveFrom)
        MigrateToFile migrate ->
            -- The database is described while git shows the file at the base revision when
            -- the comparison will need it; otherwise only once an edit asks for it.
            withDatabase (dependentsScope config.excludeSchemas) db \describe -> do
                let described = describe >>= \c -> (c,) <$> routineWords c.schema
                    needed = case migrate.comparison of
                        WithLiveTable _ -> True
                        WithBaseFile renames -> not (null renames)
                startingIf needed described \early -> do
                    repo <- findRepo root
                    original <- timed "table.baseFile" $ showFile repo opts.base absFile
                    let unchanged = Right statements == (withoutTransactionControl <=< splitStatements) original
                    startingIf (not needed && not unchanged) early \later ->
                        runRIO (envWith repo original) (compareAndReport migrate later)


-- | Runs @action@ with @get@, started now in the background when @now@ is set.
startingIf :: Bool -> IO a -> (IO a -> IO b) -> IO b
startingIf now get action
    | now = withAsync get (action . wait)
    | otherwise = action get


-- | The file's absolute path, its sqitch change and its table, by the table name pattern.
locateTable :: Names -> Layout -> FilePath -> FilePath -> IO (FilePath, Text, QName)
locateTable names layout root file = do
    absFile <- makeAbsolute file
    let deployDir = normalise (root </> layout.deployDir)
        relative = T.pack (makeRelative deployDir absFile)
        pattern = names.table
    changeName <- maybe (failWith [i|#{file} is not under #{deployDir}|]) pure (T.stripSuffix ".sql" relative)
    table <- case match pattern changeName of
        Just values
            | Just s <- lookup "schema" values
            , Just t <- lookup "name" values
            , not ("@" `T.isInfixOf` t) ->
                pure (qname s t)
        _ -> failWith [i|#{file} is not a table's own deploy file (#{pattern}.sql under #{deployDir})|]
    pure (absFile, changeName, table)


-- | Compares the table, checks what depends on the edit, and prints or writes the migration.
compareAndReport :: MigrateOptions -> IO (Connected, Map ObjectId BodyText) -> RIO TableEnv ()
compareAndReport migrate described = do
    env <- ask
    let opts = env.opts
        table = env.table
        renames = renamesOf migrate.comparison
    branchChanges <- branchChangesFor migrate.comparison
    let branchStatements = concatMap snd branchChanges
        normalised = T.toLower . T.unwords . T.words . T.dropWhileEnd (== ';') . T.strip . withoutLeadingComments
        pendingRenames = [r | r <- renames, normalised (renameSql table r) `notElem` map normalised branchStatements]
    for_ branchChanges \(n, _) -> say [i|-- already on this branch: #{n}|]
    baseline <- case migrate.comparison of
        WithLiveTable drops -> pure (LiveTable SameDatabase drops)
        WithBaseFile _ -> BaseFile . (<> (branchStatements <> map (renameSql table) pendingRenames)) <$> parseFile env.file (atBase opts) env.original
    timed "table.connect" . withScratch env.db $ \conn -> compareWith migrate described branchChanges pendingRenames baseline conn


-- | 'compareAndReport' once the baseline is known, on the scratch connection.
compareWith :: MigrateOptions -> IO (Connected, Map ObjectId BodyText) -> [(Text, [Text])] -> [(Identifier, Identifier)] -> Baseline -> Connection.Connection -> RIO TableEnv ()
compareWith migrate described branchChanges pendingRenames baseline conn = do
    env <- ask
    let opts = env.opts
        table = env.table
    edit <- timed "table.compare1" $ liftIO (compareTable conn baseline table env.statements)
    let renamedFrom = Set.fromList [Column table old | (old, _) <- renamesOf migrate.comparison]
    deps <- timed "table.dependentsIn" $ liftIO (dependentsIn env.config.excludeSchemas described (edit.changes <> Map.fromSet (const (Dropped (Printed ""))) (Set.map (ObjectKey Definition) renamedFrom)))
    bodies <- bodyResultsFor conn baseline deps
    let retyped = retypedColumns edit
    foreignKeyBreaks <- liftIO (foreignKeyBreaksFor conn deps retyped)
    typeFacts <- liftIO (typeFactsFor conn edit retyped)
    findings <- either (failWith . cycleMessage) pure $ findingsFor env migrate Evidence{edit, deps, bodies, retyped, foreignKeyBreaks, typeFacts, branchChanges, pendingRenames, renamedFrom}
    measure "table.planRender" (T.length (T.unlines (map renderStep findings.steps <> findings.usage)) + length findings.blocking)
    printLeftAlone edit
    case opts.output of
        PrintMigration -> printMigration findings
        WriteMigration -> writeMigration conn baseline edit findings


-- | This branch's changes for the table, with the statements the comparison can replay;
-- none against the database.
branchChangesFor :: Comparison -> RIO TableEnv [(Text, [Text])]
branchChangesFor comparison = do
    env <- ask
    let opts = env.opts
    case comparison of
        WithLiveTable _ -> pure []
        WithBaseFile _ -> do
            current <- liftIO (readPlan (planPath env.layout))
            basePlan <- liftIO (parsePlan <$> showFile env.repo opts.base (planPath env.layout))
            let before = Set.fromList basePlan.changes
            for (filter (\n -> n `Set.notMember` before && forTable env.config.names env.table n) (nubOrd current.changes)) \n -> do
                script <- readFileUtf8 (scriptPath env.layout Deploy n)
                (n,) . replayable <$> either (\e -> failWith [i|#{n}: #{e}|]) pure ((withoutTransactionControl <=< splitStatements) script)


-- | Checks the PL/pgSQL and string-bodied SQL functions that depend on the edit or name it,
-- before and after the edit.
bodyResultsFor :: Connection.Connection -> Baseline -> Dependents -> RIO TableEnv BodyResults
bodyResultsFor conn baseline deps = do
    env <- ask
    let table = env.table
        definitionOf a = printed <$> Map.lookup (definitionKey a) deps.dbSchema
        takesRowType = \case
            Routine _ _ args -> any ((`elem` [qualified table, qualified table <> "[]"]) . typeText) args
            _ -> False
        routines = Set.toList (Set.filter (\a -> isRoutine a && not (takesRowType a)) (fold deps.named <> fold deps.exact))
        plpgsql = [a | a <- routines, Just def <- [definitionOf a], "LANGUAGE plpgsql" `T.isInfixOf` def]
        sqlBodies = [a | a <- routines, Just def <- [definitionOf a], "LANGUAGE sql" `T.isInfixOf` def, not (standardBody def)]
        signature a = case a of
            Routine _ q args -> let list = T.intercalate ", " (map typeText args) in [i|#{qualified q}(#{list})|]
            _ -> render a
        bySignature = Map.fromList [(signature a, a) | a <- plpgsql <> sqlBodies]
        check made = timed [i|table.bodyErrors n=#{Map.size bySignature}|] $ bodyErrors conn (nameText aside) (qualified table) made (map signature plpgsql) (map signature sqlBodies)
        rowTypeCall message = "function " `T.isPrefixOf` message && any (\end -> (qualified table <> end) `T.isInfixOf` message) [")", ","]
    results <-
        if null plpgsql && null sqlBodies
            then pure Nothing
            else
                timed "table.compare2"
                    . liftIO
                    $ withEditedTable conn baseline table env.statements (check deps.dbTempTables) (\before -> (\after -> (,) <$> before <*> after) <$> check (pure Nothing))
    let checked = Set.filter (\(_, _, message) -> not (rowTypeCall message)) . uncurry (flip Set.difference) <$> results
        unchecked = Set.fromList [s | Just (_, after) <- [results], (s, _, message) <- Set.toList after, notChecked `T.isPrefixOf` message]
        breaking = Map.fromListWith (\_ earlier -> earlier) [(a, message) | (s, _, message) <- maybe [] Set.toList checked, Just a <- [Map.lookup s bySignature]]
        passed = Set.fromList [a | isJust checked, a <- plpgsql <> sqlBodies, a `Map.notMember` breaking, signature a `Set.notMember` unchecked, Just def <- [definitionOf a], not ("execute" `T.isInfixOf` T.toLower def)]
    pure BodyResults{breaking, passed}


-- | The foreign keys on retyped columns that cannot be rebuilt with the new types, with
-- PostgreSQL's error.
foreignKeyBreaksFor :: Connection.Connection -> Dependents -> [Retyped] -> IO (Map ObjectId [(ObjectId, Text)])
foreignKeyBreaksFor conn deps retyped =
    timed "table.foreignKeys"
        $ Map.fromListWith (<>)
        . catMaybes
        <$> for
            [ (addr, k, types)
            | addr <- map retypedObject retyped
            , k@(TableConstraint onTable _) <- Set.toList (Map.findWithDefault Set.empty addr deps.exact)
            , Just (ConstraintDefinition def) <- [Map.lookup (definitionKey k) deps.dbSchema]
            , Just types <- [foreignKeyTypes typeIn onTable def]
            ]
            \(addr, k, types) -> fmap (\err -> (addr, [(k, err)])) <$> foreignKeyError conn types
  where
    typeIn t c = fmap typeText case [r.newType | r <- retyped, r.table == t, r.column == c] of
        ty : _ -> Just ty
        [] -> (.colType) <$> (asColumn =<< Map.lookup (definitionKey (Column t c)) deps.dbSchema)


-- | For each retyped column, whether the change rewrites the table and whether the new
-- type is a domain.
typeFactsFor :: Connection.Connection -> TableEdit -> [Retyped] -> IO (Map ObjectId TypeChange)
typeFactsFor conn edit retyped =
    timed "table.typeChangeFacts" $ Map.fromList . catMaybes <$> for retyped \r ->
        let addr = retypedObject r
         in case asColumn =<< Map.lookup (definitionKey addr) edit.current of
                Just o -> fmap (addr,) <$> typeChangeFacts conn (typeText o.colType) (typeText r.newType)
                Nothing -> pure Nothing


-- | The differences the comparison leaves alone, with the reason; an object's comment
-- goes with it.
printLeftAlone :: MonadIO m => TableEdit -> m ()
printLeftAlone edit = do
    let leftAlone = Set.fromList [k.object | (k, _) <- edit.ignored, k.aspect == Definition]
    for_ [(k, reason) | (k, reason) <- edit.ignored, not (k.aspect == Comment && k.object `Set.member` leftAlone)] \(k, reason) -> do
        let name = render k.object <> if k.aspect == Privileges then " (privileges)" else ""
        say [i|-- left alone: #{name}: #{reason}|]


printMigration :: MonadIO m => Findings -> m ()
printMigration f = do
    let same = f.same
    for_ f.usage \line -> say [i|-- #{line}|]
    unless (null f.usage) $ say ""
    if null f.steps
        then say [i|-- #{same}|]
        else for_ f.steps (say . renderStep)


-- | Adds the migration with sqitch and reworks the table's change.
writeMigration :: Connection.Connection -> Baseline -> TableEdit -> Findings -> RIO TableEnv ()
writeMigration conn baseline edit f = do
    env <- ask
    let opts = env.opts
        table = env.table
        changeName = env.sqitchChange
        same = f.same
    when (null f.steps) do
        say [i|nothing to write: #{same}|]
        exitSuccess
    for_ f.usage (liftIO . TIO.hPutStrLn stderr)
    unless (null f.blocking) do
        let n = length f.blocking
        failWith [i|#{n} dependencies on what the edit drops or retypes; change or drop the dependents first|]
    unless (null f.renameLike) $ failWith "a column is dropped and another added with the same type; say whether it is a rename with --rename or --drop-and-add"
    sqitchPlan <- timed "write.readPlan" $ liftIO (readPlan (planPath env.layout))
    basePlan <- timed "write.basePlan" $ liftIO (parsePlan <$> showFile env.repo opts.base (planPath env.layout))
    now <- liftIO getCurrentTime
    let inverseCtx = f.withViews (context Map.empty edit.desired edit.current noDependencies (Map.map invert edit.changes))
        isIndex s = case s.object of Index _ -> True; _ -> False
        written = units env.config.names isIndex inverseCtx edit.changes sqitchPlan now f.steps
        tagging = Tagging{given = opts.tableTag, now}
        entries p = length (filter (== changeName) p.changes)
        alreadyReworked = entries sqitchPlan > entries basePlan
    previous <- parseFile env.file (atBase opts) env.original
    oldFileFails <- timed "table.compare3" $ liftIO (withEditedTable conn baseline table env.statements (pure ()) (const (firstFailure conn previous)))
    let baseRev = opts.base
    for_ oldFileFails \err ->
        say [i|the file at #{baseRev} fails on the edited table (#{err}); the rework's revert does nothing and the migrations revert the table|]
    liftIO $ withProject env.layout \project -> do
        timed "write.writeUnits" $ writeUnits project env.config.templates sqitchPlan tagging written
        if alreadyReworked
            then moveToEnd project changeName
            else
                timed "write.reworkEdited"
                    $ reworkEdited project tagging changeName [i|sqiff: #{changeName} as edited|] env.original (maybe PreviousFileRuns (const PreviousFileFails) oldFileFails)
    for_ written \u -> let name = u.changeName in say [i|added #{name}|]
    say
        ( if alreadyReworked
            then [i|moved the branch's rework of #{changeName} after the new migrations; if this branch is deployed locally, revert it to before the rework first|]
            else [i|reworked #{changeName}|]
        )


-- | Whether a sqitch change is about the table, by the names sqiff gives: one of its
-- indexes, or <schema>.<table> in a migration's name, which is lower-cased and has other
-- characters made dashes ('slug').
forTable :: Names -> QName -> Text -> Bool
forTable names t n =
    let indexes = fill names.index [("schema", nameText t.schema), ("table", nameText t.name), ("name", "")]
        padded = [i|#{n}-|] :: Text
     in indexes
            `T.isPrefixOf` n
            || or
                [ [i|#{before}#{key}#{after}|] `T.isInfixOf` padded
                | key <- nubOrd [dotted t, slug (dotted t)]
                , before <- ["-", "/" :: Text]
                , after <- [".", "-" :: Text]
                ]


-- | A migration's statements as they can run inside the comparison's transaction:
-- without session settings, and with concurrent index builds made plain.
replayable :: [Text] -> [Text]
replayable = map (replaceInCode "concurrently " "" . replaceInCode "CONCURRENTLY " "") . filter (not . setting)
  where
    setting s = any (`T.isPrefixOf` T.toLower (withoutLeadingComments s)) ["set ", "reset ", "select set_config("]
