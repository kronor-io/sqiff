{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SchemaDiff (main) where

import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Data.Char (isAlphaNum)
import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Hasql.Connection qualified as Connection
import Kronor.SchemaDiff.Baseline (ensureBaseline)
import Kronor.SchemaDiff.Body (BodyCheck, checkBodies, sameDefinition)
import Kronor.SchemaDiff.Catalog (Address (..), Addresses, Aspect (..), Edge (..), ObjectKey (..), Partitions (..), PlFunction (..), PlFunctions (..), Schema, Skipped, UserError (..), analyseBodies, bodyErrors, bodyUses, connect, dependenciesFrom, dependenciesOn, describeOn, edgesBetween, failWith, notChecked, plpgsqlFunctions, relationColumns, render, tempTableStatements)
import Kronor.SchemaDiff.Diff (BodyText (..), Change (..), affected, bodiesToAnalyse, bodyText, dependentsBy, diff, reachedFrom)
import Kronor.SchemaDiff.Folder (Existing (..), writeFolder)
import Kronor.SchemaDiff.Git (Repo, findRepo, showFile)
import Kronor.SchemaDiff.Object (Object (..), QName (..), fromAddress, qualified, quoteIdent)
import Kronor.SchemaDiff.Plan (context, invert, plan, planWith, renderStep, topoOrder)
import Kronor.SchemaDiff.Recipe (ColumnDef (..), Context (viewsAround), Hazard (..), Phase (..), Step (..), Txn (..), parseColumn, recipe, referencedTable, revokeGranted)
import Kronor.SchemaDiff.Rewrite (Rewrite (..), fileDrift, rewriteFile)
import Kronor.SchemaDiff.Script (splitStatements, withoutTransactionControl)
import Kronor.SchemaDiff.Sqitch (PreviousFile (..), SqitchPlan (..), Unit (..), UnitKind (..), deployedChanges, moveToEnd, parsePlan, plannedChanges, readPlan, requireSqitch, reworkEdited, units, writeUnits)
import Kronor.SchemaDiff.TableEdit (Baseline (..), Drops (..), Live (..), TableEdit (..), TableRewrite (..), TargetType (..), TypeChange (..), aside, columnOrder, compareTable, firstFailure, foreignKeyError, readOnly, requireScratch, typeChangeFacts, withEditedTable)
import Kronor.SchemaDiff.Timing (measure, timed)
import Options.Applicative qualified as O
import RIO
import RIO.Directory (doesFileExist, makeAbsolute)
import RIO.FilePath (makeRelative, (<.>), (</>))
import RIO.HashSet qualified as HS
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.NonEmpty (nonEmpty)
import RIO.Set qualified as Set
import RIO.Text qualified as T
import RIO.Text.Partial qualified as TP
import System.Environment (lookupEnv)


data Scope = Scope
    { excluded :: [Text]
    , partitions :: Partitions
    , bodyCheck :: BodyChecks
    }


-- | Whether SQL-standard function bodies are recreated on @--to@ to compare them.
data BodyChecks = CheckBodies | SkipBodyChecks
    deriving stock (Eq)


-- | Which edges a command uses.
data Needed
    = -- | Edges from @pg_depend@, and the body edges 'affected' can reach from these.
      ReachableFrom (Set Address)
    | -- | Edges from @pg_depend@, and edges from the bodies of these functions only.
      BodiesOf (Set Address)
    | -- | None; plpgsql_check still lists the functions it cannot analyse.
      NoEdges


-- | Whether a command lists the differences or plans the migration.
data Purpose = ForDiff | ForPlan
    deriving stock (Eq)


-- | The edges a command needs, given the objects that differ between the databases.
neededFor :: Purpose -> Map ObjectKey Change -> Needed
neededFor purpose changes
    | Map.null changes = NoEdges
    | purpose == ForPlan = BodiesOf (Set.fromList [k.address | k <- Map.keys changes])
    | null starts = NoEdges
    | otherwise = ReachableFrom (Set.fromList starts)
  where
    starts = [k.address | (k, c) <- Map.toList changes, k.aspect == Definition, removesOrChanges c]
    removesOrChanges = \case Added _ -> False; _ -> True


-- | Edges from @pg_depend@ and, when plpgsql_check is installed, from PL/pgSQL bodies,
-- with notes for stderr. @label@ names the database in the notes.
edgesOf :: Text -> Scope -> Needed -> Connected -> IO ([Edge], [Text])
edgesOf label scope needed connected@(Connected conn extra _ addresses made) = timed [i|edgesOf #{label}|] case needed of
    ReachableFrom starts -> let Connected _ _ schema _ _ = connected in reachableEdges label scope starts connected (routineWords schema)
    _ -> do
        let wantedIds wanted = [ids | (ids, addr) <- Map.toList addresses, addr `Set.member` wanted]
            only = case needed of
                BodiesOf wanted -> Just (Set.fromList [oid | (classid, oid, _) <- wantedIds wanted, classid == pgProc])
                _ -> Just Set.empty
        (edges, body) <-
            concurrently
                ( timed [i|edgesOf.dependOn #{label}|] case needed of
                    BodiesOf wanted -> dependenciesOn conn addresses (wantedIds wanted)
                    _ -> pure []
                )
                (timed [i|edgesOf.bodyUses #{label}|] $ fmap (first (edgesBetween addresses)) <$> bodyUses (fromMaybe (conn :| []) (nonEmpty extra)) scope.excluded only made)
        pure (edges <> maybe [] fst body, bodyNotes label (snd <$> body))


pgProc :: Int64
pgProc = 1255


pgClass :: Int64
pgClass = 1259


bodyNotes :: Text -> Maybe Skipped -> [Text]
bodyNotes label = \case
    Nothing -> ["schema-diff: plpgsql_check is not installed; PL/pgSQL body dependencies are left out"]
    Just [] -> []
    Just skipped ->
        let n = length skipped
         in [i|schema-diff: plpgsql_check could not analyse #{n} PL/pgSQL functions on #{label}; their body dependencies are left out:|]
                : [[i|  #{name}: #{reason}|] | (name, reason) <- skipped]


-- | The @pg_depend@ edges reachable from @starts@, and the body edges of the PL/pgSQL
-- functions 'bodiesToAnalyse' picks, repeated until nothing new is reached or picked.
reachableEdges :: Text -> Scope -> Set Address -> Connected -> IO (Map Address BodyText) -> IO ([Edge], [Text])
reachableEdges label scope starts (Connected conn extra schema addresses made) splitWords = timed [i|reachableEdges #{label}|] do
    let idsOf = Map.fromListWith (<>) [(addr, [ids]) | (ids, addr) <- Map.toList addresses]
        idsFor = concatMap (\a -> Map.findWithDefault [] a idsOf) . Set.toList
        columnsOf = Map.fromListWith (<>) [(rel, [addr]) | ((classid, objid, sub), addr) <- Map.toList addresses, classid == pgClass, sub > 0, Just rel <- [Map.lookup (pgClass, objid, 0) addresses]]
        withColumns xs = xs <> Set.fromList (concatMap (\a -> Map.findWithDefault [] a columnsOf) (Set.toList xs))
    (edges, functions) <-
        concurrently
            (timed "reach.dependFrom0" $ dependenciesFrom conn addresses (idsFor starts))
            ( timed "reach.functionsAndWords"
                $ plpgsqlFunctions (fromMaybe conn (listToMaybe extra)) scope.excluded
                >>= traverse \fs -> do
                    split_ <- splitWords
                    let wordsAt addr = fromMaybe (bodyText (Map.findWithDefault "" (ObjectKey Definition addr) schema)) (Map.lookup addr split_)
                        split part = do
                            let described =
                                    [ (f, addr, wordsAt addr, trigger)
                                    | f <- part
                                    , let trigger = f.relid >>= \r -> Map.lookup (pgClass, r, 0) addresses
                                    , Just addr <- [Map.lookup (pgProc, f.oid, 0) addresses]
                                    ]
                            _ <- evaluate (foldl' (\n (_, _, b, _) -> n + HS.size b.wordSet) (0 :: Int) described)
                            pure described
                        parts = [[f | (n, f) <- zip [0 :: Int ..] fs.checkable, n `mod` 8 == k] | k <- [0 .. 7]]
                    described <- concat <$> pooledMapConcurrently split parts
                    pure (fs, described)
            )
    case functions of
        Nothing -> pure (edges, bodyNotes label Nothing)
        Just (fs, described) -> do
            let position = Map.fromList (zip [f.name | f <- fs.checkable] [0 :: Int ..])
                implicit = [a | o <- fs.implicit, Just a <- [Map.lookup (pgProc, o, 0) addresses]]
                go explored pgEdges done bodyEdges failed = do
                    let dependents = dependentsBy (pgEdges <> bodyEdges)
                        frontier = (starts <> reachedFrom dependents starts) `Set.difference` explored
                    more <- timed "reach.dependFrom" $ dependenciesFrom conn addresses (idsFor frontier)
                    let pgEdges' = pgEdges <> more
                        explored' = explored <> withColumns (frontier <> Set.fromList (map (.dependent) more))
                        dependents' = dependentsBy (pgEdges' <> bodyEdges)
                        picked
                            | any (`Set.member` (starts <> reachedFrom dependents' starts)) implicit = [f | (f, _, _, _) <- described]
                            | otherwise = bodiesToAnalyse dependents' starts described
                    case [f | f <- picked, f.oid `Set.notMember` done] of
                        []
                            | Set.null frontier -> pure (pgEdges' <> bodyEdges, failed)
                            | otherwise -> go explored' pgEdges' done bodyEdges failed
                        todo -> do
                            (uses, failed') <- timed [i|reach.analyseBodies n=#{length todo}|] $ analyseBodies (conn :| extra) fs{tempTables = made >>= maybe fs.tempTables pure} todo
                            go explored' pgEdges' (done <> Set.fromList (map (.oid) todo)) (bodyEdges <> edgesBetween addresses uses) (failed <> failed')
            (allEdges, failed) <- go (withColumns (starts <> Set.fromList (map (.dependent) edges))) edges Set.empty [] []
            pure (allEdges, bodyNotes label (Just (fs.unchecked <> L.sortOn (\(name, _) -> Map.lookup name position) failed)))


-- | The words of every function's and procedure's definition, split on every core.
routineWords :: Schema -> IO (Map Address BodyText)
routineWords schema = do
    let routines = [(k.address, def) | (k, def) <- Map.toList schema, k.aspect == Definition, k.address.objectType `elem` ["function", "procedure"]]
        split part = do
            let split_ = [(addr, bodyText def) | (addr, def) <- part]
            _ <- evaluate (foldl' (\n (_, b) -> n + HS.size b.wordSet + T.length b.lowered) (0 :: Int) split_)
            pure split_
        parts = [[r | (n, r) <- zip [0 :: Int ..] routines, n `mod` 8 == k] | k <- [0 .. 7]]
    Map.fromList . concat <$> pooledMapConcurrently split parts


-- | How 'dependentsIn' describes the database.
dependentsScope :: Scope
dependentsScope = Scope{excluded = ["hdb_catalog"], partitions = SkipPartitions, bodyCheck = SkipBodyChecks}


-- | For each dropped or changed definition, its dependents by @pg_depend@ and PL/pgSQL
-- bodies, and the other functions whose text names it.
dependentsIn :: IO (Connected, Map Address BodyText) -> Map ObjectKey Change -> IO Dependents
dependentsIn described changes
    | Set.null starts = pure (Dependents Map.empty Map.empty Map.empty [] (pure Nothing))
    | otherwise = do
        (connected@(Connected conn _ schema _ made), split_) <- timed "deps.describeDatabase" described
        (edges, notes) <- timed "deps.reachableEdges" $ reachableEdges "--db" dependentsScope starts connected (pure split_)
        for_ notes (TIO.hPutStrLn stderr)
        let known = Set.fromList [k.address | k <- Map.keys schema, k.aspect == Definition]
            exact = affected known edges changes
            viewsOf addr = [(s, v) | a <- Set.toList (Map.findWithDefault Set.empty addr exact), a.objectType `elem` ["view", "materialized view"], [s, v] <- [a.objectNames]]
        measure "deps.affected" (Map.foldl' (\n s -> n + Set.size s) (0 :: Int) exact)
        viewColumns <- timed "deps.relationColumns" $ relationColumns conn (Set.toList (Set.fromList (concatMap viewsOf (Set.toList starts))))
        let routines = Map.toList split_
            skip = Set.fromList (map (.address) (Map.keys changes)) <> fold exact
            throughViews column addr =
                [ T.toLower s : T.toLower v : [T.toLower c | Just c <- [column], keepsName c]
                | (s, v) <- viewsOf addr
                , let keepsName c = c `Set.member` Map.findWithDefault Set.empty (s, v) viewColumns
                ]
            named addr = case (addr.objectType, addr.objectNames) of
                ("table column", [_, t, c]) -> [T.toLower t, T.toLower c] : throughViews (Just c) addr
                (kind, [_, t]) | kind `elem` relationKinds, droppedHere addr -> [T.toLower t] : throughViews Nothing addr
                _ -> []
            textual =
                Map.filter (not . Set.null)
                    $ Map.fromList
                        [ (addr, Set.fromList [r | (r, body) <- routines, any (all (`HS.member` body.wordSet)) wordSets, r `Set.notMember` skip])
                        | addr <- Set.toList starts
                        , let wordSets = named addr
                        , not (null wordSets)
                        ]
        measure "deps.textMatch" (Map.foldl' (\n s -> n + Set.size s) (0 :: Int) textual)
        pure (Dependents exact textual schema edges made)
  where
    relationKinds = ["table", "view", "materialized view", "foreign table"]
    droppedHere addr = case Map.lookup (ObjectKey Definition addr) changes of
        Just (Dropped _) -> True
        _ -> False
    starts = Set.fromList [k.address | (k, c) <- Map.toList changes, k.aspect == Definition, removesOrChanges c]
    removesOrChanges = \case Added _ -> False; _ -> True


-- | What depends on an edit's dropped or changed objects, in the database it runs on.
data Dependents = Dependents
    { exact :: Map Address (Set Address)
    , named :: Map Address (Set Address)
    , dbSchema :: Schema
    , dbEdges :: [Edge]
    , dbTempTables :: IO (Maybe Text)
    -- ^ The statements that make plpgsql_check's temp tables there, from 'tempTableStatements'.
    }


-- | For each retyped column, the statements that drop the views, policies, triggers and
-- routines that hold it, and the ones that create them again.
viewsAroundTypeChanges :: Dependents -> [(QName, Text, Address)] -> Map (QName, Text) (Set Address, ([Text], [Text]))
viewsAroundTypeChanges deps retyped =
    Map.fromList
        [ ((t, c), (members, (map dropView (reverse order), concatMap createView order)))
        | (t, c, addr) <- retyped
        , let closure = Map.findWithDefault Set.empty addr deps.exact
              onColumn = Set.filter (\a -> a.objectType `elem` ["policy", "trigger"] && any (\e -> e.dependent == a && e.referenced == addr) deps.dbEdges) closure
              members = held addr closure (Set.filter isView closure <> onColumn)
              order = topoOrder members [(d, r) | e <- deps.dbEdges, let d = relation e.dependent, let r = relation e.referenced, d `Set.member` members, r `Set.member` members]
        , not (Set.null members)
        ]
  where
    isView a = a.objectType `elem` ["view", "materialized view"]
    isRoutine a = a.objectType `elem` ["function", "procedure"]
    held column closure members =
        let views = Set.filter isView members
            holds r = case Map.lookup (ObjectKey Definition r) deps.dbSchema of
                Just def
                    | standardBody def -> any (\e -> e.dependent == r && (e.referenced == column || relation e.referenced `Set.member` members)) deps.dbEdges
                    | otherwise -> any (\v -> qualifiedAddress v `T.isInfixOf` signature def) views
                Nothing -> False
            more = Set.filter (\r -> isRoutine r && r `Set.notMember` members && holds r) closure
         in if Set.null more then members else held column closure (members <> more)
    signature = T.unlines . takeWhile (\l -> not ("AS " `T.isPrefixOf` T.stripStart l || "BEGIN ATOMIC" `T.isInfixOf` l)) . T.lines
    relation a = case (a.objectType, a.objectNames) of
        (kind, [s, t, _]) | Just rel <- T.stripSuffix " column" kind -> Address rel [s, t] []
        _ -> a
    ctx0 = context Map.empty Map.empty Map.empty [] Map.empty
    dropView a
        | isRoutine a = let kind = a.objectType; args = T.intercalate ", " a.objectArgs in [i|drop #{kind} #{qualifiedAddress a}(#{args})|]
        | [s, t, n] <- a.objectNames, a.objectType `elem` ["policy", "trigger"] = let kind = a.objectType in [i|drop #{kind} #{quoteIdent n} on #{qualified (QName s t)}|]
        | otherwise = let kind = a.objectType in [i|drop #{kind} #{qualifiedAddress a}|]
    qualifiedAddress a = case a.objectNames of
        [s, n] -> qualified (QName s n)
        ns -> T.intercalate "." ns
    stepsFor key c = map (.sql) (recipe ctx0 key c)
    createView a =
        concat
            [ [revokeGranted (qualifiedAddress a) | aspect == Privileges, isView a] <> stepsFor key (Added def)
            | aspect <- [Definition, Privileges]
            , let key = ObjectKey aspect a
            , Just def <- [Map.lookup key deps.dbSchema]
            ]
            <> [ def
               | (k, def) <- Map.toList deps.dbSchema
               , k.aspect == Definition
               , case fromAddress k.address of
                    Index _ -> [i| ON #{qualifiedAddress a} |] `T.isInfixOf` def
                    Trigger t _ -> [t.schema, t.name] == a.objectNames
                    _ -> False
               ]
            <> concat
                [ stepsFor k (Added def)
                | (k, def) <- Map.toList deps.dbSchema
                , k.aspect == Comment
                , k.address == a || (relation k.address == a && k.address /= a)
                ]


-- | A SQL-standard body, which PostgreSQL parses once and tracks in pg_depend.
standardBody :: Text -> Bool
standardBody def = "BEGIN ATOMIC" `T.isInfixOf` def || any ("RETURN " `T.isPrefixOf`) (T.lines def)


-- | For a foreign key constraint's definition, each of its columns' types paired with the
-- referenced column's, by @typeIn schema table column@.
foreignKeyTypes :: (Text -> Text -> Text -> Maybe Text) -> Address -> Text -> Maybe [(Text, Text)]
foreignKeyTypes typeIn k def = do
    [s, t, _] <- Just k.objectNames
    rest <- T.stripPrefix "FOREIGN KEY " def
    let (cols, afterCols) = columnList rest
    refPart <- T.stripPrefix " REFERENCES " afterCols
    let (refTable, refRest) = T.break (== '(') refPart
        (refCols, _) = columnList refRest
        (rs, rt) = case T.break (== '.') refTable of
            (name, "") -> ("public", unquote name)
            (schema, name) -> (unquote schema, unquote (T.drop 1 name))
    guard (length cols == length refCols && not (null cols))
    for (zip cols refCols) \(a, b) -> (,) <$> typeIn s t a <*> typeIn rs rt b
  where
    columnList x = let (inside, after) = T.break (== ')') (T.drop 1 x) in (map (unquote . T.strip) (T.split (== ',') inside), T.drop 1 after)
    unquote c = fromMaybe c (T.stripPrefix "\"" c >>= T.stripSuffix "\"")


-- | Connections per database; the describe queries run over all of them at once.
describeConnections :: Int
describeConnections = 4


-- | A described database, its open connections, and the statements that make
-- plpgsql_check's temp tables there.
data Connected = Connected Connection.Connection [Connection.Connection] Schema Addresses (IO (Maybe Text))


-- | @local@ names the dev stack's database, on @KRONOR_PG_PORT@ (5432 when unset).
resolveDb :: Text -> IO Text
resolveDb = \case
    "local" -> do
        port <- fromMaybe "5432" <$> lookupEnv "KRONOR_PG_PORT"
        pure [i|host=localhost port=#{port} user=kronor dbname=kronor|]
    db -> pure db


-- | A connection to the database the table command edits in rolled-back transactions.
connectScratch :: Text -> IO Connection.Connection
connectScratch name = do
    conn <- connect =<< resolveDb name
    requireScratch name conn
    pure conn


-- | A database's schema over 'describeConnections' connections.
describeDatabase :: Scope -> Text -> IO Connected
describeDatabase scope name = do
    db <- resolveDb name
    tempTables <- async (either (const Nothing) id <$> tryAny (bracket (connect db) Connection.release tempTableStatements))
    (conn, extra) <- concurrently (connect db) (replicateConcurrently (describeConnections - 1) (connect db))
    (schema, addresses) <- describeOn (conn : extra) scope.excluded scope.partitions Nothing
    pure (Connected conn extra schema addresses (wait tempTables))


-- | Reads both databases at the same time, then the edges each command needs, given what
-- differs, alongside the body checks; the edges of @--to@ only for 'ForPlan'.
readBoth :: Scope -> Source -> Text -> Purpose -> IO ((Schema, [Edge]), (Schema, [Edge]), Map ObjectKey BodyCheck)
readBoth scope from toDb purpose = do
    toDescribed <- async (timed "describe --to" $ describeDatabase scope toDb)
    fromDb <- timed "connInfoOf" $ connInfoOf from
    (fromDatabase@(Connected _ _ fromSchema _ _), toDatabase@(Connected _ _ toSchema _ _)) <-
        timed "readBoth.describe" $ concurrently (timed "describe --from" $ describeDatabase scope fromDb) (wait toDescribed)
    let plainChanges = diff (\_ a b -> a == b) fromSchema toSchema
        needed = neededFor purpose plainChanges
    measure "readBoth.plainDiff" (Map.size plainChanges)
    (((fromEdges, fromNotes), (toEdges, toNotes)), checks) <-
        timed "readBoth.edgesAndChecks"
            $ concurrently
                ( concurrently
                    (edgesOf "--from" scope needed fromDatabase)
                    (if purpose == ForPlan then edgesOf "--to" scope needed toDatabase else pure ([], []))
                )
                (bodyChecks scope toDb toSchema plainChanges)
    for_ (fromNotes <> toNotes) (TIO.hPutStrLn stderr)
    pure ((fromSchema, fromEdges), (toSchema, toEdges), checks)


-- | Body checks for the definitions that are added or changed: no other check is consulted.
bodyChecks :: Scope -> Text -> Schema -> Map ObjectKey Change -> IO (Map ObjectKey BodyCheck)
bodyChecks scope toDb toSchema plainChanges
    | scope.bodyCheck == CheckBodies =
        bracket (connect =<< resolveDb toDb) Connection.release \conn ->
            timed "bodyChecks" $ checkBodies conn (Map.restrictKeys toSchema addedOrChanged)
    | otherwise = pure Map.empty
  where
    addedOrChanged = Map.keysSet (Map.filter (\case Dropped _ -> False; _ -> True) plainChanges)


-- | What @--from@ names: a connection string, or the baseline database built from a revision.
data Source
    = Database Text
    | BaselineAt Text


source :: Text -> Source
source s
    | s == "master" = BaselineAt s
    | Just rev <- T.stripPrefix "rev:" s = BaselineAt rev
    | otherwise = Database s


connInfoOf :: Source -> IO Text
connInfoOf = \case
    Database db -> pure db
    BaselineAt rev -> ensureBaseline rev


data Command
    = Describe Text (Maybe FilePath) Existing Scope
    | Diff Source Text Scope
    | Plan Source Text Scope
    | Sqitch Source Text Scope SqitchOptions
    | TableFile FilePath Text TableOptions


data TableOptions = TableOptions
    { output :: Output
    , compareWith :: CompareWith
    , drops :: Drops
    , base :: Text
    , project :: FilePath
    , top :: FilePath
    , planPath :: FilePath
    , tableTag :: Maybe Text
    , renames :: [(Text, Text)]
    , sameTypeSwap :: SameTypeSwap
    , fileMode :: FileMode
    , liveFrom :: Maybe Text
    }


-- | Whether @table@ migrates the table to the file, or rewrites the file from the table.
data FileMode = MigrateToFile | RewriteFromDatabase
    deriving stock (Eq)


-- | Whether @table@ prints the migration or writes it with sqitch.
data Output = PrintMigration | WriteMigration
    deriving stock (Eq)


-- | What the edited file is compared with.
data CompareWith = FileAtBase | LiveDatabase
    deriving stock (Eq)


-- | A dropped column and an added one of the same type: maybe a rename, or not one.
data SameTypeSwap = AskIfRename | DropAndAdd
    deriving stock (Eq)


data SqitchOptions = SqitchOptions
    { projectDir :: FilePath
    , topDir :: FilePath
    , planFile :: FilePath
    , tag :: Maybe Text
    , dryRun :: Bool
    }


main :: IO ()
main =
    handleAny unexpected
        . handle (\(UserError message) -> TIO.hPutStrLn stderr [i|schema-diff: #{message}|] >> exitWith (ExitFailure 2))
        $ O.execParser (O.info (commandParser O.<**> O.helper) O.fullDesc)
        >>= \case
            Describe name out replace scope -> do
                db <- resolveDb name
                conns <- timed "describe.connect" $ replicateConcurrently describeConnections (connect db)
                (schema, _) <- timed "describe.describeOn" $ describeOn conns scope.excluded scope.partitions Nothing
                case out of
                    Just dir -> writeFolder replace dir schema
                    Nothing ->
                        for_ (Map.toList (Map.fromListWith (+) [(k.address.objectType, 1 :: Int) | k <- Map.keys schema])) \(kind, n) ->
                            TIO.putStrLn [i|#{n}\t#{kind}|]
            Diff from toDb scope -> do
                ((fromSchema, edges), (toSchema, _), checks) <- timed "readBoth" $ readBoth scope from toDb ForDiff
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    known = Set.fromList [k.address | k <- Map.keys fromSchema, k.aspect == Definition]
                    affectedBy = affected known edges changes
                    dependentsOf key = case key.aspect of
                        Definition -> Map.findWithDefault Set.empty key.address affectedBy
                        _ -> Set.empty
                measure "diff.diffAndAffected" (Map.size changes + Map.foldl' (\n s -> n + Set.size s) 0 affectedBy)
                for_ (Map.toList changes) \(key, change) -> report key change (dependentsOf key)
                TIO.putStrLn (summary changes)
                unless (Map.null changes) $ exitWith (ExitFailure 1)
            Plan from toDb scope -> do
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- timed "readBoth" $ readBoth scope from toDb ForPlan
                let steps = plan checks fromSchema toSchema fromEdges toEdges (diff (sameDefinition checks) fromSchema toSchema)
                measure "plan.planRender" (T.length (T.unlines (map renderStep steps)))
                for_ steps (TIO.putStrLn . renderStep)
            Sqitch from toDb scope opts -> do
                unless opts.dryRun requireSqitch
                sqitchPlan <- readPlan (opts.projectDir </> opts.planFile)
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- timed "readBoth" $ readBoth scope from toDb ForPlan
                now <- getCurrentTime
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    steps = plan checks fromSchema toSchema fromEdges toEdges changes
                    inverseCtx = context Map.empty toSchema fromSchema toEdges (Map.map invert changes)
                    written = units (const True) inverseCtx sqitchPlan now steps
                    tagName = fromMaybe (defaultTag now) opts.tag
                measure "sqitch.planUnits" (sum [length u.deploy + length u.revert | u <- written])
                if opts.dryRun
                    then for_ written \u -> do
                        let kind = case u.kind of AddChange -> "add" :: Text; ReworkChange -> "rework"
                            count = length u.deploy
                            changeName = u.changeName
                            note = u.note
                        TIO.putStrLn [i|#{kind}\t#{changeName}\t#{count} statements\t#{note}|]
                    else writeUnits opts.projectDir opts.topDir sqitchPlan tagName written
            TableFile file db opts -> tableCommand file db opts
  where
    unexpected e = case fromException e of
        Just code -> throwIO (code :: ExitCode)
        Nothing -> TIO.hPutStrLn stderr [i|schema-diff: #{displayException e}|] >> exitWith (ExitFailure 2)


-- | What the table command works on: its options, the deploy file before and after the
-- edit, and where they live.
data TableEnv = TableEnv
    { opts :: TableOptions
    , db :: Text
    , file :: FilePath
    , root :: FilePath
    , repo :: Repo
    , sqitchChange :: Text
    , table :: QName
    , statements :: [Text]
    , original :: Text
    }


-- | A retyped column: its table, name, address and new type.
type Retyped = (QName, Text, Address, Text)


-- | The functions the body check finds broken by the edit, and the ones it passes.
data BodyResults = BodyResults
    { breaking :: Map Address Text
    , passed :: Set Address
    }


-- | What the comparison and the checks found.
data Evidence = Evidence
    { edit :: TableEdit
    , deps :: Dependents
    , bodies :: BodyResults
    , retyped :: [Retyped]
    , foreignKeyBreaks :: Map Address [(Address, Text)]
    , typeFacts :: Map Address TypeChange
    , branchChanges :: [(Text, [Text])]
    , pendingRenames :: [(Text, Text)]
    , renamedFrom :: Set Address
    }


-- | The migration's steps and what the command reports about them.
data Findings = Findings
    { steps :: [Step]
    , renameSteps :: [Step]
    , newRenames :: [(Text, Text)]
    , usage :: [Text]
    , blocking :: [Address]
    , renameLike :: [(Text, Text)]
    , same :: Text
    , withViews :: Context -> Context
    }


-- | Prints the migration that turns the table into what its edited file creates, or
-- writes it with sqitch.
tableCommand :: FilePath -> Text -> TableOptions -> IO ()
tableCommand file db opts = do
    when (opts.output == WriteMigration) requireSqitch
    (root, absFile, changeName, table) <- locateTable file opts
    statements <- parseFile file "" =<< readFileUtf8 absFile
    let describeWithWords = describeDatabase dependentsScope db >>= \c@(Connected _ _ schema _ _) -> (c,) <$> routineWords schema
        migrating = opts.fileMode == MigrateToFile
    early <- if migrating && (opts.compareWith == LiveDatabase || not (null opts.renames)) then Just <$> async describeWithWords else pure Nothing
    repo <- findRepo root
    original <- timed "table.baseFile" $ showFile repo opts.base absFile
    let env = TableEnv{opts, db, file, root, repo, sqitchChange = changeName, table, statements, original}
    if migrating
        then do
            described <- case early of
                Just a -> pure (wait a)
                Nothing
                    | Right statements == (withoutTransactionControl <=< splitStatements) original -> pure describeWithWords
                    | otherwise -> wait <$> async describeWithWords
            runRIO env (compareAndReport described)
        else runRIO env rewriteFromDatabase


-- | Prints the file rewritten to create the table as the database has it, or writes it
-- and reworks the change.
rewriteFromDatabase :: RIO TableEnv ()
rewriteFromDatabase = do
    env <- ask
    let opts = env.opts
        table = env.table
        deployDir = env.root </> opts.top </> "deploy"
    unless (null opts.renames) $ failWith "--rename works with the file comparison, not --rewrite-file"
    current <- readFileUtf8 env.file
    conn <- liftIO (connectScratch env.db)
    (live, liveConn) <- case opts.liveFrom of
        Nothing -> pure (SameDatabase, conn)
        Just name -> liftIO do
            c <- connect =<< resolveDb name
            readOnly c
            pure (OtherDatabase c, c)
    let liveDb = fromMaybe env.db opts.liveFrom
    order <- liftIO (columnOrder liveConn table)
    when (null order) $ failWith [i|#{qualified table} is not in #{liveDb}; there is nothing to rewrite the file from|]
    for_ opts.liveFrom \_ -> do
        pending <- undeployedTouching liveConn
        let n = length pending
            names = T.intercalate ", " pending
        unless (null pending) $ failWith [i|#{liveDb} has not deployed #{n} planned changes that touch #{qualified table}, and a rewrite from it would undo them: #{names}|]
    edit <- timed "rewrite.compare" $ liftIO (compareTable conn (LiveTable live ReportDrops) table env.statements)
    ownFiles <-
        fmap Set.fromList
            . filterM (\n -> doesFileExist (deployDir </> T.unpack table.schema </> "table" </> T.unpack table.name </> "index" </> T.unpack n <.> "sql"))
            $ [n | k <- Map.keys edit.current, Index q <- [fromAddress k.address], let n = q.name]
    let path = env.file
    rewrite <- either (\e -> failWith [i|#{path}: #{e}|]) pure (rewriteFile table ownFiles order edit.current edit.desired current)
    remaining <-
        if rewrite.text == current
            then pure (fileDrift ownFiles edit.desired edit.current)
            else do
                rewritten <- parseFile env.file " as rewritten" rewrite.text
                check <- timed "rewrite.check" $ liftIO (compareTable conn (LiveTable live ReportDrops) table rewritten)
                pure (fileDrift ownFiles check.desired check.current)
    let still = [render k.address <> (if k.aspect == Comment then " (comment)" else "") | k <- Map.keys remaining]
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
                sqitchPlan <- liftIO (readPlan (env.root </> opts.planPath))
                basePlan <- liftIO (parsePlan <$> showFile env.repo opts.base (env.root </> opts.planPath))
                now <- liftIO getCurrentTime
                let changeName = env.sqitchChange
                    entries p = length (filter (== changeName) p.changes)
                    tagName = fromMaybe (defaultTag now) opts.tableTag
                writeFileUtf8 env.file rewrite.text
                if entries sqitchPlan > entries basePlan
                    then say [i|rewrote #{changeName}; the branch already reworks it|]
                    else do
                        liftIO $ reworkEdited env.root opts.top sqitchPlan tagName changeName [i|schema-diff: #{changeName} as the database has it|] current PreviousFileRuns
                        say [i|rewrote and reworked #{changeName}|]


-- | The planned changes @live@'s sqitch registry lacks whose deploy scripts name the table.
undeployedTouching :: Connection.Connection -> RIO TableEnv [Text]
undeployedTouching live = do
    env <- ask
    let opts = env.opts
        deployDir = env.root </> opts.top </> "deploy"
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
    planText <- readFileUtf8 (env.root </> opts.planPath)
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


-- | The project root, the file's absolute path, its sqitch change and its table.
locateTable :: FilePath -> TableOptions -> IO (FilePath, FilePath, Text, QName)
locateTable file opts = do
    root <- makeAbsolute opts.project
    absFile <- makeAbsolute file
    let deployDir = root </> opts.top </> "deploy"
        relative = T.pack (makeRelative deployDir absFile)
    changeName <- maybe (failWith [i|#{file} is not under #{deployDir}|]) pure (T.stripSuffix ".sql" relative)
    table <- case T.split (== '/') changeName of
        [s, "table", t] | not ("@" `T.isInfixOf` t) -> pure (QName s t)
        _ -> failWith [i|#{file} is not a table's own deploy file (<schema>/table/<name>.sql)|]
    pure (root, absFile, changeName, table)


-- | A deploy file's statements; @label@ names the version in a parse error.
parseFile :: MonadIO m => FilePath -> Text -> Text -> m [Text]
parseFile file label = either (\e -> failWith [i|#{file}#{label}: #{e}|]) pure . (withoutTransactionControl <=< splitStatements)


atBase :: TableOptions -> Text
atBase opts = let base = opts.base in [i| at #{base}|]


renameSql :: QName -> (Text, Text) -> Text
renameSql table (old, new) = [i|alter table #{qualified table} rename column #{quoteIdent old} to #{quoteIdent new}|]


columnAddress :: QName -> Text -> Address
columnAddress table c = Address "table column" [table.schema, table.name, c] []


say :: MonadIO m => Text -> m ()
say = liftIO . TIO.putStrLn


-- | Compares the table, checks what depends on the edit, and prints or writes the migration.
compareAndReport :: IO (Connected, Map Address BodyText) -> RIO TableEnv ()
compareAndReport described = do
    env <- ask
    let opts = env.opts
        table = env.table
    when (opts.compareWith == LiveDatabase && not (null opts.renames)) $ failWith "--rename works with the file comparison, not --against-database"
    when (isJust opts.liveFrom) $ failWith "--live-from works with --rewrite-file"
    branchChanges <- branchChangesFor
    let branchStatements = concatMap snd branchChanges
        normalised = T.toLower . T.unwords . T.words . T.dropWhileEnd (== ';') . T.strip . withoutLeadingComments
        pendingRenames = [r | r <- opts.renames, normalised (renameSql table r) `notElem` map normalised branchStatements]
    for_ branchChanges \(n, _) -> say [i|-- already on this branch: #{n}|]
    baseline <-
        if opts.compareWith == LiveDatabase
            then pure (LiveTable SameDatabase opts.drops)
            else BaseFile . (<> (branchStatements <> map (renameSql table) pendingRenames)) <$> parseFile env.file (atBase opts) env.original
    conn <- timed "table.connect" $ liftIO (connectScratch env.db)
    edit <- timed "table.compare1" $ liftIO (compareTable conn baseline table env.statements)
    let renamedFrom = Set.fromList [columnAddress table old | (old, _) <- opts.renames]
    deps <- timed "table.dependentsIn" $ liftIO (dependentsIn described (edit.changes <> Map.fromSet (const (Dropped "")) (Set.map (ObjectKey Definition) renamedFrom)))
    bodies <- bodyResultsFor conn baseline deps
    let retyped = retypedColumns edit
    foreignKeyBreaks <- liftIO (foreignKeyBreaksFor conn deps retyped)
    typeFacts <- liftIO (typeFactsFor conn edit retyped)
    let findings = findingsFor env Evidence{edit, deps, bodies, retyped, foreignKeyBreaks, typeFacts, branchChanges, pendingRenames, renamedFrom}
    measure "table.planRender" (T.length (T.unlines (map renderStep findings.steps <> findings.usage)) + length findings.blocking)
    printLeftAlone edit
    case opts.output of
        PrintMigration -> printMigration findings
        WriteMigration -> writeMigration conn baseline edit findings


-- | This branch's changes for the table, with the statements the comparison can replay;
-- none against the database.
branchChangesFor :: RIO TableEnv [(Text, [Text])]
branchChangesFor = do
    env <- ask
    let opts = env.opts
        root = env.root
    if opts.compareWith == LiveDatabase
        then pure []
        else do
            current <- liftIO (readPlan (root </> opts.planPath))
            basePlan <- liftIO (parsePlan <$> showFile env.repo opts.base (root </> opts.planPath))
            let before = Set.fromList basePlan.changes
            for (filter (\n -> n `Set.notMember` before && forTable env.table n) (nubOrd current.changes)) \n -> do
                script <- readFileUtf8 (root </> opts.top </> "deploy" </> T.unpack n <.> "sql")
                (n,) . replayable <$> either (\e -> failWith [i|#{n}: #{e}|]) pure ((withoutTransactionControl <=< splitStatements) script)


-- | Checks the PL/pgSQL and string-bodied SQL functions that depend on the edit or name it,
-- before and after the edit.
bodyResultsFor :: Connection.Connection -> Baseline -> Dependents -> RIO TableEnv BodyResults
bodyResultsFor conn baseline deps = do
    env <- ask
    let table = env.table
        definitionOf a = Map.lookup (ObjectKey Definition a) deps.dbSchema
        takesRowType a = any (`elem` [qualified table, qualified table <> "[]"]) a.objectArgs
        routines = Set.toList (Set.filter (\a -> a.objectType `elem` ["function", "procedure"] && not (takesRowType a)) (fold deps.named <> fold deps.exact))
        plpgsql = [a | a <- routines, Just def <- [definitionOf a], "LANGUAGE plpgsql" `T.isInfixOf` def]
        sqlBodies = [a | a <- routines, Just def <- [definitionOf a], "LANGUAGE sql" `T.isInfixOf` def, not (standardBody def)]
        signature a = case a.objectNames of
            [s, n] -> let args = T.intercalate ", " a.objectArgs in [i|#{quoteIdent s}.#{quoteIdent n}(#{args})|]
            _ -> render a
        bySignature = Map.fromList [(signature a, a) | a <- plpgsql <> sqlBodies]
        check made = timed [i|table.bodyErrors n=#{Map.size bySignature}|] $ bodyErrors conn aside (qualified table) made (map signature plpgsql) (map signature sqlBodies)
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


retypedColumns :: TableEdit -> [Retyped]
retypedColumns edit =
    [ (t, c, k.address, n.colType)
    | (k, Changed old new) <- Map.toList edit.changes
    , k.aspect == Definition
    , let o = parseColumn old
          n = parseColumn new
    , o.colType /= n.colType || o.collation /= n.collation
    , Column t c <- [fromAddress k.address]
    ]


-- | The foreign keys on retyped columns that cannot be rebuilt with the new types, with
-- PostgreSQL's error.
foreignKeyBreaksFor :: Connection.Connection -> Dependents -> [Retyped] -> IO (Map Address [(Address, Text)])
foreignKeyBreaksFor conn deps retyped =
    timed "table.foreignKeys"
        $ Map.fromListWith (<>)
        . catMaybes
        <$> for
            [ (addr, k, types)
            | (_, _, addr, _) <- retyped
            , k <- Set.toList (Map.findWithDefault Set.empty addr deps.exact)
            , k.objectType == "table constraint"
            , Just def <- [Map.lookup (ObjectKey Definition k) deps.dbSchema]
            , Just types <- [foreignKeyTypes typeIn k def]
            ]
            \(addr, k, types) -> fmap (\err -> (addr, [(k, err)])) <$> foreignKeyError conn types
  where
    typeIn s t c = case [ty | (QName s' t', c', _, ty) <- retyped, (s', t', c') == (s, t, c)] of
        ty : _ -> Just ty
        [] -> (.colType) . parseColumn <$> Map.lookup (ObjectKey Definition (Address "table column" [s, t, c] [])) deps.dbSchema


-- | For each retyped column, whether the change rewrites the table and whether the new
-- type is a domain.
typeFactsFor :: Connection.Connection -> TableEdit -> [Retyped] -> IO (Map Address TypeChange)
typeFactsFor conn edit retyped =
    timed "table.typeChangeFacts" $ Map.fromList . catMaybes <$> for retyped \(_, _, addr, newType) ->
        case parseColumn <$> Map.lookup (ObjectKey Definition addr) edit.current of
            Just o -> fmap (addr,) <$> typeChangeFacts conn o.colType newType
            Nothing -> pure Nothing


findingsFor :: TableEnv -> Evidence -> Findings
findingsFor env ev = Findings{steps, renameSteps, newRenames = ev.pendingRenames, usage, blocking, renameLike, same, withViews}
  where
    opts = env.opts
    table = env.table
    edit = ev.edit
    deps = ev.deps
    breaking = ev.bodies.breaking
    passed = ev.bodies.passed
    retyped = ev.retyped
    otherTables addr =
        Set.toList
            . Set.fromList
            $ [ other
              | k <- Set.toList (Map.findWithDefault Set.empty addr deps.exact)
              , k.objectType == "table constraint"
              , [s, t, _] <- [k.objectNames]
              , Just def <- [Map.lookup (ObjectKey Definition k) deps.dbSchema]
              , Just referenced <- [referencedTable def]
              , let other = if QName s t == table then referenced else qualified (QName s t)
              , other /= qualified table
              ]
    convertNote = MayFail "fails when an existing value does not convert to the new type"
    withTypeFacts s = case Map.lookup s.address ev.typeFacts of
        Just (TypeChange rewrite target)
            | MayRewriteTable `elem` s.hazards ->
                let rewrites = rewrite == RewritesFiles
                    domain = target == DomainType
                 in let kept = concatMap (\h -> if h == MayRewriteTable then [RewritesTable | rewrites] else [h | h /= convertNote || rewrites || domain]) s.hazards
                     in s{hazards = kept <> concat [LocksOther o : [RechecksForeignKey o | rewrites] | o <- otherTables s.address]}
        _ -> s
    breaksFor addr = [a | a <- Set.toList (Map.findWithDefault Set.empty addr deps.named <> Map.findWithDefault Set.empty addr deps.exact), a `Map.member` breaking]
    dependents = deps.exact
    mentions = Map.map (Set.filter (\a -> a `Map.notMember` breaking && a `Set.notMember` passed)) deps.named
    retypedAddresses = Set.fromList [a | (_, _, a, _) <- retyped]
    around = viewsAroundTypeChanges deps [(t, c, a) | (t, c, a, _) <- retyped]
    withViews c = c{viewsAround = Map.map snd around}
    aroundOf addr = fold [members | (t, c, a, _) <- retyped, a == addr, Just (members, _) <- [Map.lookup (t, c) around]]
    foreignKeysFor addr = Map.findWithDefault [] addr ev.foreignKeyBreaks
    renameHazard = MayFail "breaks running code and Hasura metadata that still use the old name; kronor renames over two deploys with create-migration rename-column-step-1 and rename-column-step-2"
    renameSteps =
        [ Step DropPhase (-1) addr (Column table new) (renameSql table (old, new)) Shared [renameHazard] (ObjectKey Definition addr) (Changed def def)
        | (old, new) <- ev.pendingRenames
        , let addr = columnAddress table new
              def = Map.findWithDefault "" (ObjectKey Definition addr) edit.desired
        ]
    steps = renameSteps <> map withTypeFacts (planWith withViews Map.empty edit.current edit.desired [] [] edit.changes)
    same :: Text
    same
        | opts.compareWith == LiveDatabase = "the file matches the table"
        | not (null ev.branchChanges) = "this branch's changes already cover the file"
        | otherwise = let base = opts.base in [i|the file has no schema changes since #{base}|]
    columnsWhere p = [(c, (parseColumn (changeText ch)).colType) | (k, ch) <- Map.toList edit.changes, k.aspect == Definition, p ch, Column _ c <- [fromAddress k.address]]
    changeText = \case Added d -> d; Dropped d -> d; Changed _ d -> d
    renameLike =
        [ (old, new)
        | null opts.renames
        , opts.sameTypeSwap == AskIfRename
        , (old, oldType) <- columnsWhere (\case Dropped _ -> True; _ -> False)
        , (new, newType) <- columnsWhere (\case Added _ -> True; _ -> False)
        , oldType == newType
        ]
    renameNotes = [[i|#{old} is dropped and #{new} added with the same type; pass --rename #{old}:#{new} if it is a rename, or --drop-and-add if it is not|] | (old, new) <- renameLike]
    dropped addr = case Map.lookup (ObjectKey Definition addr) edit.changes of
        Just (Dropped _) -> True
        _ -> False
    goesWith d = d.objectType == "index" || (d.objectType == "table constraint" && take 2 d.objectNames == [table.schema, table.name])
    listed = fold dependents <> fold mentions
    overloaded = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [((a.objectType, a.objectNames), 1) | a <- Set.toList listed]))
    short a =
        let objectType = a.objectType
            kind = if objectType `elem` ["function", "procedure"] then "" else [i|#{objectType} |] :: Text
            name = T.intercalate "." a.objectNames
            args = if (a.objectType, a.objectNames) `Set.member` overloaded then let list = T.intercalate ", " a.objectArgs in [i|(#{list})|] else "" :: Text
         in [i|#{kind}#{name}#{args}|] :: Text
    item d = [i|    #{short d}|] :: Text
    detail t = [i|      #{t}|] :: Text
    section :: Text -> [Address] -> [Text]
    section heading = \case
        [] -> []
        ds -> [i|  #{heading}|] : map item ds
    breaksSection = \case
        [] -> []
        fs -> "  breaks" : concat [[item f, detail (Map.findWithDefault "" f breaking)] | f <- fs]
    foreignKeySection = \case
        [] -> []
        ks -> "  cannot be rebuilt with the new type" : concat [[item k, detail err] | (k, err) <- ks]
    isView d = d.objectType `elem` ["view", "materialized view"]
    recreated addr d = d `Set.member` aroundOf addr || (d.objectType == "trigger" && any (\v -> isView v && v.objectNames == take 2 d.objectNames) (aroundOf addr))
    rebuiltWith d = d.objectType `elem` ["index", "table constraint", "sequence"]
    usage =
        concat
            [ T.intercalate "." addr.objectNames
                : section (if addr `Set.member` ev.renamedFrom then "used by, and follows the rename" else "used by") users
                    <> section "dropped with it" with
                    <> section "dropped and created again around it" again
                    <> breaksSection (breaksFor addr)
                    <> foreignKeySection (foreignKeysFor addr)
                    <> section "named in" (Set.toList (Map.findWithDefault Set.empty addr mentions))
            | addr <- Set.toList (Map.keysSet dependents <> Map.keysSet deps.named)
            , not (null (Map.findWithDefault Set.empty addr dependents)) || not (null (breaksFor addr)) || not (null (Map.findWithDefault Set.empty addr mentions))
            , let (with, rest) = L.partition (\d -> dropped addr && goesWith d) (Set.toList (Map.findWithDefault Set.empty addr dependents))
                  (again, users) = L.partition (recreated addr) (filter (`Map.notMember` breaking) rest)
            ]
            <> renameNotes
    blocking =
        Set.toList
            . Set.fromList
            $ [d | (addr, ds) <- Map.toList dependents, dropped addr, d <- Set.toList ds, not (goesWith d)]
            <> [d | (addr, ds) <- Map.toList dependents, addr `Set.member` retypedAddresses, d <- Set.toList ds, not (recreated addr d), not (rebuiltWith d), d `Set.notMember` passed]
            <> Map.keys breaking
            <> [k | ks <- Map.elems ev.foreignKeyBreaks, (k, _) <- ks]


-- | The differences the comparison leaves alone, with the reason; an object's comment
-- goes with it.
printLeftAlone :: MonadIO m => TableEdit -> m ()
printLeftAlone edit = do
    let leftAlone = Set.fromList [k.address | (k, _) <- edit.ignored, k.aspect == Definition]
    for_ [(k, reason) | (k, reason) <- edit.ignored, not (k.aspect == Comment && k.address `Set.member` leftAlone)] \(k, reason) -> do
        let name = render k.address <> if k.aspect == Privileges then " (privileges)" else ""
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
        root = env.root
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
    sqitchPlan <- timed "write.readPlan" $ liftIO (readPlan (root </> opts.planPath))
    basePlan <- timed "write.basePlan" $ liftIO (parsePlan <$> showFile env.repo opts.base (root </> opts.planPath))
    now <- liftIO getCurrentTime
    let inverseCtx = f.withViews (context Map.empty edit.desired edit.current [] (Map.map invert edit.changes))
        renameBack =
            [ Step DropPhase (-1) (columnAddress table old) (Column table old) (renameSql table (new, old)) Shared [] s.key s.change
            | (s, (old, new)) <- zip f.renameSteps f.newRenames
            ]
        withRenameBack u
            | any (`elem` f.renameSteps) u.deploy = u{revert = u.revert <> renameBack}
            | otherwise = u
        isIndex s = case s.object of Index _ -> True; _ -> False
        written = map withRenameBack (units isIndex inverseCtx sqitchPlan now f.steps)
        tagName = fromMaybe (defaultTag now) opts.tableTag
        entries p = length (filter (== changeName) p.changes)
        alreadyReworked = entries sqitchPlan > entries basePlan
    previous <- parseFile env.file (atBase opts) env.original
    oldFileFails <- timed "table.compare3" $ liftIO (withEditedTable conn baseline table env.statements (pure ()) (const (firstFailure conn previous)))
    let baseRev = opts.base
    for_ oldFileFails \err ->
        say [i|the file at #{baseRev} fails on the edited table (#{err}); the rework's revert does nothing and the migrations revert the table|]
    timed "write.writeUnits" $ liftIO (writeUnits root opts.top sqitchPlan tagName written)
    unless alreadyReworked do
        afterUnits <- liftIO (readPlan (root </> opts.planPath))
        timed "write.reworkEdited"
            . liftIO
            $ reworkEdited root opts.top afterUnits tagName changeName [i|schema-diff: #{changeName} as edited|] env.original (maybe PreviousFileRuns (const PreviousFileFails) oldFileFails)
    when alreadyReworked . liftIO $ moveToEnd (root </> opts.planPath) changeName
    for_ written \u -> let name = u.changeName in say [i|added #{name}|]
    say
        ( if alreadyReworked
            then [i|moved the branch's rework of #{changeName} after the new migrations; if this branch is deployed locally, revert it to before the rework first|]
            else [i|reworked #{changeName}|]
        )


-- | Whether a sqitch change is about the table, by the names create-migration and
-- schema-diff give: <schema>/table/<table>/index/…, or <schema>.<table> in a migration's name.
forTable :: QName -> Text -> Bool
forTable t n =
    let (schema, name) = (t.schema, t.name)
        key = [i|#{schema}.#{name}|] :: Text
        indexes = [i|#{schema}/table/#{name}/index/|] :: Text
        padded = [i|#{n}-|] :: Text
     in indexes `T.isPrefixOf` n || any (`T.isInfixOf` padded) [[i|#{before}#{key}#{after}|] | before <- ["-", "/" :: Text], after <- [".", "-" :: Text]]


-- | A migration's statements as they can run inside the comparison's transaction:
-- without session settings, and with concurrent index builds made plain.
replayable :: [Text] -> [Text]
replayable = map (TP.replace "concurrently " "" . TP.replace "CONCURRENTLY " "") . filter (not . setting)
  where
    setting s = any (`T.isPrefixOf` T.toLower (withoutLeadingComments s)) ["set ", "reset ", "select set_config("]


-- | A statement from its first line of code.
withoutLeadingComments :: Text -> Text
withoutLeadingComments = T.stripStart . T.unlines . dropWhile (\l -> T.null (T.strip l) || "--" `T.isPrefixOf` T.stripStart l) . T.lines


commandParser :: O.Parser Command
commandParser =
    O.hsubparser
        $ O.command "describe" (O.info describeParser (O.progDesc "Read one database's schema; with --out, write one file per object"))
        <> O.command "diff" (O.info diffParser (O.progDesc "Show what turns --from into --to; exits 1 when they differ"))
        <> O.command "plan" (O.info (twoDatabases Plan) (O.progDesc "Print the SQL that turns --from into --to"))
        <> O.command "sqitch" (O.info (twoDatabases Sqitch <*> sqitchOptions) (O.progDesc "Write the plan as sqitch changes"))
        <> O.command
            "table"
            ( O.info
                tableParser
                (O.progDesc "Print the migration that turns a table into what its edited deploy file creates; with --write, add it and rework the file")
            )
  where
    tableParser =
        TableFile
            <$> O.strArgument (O.metavar "FILE" <> O.help "The table's deploy file, for example database/deploy/runtime/table/payment.sql")
            <*> O.strOption (O.long "db" <> O.metavar "CONNINFO|local" <> O.help "A database with the table as it is now; changes are rolled back. local is the dev stack's database")
            <*> ( TableOptions
                    <$> O.flag PrintMigration WriteMigration (O.long "write" <> O.help "Add the migration with sqitch and rework the table's change")
                    <*> O.flag FileAtBase LiveDatabase (O.long "against-database" <> O.help "Compare with the live table instead of the file at --base; shows drift too")
                    <*> O.flag ReportDrops AllowDrops (O.long "allow-drops" <> O.help "With --against-database, drop columns and constraints that are not in the file")
                    <*> O.strOption (O.long "base" <> O.metavar "REV" <> O.value "master" <> O.help "Git revision with the file before your edit")
                    <*> O.strOption (O.long "project" <> O.metavar "DIR" <> O.value ".")
                    <*> O.strOption (O.long "top-dir" <> O.metavar "DIR" <> O.value "database")
                    <*> O.strOption (O.long "plan-file" <> O.metavar "FILE" <> O.value "database/sqitch.plan")
                    <*> O.optional (O.strOption (O.long "tag" <> O.metavar "NAME" <> O.help "Tag to add when the rework needs one"))
                    <*> O.many (O.option (O.eitherReader renameArg) (O.long "rename" <> O.metavar "OLD:NEW" <> O.help "Column OLD is renamed to NEW; repeat for more columns"))
                    <*> O.flag AskIfRename DropAndAdd (O.long "drop-and-add" <> O.help "A column dropped and one added with the same type are not a rename")
                    <*> O.flag MigrateToFile RewriteFromDatabase (O.long "rewrite-file" <> O.help "Print the file rewritten to create the table as --db has it; with --write, write it and rework the change")
                    <*> O.optional (O.strOption (O.long "live-from" <> O.metavar "CONNINFO|local" <> O.help "With --rewrite-file, read the table from this database, read-only, instead of --db; --db still builds the file"))
                )
    renameArg s = case T.split (== ':') (T.pack s) of
        [old, new] | not (T.null old), not (T.null new) -> Right (old, new)
        _ -> Left "expected OLD:NEW"
    sqitchOptions =
        SqitchOptions
            <$> O.strOption (O.long "project" <> O.metavar "DIR" <> O.value "." <> O.help "Where sqitch.conf is")
            <*> O.strOption (O.long "top-dir" <> O.metavar "DIR" <> O.value "database")
            <*> O.strOption (O.long "plan-file" <> O.metavar "FILE" <> O.value "database/sqitch.plan")
            <*> O.optional (O.strOption (O.long "tag" <> O.metavar "NAME" <> O.help "Tag to add when a rework needs one"))
            <*> O.switch (O.long "dry-run" <> O.help "List the changes without writing them")
    twoDatabases constructor =
        constructor
            <$> fmap
                source
                ( O.strOption
                    ( O.long "from"
                        <> O.metavar "CONNINFO|local|master|rev:REV"
                        <> O.help "The database to change; master or rev:REV builds or updates the baseline database at that revision and uses it; local is the dev stack's database"
                    )
                )
            <*> O.strOption (O.long "to" <> O.metavar "CONNINFO|local" <> O.help "The database with the desired schema; local is the dev stack's database")
            <*> scopeParser
    describeParser =
        Describe
            <$> O.strOption (O.long "db" <> O.metavar "CONNINFO|local" <> O.help "local is the dev stack's database")
            <*> O.optional (O.strOption (O.long "out" <> O.metavar "DIR"))
            <*> O.flag KeepExisting ReplaceOwn (O.long "replace" <> O.help "Clear --out first, if schema-diff wrote it")
            <*> scopeParser
    diffParser = twoDatabases Diff
    scopeParser =
        Scope
            <$> fmap
                ("hdb_catalog" :)
                (O.many (O.strOption (O.long "exclude-schema" <> O.metavar "SCHEMA" <> O.help "Leave this schema out; Hasura's hdb_catalog always is")))
            <*> O.flag SkipPartitions IncludePartitions (O.long "include-partitions" <> O.help "Also compare partitions and their columns")
            <*> O.flag
                CheckBodies
                SkipBodyChecks
                ( O.long "no-body-check"
                    <> O.help "Skip recreating SQL-standard function bodies in rolled-back transactions on --to"
                )


report :: ObjectKey -> Change -> Set Address -> IO ()
report key change dependents = do
    let name = render key.address
    TIO.putStrLn [i|#{verb} #{name}#{suffix}|]
    case change of
        Changed old new ->
            for_ (getDiff (T.lines old) (T.lines new)) \case
                First line -> TIO.putStrLn [i|  - #{line}|]
                Second line -> TIO.putStrLn [i|  + #{line}|]
                Both _ _ -> pure ()
        _ -> pure ()
    unless (Set.null dependents) do
        TIO.putStrLn "  depended on by:"
        for_ (Set.toList dependents) \addr -> TIO.putStrLn [i|    #{render addr}|]
  where
    verb :: Text
    verb = case change of
        Added _ -> "added  "
        Dropped _ -> "dropped"
        Changed _ _ -> "changed"
    suffix :: Text
    suffix = case key.aspect of
        Definition -> ""
        Privileges -> " (privileges)"
        Comment -> " (comment)"


-- | The tag a rework gets when none is named: today's date with @_schema_diff@.
defaultTag :: UTCTime -> Text
defaultTag now = let day = formatTime defaultTimeLocale "%Y-%m-%d" now in [i|#{day}_schema_diff|]


summary :: Map ObjectKey Change -> Text
summary changes = [i|#{count isAdded} added, #{count isDropped} dropped, #{count isChanged} changed|]
  where
    count p = L.length (filter p (Map.elems changes))
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False
    isChanged = \case Changed _ _ -> True; _ -> False
