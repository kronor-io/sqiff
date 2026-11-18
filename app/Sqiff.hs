{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sqiff (main) where

import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Data.Char (isAlphaNum)
import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Hasql.Connection qualified as Connection
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
import Sqiff.Baseline (ensureBaseline)
import Sqiff.Body (BodyCheck, checkBodies, sameDefinition)
import Sqiff.Catalog (Addresses, CatalogId (..), Partitions (..), PlFunction (..), PlFunctions (..), Skipped, UserError (..), analyseBodies, bodyErrors, bodyUses, connect, dependenciesFrom, dependenciesOn, describeOn, edgesBetween, failWith, notChecked, pgClass, pgProc, plpgsqlFunctions, relationColumns, tempTableStatements)
import Sqiff.Config (Config (..), Layout (..), Names (..), fill, loadConfig, match, sqitchLayout)
import Sqiff.ConnectionString (ConnectionString, connectionString, redacted)
import Sqiff.Definition
import Sqiff.Diff (BodyText (..), Change (..), Changes, affected, bodiesToAnalyse, bodyText, dependentsBy, diff, reachedFrom)
import Sqiff.Folder (Existing (..), writeFolder)
import Sqiff.Git (Repo, findRepo, showFile)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (context, invert, plan, planWith, renderStep, topoOrder)
import Sqiff.Recipe (Context (viewsAround), Hazard (..), Phase (..), Step (..), Txn (..), recipe, referencedTable, revokeGranted)
import Sqiff.Rewrite (Rewrite (..), fileDrift, rewriteFile)
import Sqiff.Script (splitStatements, withoutTransactionControl)
import Sqiff.Sqitch (PreviousFile (..), SqitchPlan (..), Unit (..), UnitKind (..), deployedChanges, moveToEnd, parsePlan, plannedChanges, readPlan, requireSqitch, reworkEdited, units, writeUnits)
import Sqiff.TableEdit (Baseline (..), Drops (..), Live (..), TableEdit (..), TableRewrite (..), TargetType (..), TypeChange (..), aside, columnOrder, compareTable, firstFailure, foreignKeyError, readOnly, requireScratch, typeChangeFacts, withEditedTable)
import Sqiff.Timing (measure, timed)


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
      ReachableFrom (Set ObjectId)
    | -- | Edges from @pg_depend@, and edges from the bodies of these functions only.
      BodiesOf (Set ObjectId)
    | -- | None; plpgsql_check still lists the functions it cannot analyse.
      NoEdges


-- | Whether a command lists the differences or plans the migration.
data Purpose = ForDiff | ForPlan
    deriving stock (Eq)


-- | The edges a command needs, given the objects that differ between the databases.
neededFor :: Purpose -> Changes -> Needed
neededFor purpose changes
    | Map.null changes = NoEdges
    | purpose == ForPlan = BodiesOf (Set.fromList [k.object | k <- Map.keys changes])
    | null starts = NoEdges
    | otherwise = ReachableFrom (Set.fromList starts)
  where
    starts = [k.object | (k, c) <- Map.toList changes, k.aspect == Definition, removesOrChanges c]
    removesOrChanges = \case Added _ -> False; _ -> True


-- | Edges from @pg_depend@ and, when plpgsql_check is installed, from PL/pgSQL bodies,
-- with notes for stderr. @label@ names the database in the notes.
edgesOf :: Text -> Scope -> Needed -> Connected -> IO ([Edge], [Text])
edgesOf label scope needed connected = timed [i|edgesOf #{label}|] case needed of
    ReachableFrom starts -> reachableEdges label scope starts connected (routineWords connected.schema)
    _ -> do
        let Connected{conn, extra, addresses, tempTableScript = made} = connected
            wantedIds wanted = [ids | (ids, object) <- Map.toList addresses, object `Set.member` wanted]
            only = case needed of
                BodiesOf wanted -> Just (Set.fromList [ids.objectOid | ids <- wantedIds wanted, ids.classId == pgProc])
                _ -> Just Set.empty
        (edges, body) <-
            concurrently
                ( timed [i|edgesOf.dependOn #{label}|] case needed of
                    BodiesOf wanted -> dependenciesOn conn addresses (wantedIds wanted)
                    _ -> pure []
                )
                (timed [i|edgesOf.bodyUses #{label}|] $ fmap (first (edgesBetween addresses)) <$> bodyUses (fromMaybe (conn :| []) (nonEmpty extra)) scope.excluded only made)
        pure (edges <> maybe [] fst body, bodyNotes label (snd <$> body))


bodyNotes :: Text -> Maybe Skipped -> [Text]
bodyNotes label = \case
    Nothing -> ["sqiff: plpgsql_check is not installed; PL/pgSQL body dependencies are left out"]
    Just [] -> []
    Just skipped ->
        let n = length skipped
         in [i|sqiff: plpgsql_check could not analyse #{n} PL/pgSQL functions on #{label}; their body dependencies are left out:|]
                : [[i|  #{name}: #{reason}|] | (name, reason) <- skipped]


-- | The @pg_depend@ edges reachable from @starts@, and the body edges of the PL/pgSQL
-- functions 'bodiesToAnalyse' picks, repeated until nothing new is reached or picked.
reachableEdges :: Text -> Scope -> Set ObjectId -> Connected -> IO (Map ObjectId BodyText) -> IO ([Edge], [Text])
reachableEdges label scope starts Connected{conn, extra, schema, addresses, tempTableScript = made} splitWords = timed [i|reachableEdges #{label}|] do
    let idsOf = Map.fromListWith (<>) [(object, [ids]) | (ids, object) <- Map.toList addresses]
        idsFor = concatMap (\a -> Map.findWithDefault [] a idsOf) . Set.toList
        relationWith ids = Map.lookup (CatalogId pgClass ids.objectOid 0) addresses
        columnsOf = Map.fromListWith (<>) [(rel, [object]) | (ids, object) <- Map.toList addresses, ids.classId == pgClass, ids.subId > 0, Just rel <- [relationWith ids]]
        withColumns xs = xs <> Set.fromList (concatMap (\a -> Map.findWithDefault [] a columnsOf) (Set.toList xs))
    (edges, functions) <-
        concurrently
            (timed "reach.dependFrom0" $ dependenciesFrom conn addresses (idsFor starts))
            ( timed "reach.functionsAndWords"
                $ plpgsqlFunctions (fromMaybe conn (listToMaybe extra)) scope.excluded
                >>= traverse \fs -> do
                    split_ <- splitWords
                    let wordsAt object = fromMaybe (bodyText (maybe "" printed (Map.lookup (definitionKey object) schema))) (Map.lookup object split_)
                        split part = do
                            let described =
                                    [ (f, addr, wordsAt addr, trigger)
                                    | f <- part
                                    , let trigger = f.relid >>= \r -> Map.lookup (CatalogId pgClass r 0) addresses
                                    , Just addr <- [Map.lookup (CatalogId pgProc f.oid 0) addresses]
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
                implicit = [a | o <- fs.implicit, Just a <- [Map.lookup (CatalogId pgProc o 0) addresses]]
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
routineWords :: Schema -> IO (Map ObjectId BodyText)
routineWords schema = do
    let routines = [(k.object, printed def) | (k, def) <- Map.toList schema, k.aspect == Definition, isRoutine k.object]
        split part = do
            let split_ = [(addr, bodyText def) | (addr, def) <- part]
            _ <- evaluate (foldl' (\n (_, b) -> n + HS.size b.wordSet + T.length b.lowered) (0 :: Int) split_)
            pure split_
        parts = [[r | (n, r) <- zip [0 :: Int ..] routines, n `mod` 8 == k] | k <- [0 .. 7]]
    Map.fromList . concat <$> pooledMapConcurrently split parts


-- | How 'dependentsIn' describes the database, leaving out @excluded@ schemas.
dependentsScope :: [Text] -> Scope
dependentsScope excluded = Scope{excluded, partitions = SkipPartitions, bodyCheck = SkipBodyChecks}


-- | For each dropped or changed definition, its dependents by @pg_depend@ and PL/pgSQL
-- bodies, and the other functions whose text names it.
dependentsIn :: [Text] -> IO (Connected, Map ObjectId BodyText) -> Changes -> IO Dependents
dependentsIn excluded described changes
    | Set.null starts = pure (Dependents Map.empty Map.empty Map.empty [] (pure Nothing))
    | otherwise = do
        (connected@Connected{conn, schema, tempTableScript = made}, split_) <- timed "deps.describeDatabase" described
        (edges, notes) <- timed "deps.reachableEdges" $ reachableEdges "--db" (dependentsScope excluded) starts connected (pure split_)
        for_ notes (TIO.hPutStrLn stderr)
        let known = Set.fromList [k.object | k <- Map.keys schema, k.aspect == Definition]
            exact = affected known edges changes
            viewsOf object = [v | a <- Set.toList (Map.findWithDefault Set.empty object exact), isView a, Just v <- [isRelation a]]
        measure "deps.affected" (Map.foldl' (\n s -> n + Set.size s) (0 :: Int) exact)
        viewColumns <- timed "deps.relationColumns" $ relationColumns conn (Set.toList (Set.fromList (concatMap viewsOf (Set.toList starts))))
        let routines = Map.toList split_
            skip = Set.fromList (map (.object) (Map.keys changes)) <> fold exact
            lower = T.toLower . nameText
            throughViews column object =
                [ lower v.schema : lower v.name : [lower c | Just c <- [column], keepsName c]
                | v <- viewsOf object
                , let keepsName c = c `Set.member` Map.findWithDefault Set.empty v viewColumns
                ]
            -- The words a function body must contain to use the object: a column by its
            -- table and name, a dropped relation by its name, and both through views.
            named object = case object of
                Column t c -> [lower t.name, lower c] : throughViews (Just c) object
                _ | Just q <- isRelation object, droppedHere object -> [lower q.name] : throughViews Nothing object
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
    droppedHere object = case Map.lookup (definitionKey object) changes of
        Just (Dropped _) -> True
        _ -> False
    starts = Set.fromList [k.object | (k, c) <- Map.toList changes, k.aspect == Definition, removesOrChanges c]
    removesOrChanges = \case Added _ -> False; _ -> True


-- | What depends on an edit's dropped or changed objects, in the database it runs on.
data Dependents = Dependents
    { exact :: Map ObjectId (Set ObjectId)
    , named :: Map ObjectId (Set ObjectId)
    , dbSchema :: Schema
    , dbEdges :: [Edge]
    , dbTempTables :: IO (Maybe Text)
    -- ^ The statements that make plpgsql_check's temp tables there, from 'tempTableStatements'.
    }


-- | For each retyped column, the statements that drop the views, policies, triggers and
-- routines that hold it, and the ones that create them again.
viewsAroundTypeChanges :: Dependents -> [(QName, Identifier, ObjectId)] -> Map (QName, Identifier) (Set ObjectId, ([Text], [Text]))
viewsAroundTypeChanges deps retyped =
    Map.fromList
        [ ((t, c), (members, (map dropStatement (reverse order), concatMap createAgain order)))
        | (t, c, column) <- retyped
        , let closure = Map.findWithDefault Set.empty column deps.exact
              onColumn = Set.filter (\a -> isPolicyOrTrigger a && any (\e -> e.dependent == a && e.referenced == column) deps.dbEdges) closure
              members = held column closure (Set.filter isView closure <> onColumn)
              order = topoOrder members [(d, r) | e <- deps.dbEdges, let d = relationOf e.dependent, let r = relationOf e.referenced, d `Set.member` members, r `Set.member` members]
        , not (Set.null members)
        ]
  where
    isPolicyOrTrigger = \case
        Policy{} -> True
        Trigger{} -> True
        _ -> False
    -- Adds the routines whose definitions hold the column or one of the members, until
    -- no more do.
    held column closure members =
        let views = Set.filter isView members
            holds r = case printed <$> Map.lookup (definitionKey r) deps.dbSchema of
                Just def
                    | standardBody def -> any (\e -> e.dependent == r && (e.referenced == column || relationOf e.referenced `Set.member` members)) deps.dbEdges
                    | otherwise -> any (\v -> qualifiedName v `T.isInfixOf` signature def) views
                Nothing -> False
            more = Set.filter (\r -> isRoutine r && r `Set.notMember` members && holds r) closure
         in if Set.null more then members else held column closure (members <> more)
    signature = T.unlines . takeWhile (\l -> not ("AS " `T.isPrefixOf` T.stripStart l || "BEGIN ATOMIC" `T.isInfixOf` l)) . T.lines
    ctx0 = context Map.empty Map.empty Map.empty [] Map.empty
    stepsFor key c = map (.sql) (recipe ctx0 key c)
    createAgain a =
        concat
            [ [revokeGranted (qualifiedName a) | aspect == Privileges, isView a] <> stepsFor key (Added def)
            | aspect <- [Definition, Privileges]
            , let key = ObjectKey aspect a
            , Just def <- [Map.lookup key deps.dbSchema]
            ]
            <> [ printed def
               | (k, def) <- Map.toList deps.dbSchema
               , k.aspect == Definition
               , case k.object of
                    Index _ -> [i| ON #{qualifiedName a} |] `T.isInfixOf` printed def
                    Trigger t _ -> isRelation a == Just t
                    _ -> False
               ]
            <> concat
                [ stepsFor k (Added def)
                | (k, def) <- Map.toList deps.dbSchema
                , k.aspect == Comment
                , k.object == a || (relationOf k.object == a && k.object /= a)
                ]


-- | The relation a column belongs to; any other object is its own.
relationOf :: ObjectId -> ObjectId
relationOf = \case
    Column t _ -> Table t
    OtherColumn OfView t _ -> View t
    OtherColumn OfMaterializedView t _ -> MaterializedView t
    OtherColumn OfCompositeType t _ -> Unknown (Address "composite type" [nameText t.schema, nameText t.name] [])
    OtherColumn OfForeignTable t _ -> Unknown (Address "foreign table" [nameText t.schema, nameText t.name] [])
    object -> object


-- | An object's name as SQL writes it after its kind.
qualifiedName :: ObjectId -> Text
qualifiedName object = case (toAddress object).objectNames of
    [s, n] -> qualified (qname s n)
    names -> T.intercalate "." names


-- | The statement that drops a view, routine, policy or trigger around a type change.
dropStatement :: ObjectId -> Text
dropStatement object = case object of
    Routine _ q args ->
        let keyword = (toAddress object).objectType
            list = T.intercalate ", " (map typeText args)
         in [i|drop #{keyword} #{qualified q}(#{list})|]
    Policy t n -> [i|drop policy #{quoted n} on #{qualified t}|]
    Trigger t n -> [i|drop trigger #{quoted n} on #{qualified t}|]
    _ -> let keyword = (toAddress object).objectType in [i|drop #{keyword} #{qualifiedName object}|]


-- | A SQL-standard body, which PostgreSQL parses once and tracks in pg_depend.
standardBody :: Text -> Bool
standardBody def = "BEGIN ATOMIC" `T.isInfixOf` def || any ("RETURN " `T.isPrefixOf`) (T.lines def)


-- | For a foreign key constraint's definition, each of its columns' types paired with the
-- referenced column's, by @typeIn table column@.
foreignKeyTypes :: (QName -> Identifier -> Maybe Text) -> ObjectId -> Text -> Maybe [(Text, Text)]
foreignKeyTypes typeIn constraint def = do
    TableConstraint table _ <- Just constraint
    rest <- T.stripPrefix "FOREIGN KEY " def
    let (cols, afterCols) = columnList rest
    refPart <- T.stripPrefix " REFERENCES " afterCols
    let (refTable, refRest) = T.break (== '(') refPart
        (refCols, _) = columnList refRest
        referenced = case T.break (== '.') refTable of
            (name, "") -> qname "public" (unquote name)
            (schema, name) -> qname (unquote schema) (unquote (T.drop 1 name))
    guard (length cols == length refCols && not (null cols))
    for (zip cols refCols) \(a, b) -> (,) <$> typeIn table a <*> typeIn referenced b
  where
    columnList x = let (inside, after) = T.break (== ')') (T.drop 1 x) in (map (identifier . unquote . T.strip) (T.split (== ',') inside), T.drop 1 after)
    unquote c = fromMaybe c (T.stripPrefix "\"" c >>= T.stripSuffix "\"")


-- | Connections per database; the describe queries run over all of them at once.
describeConnections :: Int
describeConnections = 4


-- | A described database and its open connections.
data Connected = Connected
    { conn :: Connection.Connection
    , extra :: [Connection.Connection]
    -- ^ More connections to spread queries over.
    , schema :: Schema
    , addresses :: Addresses
    , tempTableScript :: IO (Maybe Text)
    -- ^ The statements that make plpgsql_check's temp tables there.
    }


-- | A connection to the database the table command edits in rolled-back transactions.
connectScratch :: ConnectionString -> IO Connection.Connection
connectScratch db = do
    conn <- connect db
    requireScratch (redacted db) conn
    pure conn


-- | A database's schema over 'describeConnections' connections.
describeDatabase :: Scope -> ConnectionString -> IO Connected
describeDatabase scope db = do
    tempTables <- async (either (const Nothing) id <$> tryAny (bracket (connect db) Connection.release tempTableStatements))
    (conn, extra) <- concurrently (connect db) (replicateConcurrently (describeConnections - 1) (connect db))
    (schema, addresses) <- describeOn (conn : extra) scope.excluded scope.partitions Nothing
    pure Connected{conn, extra, schema, addresses, tempTableScript = wait tempTables}


-- | Reads both databases at the same time, then the edges each command needs, given what
-- differs, alongside the body checks; the edges of @--to@ only for 'ForPlan'.
readBoth :: Config -> Scope -> Source -> ConnectionString -> Purpose -> IO ((Schema, [Edge]), (Schema, [Edge]), Map ObjectKey BodyCheck)
readBoth config scope from toDb purpose = do
    toDescribed <- async (timed "describe --to" $ describeDatabase scope toDb)
    fromDb <- timed "connInfoOf" $ connInfoOf config from
    (fromDatabase, toDatabase) <-
        timed "readBoth.describe" $ concurrently (timed "describe --from" $ describeDatabase scope fromDb) (wait toDescribed)
    let fromSchema = fromDatabase.schema
        toSchema = toDatabase.schema
        plainChanges = diff (\_ a b -> a == b) fromSchema toSchema
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
bodyChecks :: Scope -> ConnectionString -> Schema -> Changes -> IO (Map ObjectKey BodyCheck)
bodyChecks scope toDb toSchema plainChanges
    | scope.bodyCheck == CheckBodies =
        bracket (connect toDb) Connection.release \conn ->
            timed "bodyChecks" $ checkBodies conn (Map.restrictKeys toSchema addedOrChanged)
    | otherwise = pure Map.empty
  where
    addedOrChanged = Map.keysSet (Map.filter (\case Dropped _ -> False; _ -> True) plainChanges)


-- | What @--from@ names: a connection string, or the baseline database built from a revision.
data Source
    = Database ConnectionString
    | BaselineAt Text


source :: Text -> Source
source s
    | s == "master" = BaselineAt s
    | Just rev <- T.stripPrefix "rev:" s = BaselineAt rev
    | otherwise = Database (connectionString s)


connInfoOf :: Config -> Source -> IO ConnectionString
connInfoOf config = \case
    Database db -> pure db
    BaselineAt rev -> ensureBaseline config.baseline rev


data Command
    = Describe ConnectionString (Maybe FilePath) Existing Scope
    | Diff Source ConnectionString Scope
    | Plan Source ConnectionString Scope
    | Sqitch Source ConnectionString Scope SqitchOptions
    | TableFile FilePath ConnectionString TableOptions


data TableOptions = TableOptions
    { output :: Output
    , compareWith :: CompareWith
    , drops :: Drops
    , base :: Text
    , project :: FilePath
    , top :: Maybe FilePath
    , planPath :: Maybe FilePath
    , tableTag :: Maybe Text
    , renames :: [(Identifier, Identifier)]
    , sameTypeSwap :: SameTypeSwap
    , fileMode :: FileMode
    , liveFrom :: Maybe ConnectionString
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
    , topDir :: Maybe FilePath
    , planFile :: Maybe FilePath
    , tag :: Maybe Text
    , dryRun :: Bool
    }


main :: IO ()
main =
    handleAny unexpected
        . handle (\(UserError message) -> TIO.hPutStrLn stderr [i|sqiff: #{message}|] >> exitWith (ExitFailure 2))
        $ O.execParser (O.info (commandParser O.<**> O.helper) O.fullDesc)
        >>= \case
            Describe db out replace scope' -> do
                scope <- (`withConfigured` scope') <$> loadConfig "."
                conns <- timed "describe.connect" $ replicateConcurrently describeConnections (connect db)
                (schema, _) <- timed "describe.describeOn" $ describeOn conns scope.excluded scope.partitions Nothing
                case out of
                    Just dir -> writeFolder replace dir schema
                    Nothing ->
                        for_ (Map.toList (Map.fromListWith (+) [((toAddress k.object).objectType, 1 :: Int) | k <- Map.keys schema])) \(kind, n) ->
                            TIO.putStrLn [i|#{n}\t#{kind}|]
            Diff from toDb scope' -> do
                config <- loadConfig "."
                let scope = withConfigured config scope'
                ((fromSchema, edges), (toSchema, _), checks) <- timed "readBoth" $ readBoth config scope from toDb ForDiff
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    known = Set.fromList [k.object | k <- Map.keys fromSchema, k.aspect == Definition]
                    affectedBy = affected known edges changes
                    dependentsOf key = case key.aspect of
                        Definition -> Map.findWithDefault Set.empty key.object affectedBy
                        _ -> Set.empty
                measure "diff.diffAndAffected" (Map.size changes + Map.foldl' (\n s -> n + Set.size s) 0 affectedBy)
                -- In catalog order: by aspect, then kind and names as the catalog spells them.
                for_ (L.sortOn (\(key, _) -> (key.aspect, toAddress key.object)) (Map.toList changes)) \(key, change) ->
                    report key change (dependentsOf key)
                TIO.putStrLn (summary changes)
                unless (Map.null changes) $ exitWith (ExitFailure 1)
            Plan from toDb scope' -> do
                config <- loadConfig "."
                let scope = withConfigured config scope'
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- timed "readBoth" $ readBoth config scope from toDb ForPlan
                let steps = plan checks fromSchema toSchema fromEdges toEdges (diff (sameDefinition checks) fromSchema toSchema)
                measure "plan.planRender" (T.length (T.unlines (map renderStep steps)))
                for_ steps (TIO.putStrLn . renderStep)
            Sqitch from toDb scope' opts -> do
                requireSqitch
                config <- loadConfig opts.projectDir
                let scope = withConfigured config scope'
                layout <- layoutOf opts.projectDir opts.topDir opts.planFile
                sqitchPlan <- readPlan (opts.projectDir </> layout.planFile)
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- timed "readBoth" $ readBoth config scope from toDb ForPlan
                now <- getCurrentTime
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    steps = plan checks fromSchema toSchema fromEdges toEdges changes
                    inverseCtx = context Map.empty toSchema fromSchema toEdges (Map.map invert changes)
                    written = units config.names (const True) inverseCtx sqitchPlan now steps
                    tagName = fromMaybe (defaultTag now) opts.tag
                measure "sqitch.planUnits" (sum [length u.deploy + length u.revert | u <- written])
                if opts.dryRun
                    then for_ written \u -> do
                        let kind = case u.kind of AddChange -> "add" :: Text; ReworkChange -> "rework"
                            count = length u.deploy
                            changeName = u.changeName
                            note = u.note
                        TIO.putStrLn [i|#{kind}\t#{changeName}\t#{count} statements\t#{note}|]
                    else writeUnits config.templates opts.projectDir layout.topDir sqitchPlan tagName written
            TableFile file db opts -> tableCommand file db opts
  where
    unexpected e = case fromException e of
        Just code -> throwIO (code :: ExitCode)
        Nothing -> TIO.hPutStrLn stderr [i|sqiff: #{displayException e}|] >> exitWith (ExitFailure 2)
    withConfigured :: Config -> Scope -> Scope
    withConfigured config scope = scope{excluded = config.excludeSchemas <> scope.excluded}


-- | sqitch's top directory and plan file in @root@, unless both are given.
layoutOf :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO Layout
layoutOf root top planFile = case (top, planFile) of
    (Just topDir, Just given) -> pure Layout{topDir, planFile = given}
    _ -> do
        found <- sqitchLayout root
        pure Layout{topDir = fromMaybe found.topDir top, planFile = fromMaybe found.planFile planFile}


-- | What the table command works on: its options, the deploy file before and after the
-- edit, and where they live.
data TableEnv = TableEnv
    { opts :: TableOptions
    , db :: ConnectionString
    , file :: FilePath
    , root :: FilePath
    , repo :: Repo
    , config :: Config
    , layout :: Layout
    , sqitchChange :: Text
    , table :: QName
    , statements :: [Text]
    , original :: Text
    }


-- | A column whose type the edit changes.
data Retyped = Retyped
    { table :: QName
    , column :: Identifier
    , newType :: TypeName
    }


retypedObject :: Retyped -> ObjectId
retypedObject r = Column r.table r.column


-- | The functions the body check finds broken by the edit, and the ones it passes.
data BodyResults = BodyResults
    { breaking :: Map ObjectId Text
    , passed :: Set ObjectId
    }


-- | What the comparison and the checks found.
data Evidence = Evidence
    { edit :: TableEdit
    , deps :: Dependents
    , bodies :: BodyResults
    , retyped :: [Retyped]
    , foreignKeyBreaks :: Map ObjectId [(ObjectId, Text)]
    , typeFacts :: Map ObjectId TypeChange
    , branchChanges :: [(Text, [Text])]
    , pendingRenames :: [(Identifier, Identifier)]
    , renamedFrom :: Set ObjectId
    }


-- | The migration's steps and what the command reports about them.
data Findings = Findings
    { steps :: [Step]
    , renameSteps :: [Step]
    , newRenames :: [(Identifier, Identifier)]
    , usage :: [Text]
    , blocking :: [ObjectId]
    , renameLike :: [(Identifier, Identifier)]
    , same :: Text
    , withViews :: Context -> Context
    }


-- | Prints the migration that turns the table into what its edited file creates, or
-- writes it with sqitch.
tableCommand :: FilePath -> ConnectionString -> TableOptions -> IO ()
tableCommand file db opts = do
    requireSqitch
    root <- makeAbsolute opts.project
    config <- loadConfig root
    layout <- layoutOf root opts.top opts.planPath
    (absFile, changeName, table) <- locateTable config.names layout root file
    statements <- parseFile file "" =<< readFileUtf8 absFile
    let describeWithWords = describeDatabase (dependentsScope config.excludeSchemas) db >>= \c -> (c,) <$> routineWords c.schema
        migrating = opts.fileMode == MigrateToFile
    early <- if migrating && (opts.compareWith == LiveDatabase || not (null opts.renames)) then Just <$> async describeWithWords else pure Nothing
    repo <- findRepo root
    original <- timed "table.baseFile" $ showFile repo opts.base absFile
    let env = TableEnv{opts, db, file, root, repo, config, layout, sqitchChange = changeName, table, statements, original}
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
        deployDir = env.root </> env.layout.topDir </> "deploy"
    unless (null opts.renames) $ failWith "--rename works with the file comparison, not --rewrite-file"
    current <- readFileUtf8 env.file
    conn <- liftIO (connectScratch env.db)
    (live, liveConn) <- case opts.liveFrom of
        Nothing -> pure (SameDatabase, conn)
        Just name -> liftIO do
            c <- connect name
            readOnly c
            pure (OtherDatabase c, c)
    let liveDb = redacted (fromMaybe env.db opts.liveFrom)
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


-- | The file's absolute path, its sqitch change and its table, by the table name pattern.
locateTable :: Names -> Layout -> FilePath -> FilePath -> IO (FilePath, Text, QName)
locateTable names layout root file = do
    absFile <- makeAbsolute file
    let deployDir = root </> layout.topDir </> "deploy"
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


-- | A deploy file's statements; @label@ names the version in a parse error.
parseFile :: MonadIO m => FilePath -> Text -> Text -> m [Text]
parseFile file label = either (\e -> failWith [i|#{file}#{label}: #{e}|]) pure . (withoutTransactionControl <=< splitStatements)


atBase :: TableOptions -> Text
atBase opts = let base = opts.base in [i| at #{base}|]


renameSql :: QName -> (Identifier, Identifier) -> Text
renameSql table (old, new) = [i|alter table #{qualified table} rename column #{quoted old} to #{quoted new}|]


say :: MonadIO m => Text -> m ()
say = liftIO . TIO.putStrLn


-- | Compares the table, checks what depends on the edit, and prints or writes the migration.
compareAndReport :: IO (Connected, Map ObjectId BodyText) -> RIO TableEnv ()
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
    let renamedFrom = Set.fromList [Column table old | (old, _) <- opts.renames]
    deps <- timed "table.dependentsIn" $ liftIO (dependentsIn env.config.excludeSchemas described (edit.changes <> Map.fromSet (const (Dropped (Printed ""))) (Set.map (ObjectKey Definition) renamedFrom)))
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
            current <- liftIO (readPlan (root </> env.layout.planFile))
            basePlan <- liftIO (parsePlan <$> showFile env.repo opts.base (root </> env.layout.planFile))
            let before = Set.fromList basePlan.changes
            for (filter (\n -> n `Set.notMember` before && forTable env.config.names env.table n) (nubOrd current.changes)) \n -> do
                script <- readFileUtf8 (root </> env.layout.topDir </> "deploy" </> T.unpack n <.> "sql")
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


retypedColumns :: TableEdit -> [Retyped]
retypedColumns edit =
    [ Retyped{table = t, column = c, newType = n.colType}
    | (k, Changed old new) <- Map.toList edit.changes
    , k.aspect == Definition
    , ColumnDefinition o <- [old]
    , ColumnDefinition n <- [new]
    , o.colType /= n.colType || o.collation /= n.collation
    , Column t c <- [k.object]
    ]


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
            , k@TableConstraint{} <- Set.toList (Map.findWithDefault Set.empty addr deps.exact)
            , Just def <- [printed <$> Map.lookup (definitionKey k) deps.dbSchema]
            , Just types <- [foreignKeyTypes typeIn k def]
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
              | k@(TableConstraint onTable _) <- Set.toList (Map.findWithDefault Set.empty addr deps.exact)
              , Just def <- [printed <$> Map.lookup (definitionKey k) deps.dbSchema]
              , Just referenced <- [referencedTable def]
              , let other = if onTable == table then referenced else qualified onTable
              , other /= qualified table
              ]
    convertNote = MayFail "fails when an existing value does not convert to the new type"
    withTypeFacts s = case Map.lookup s.object ev.typeFacts of
        Just (TypeChange rewrite target)
            | MayRewriteTable `elem` s.hazards ->
                let rewrites = rewrite == RewritesFiles
                    domain = target == DomainType
                 in let kept = concatMap (\h -> if h == MayRewriteTable then [RewritesTable | rewrites] else [h | h /= convertNote || rewrites || domain]) s.hazards
                     in s{hazards = kept <> concat [LocksOther o : [RechecksForeignKey o | rewrites] | o <- otherTables s.object]}
        _ -> s
    breaksFor addr = [a | a <- Set.toList (Map.findWithDefault Set.empty addr deps.named <> Map.findWithDefault Set.empty addr deps.exact), a `Map.member` breaking]
    dependents = deps.exact
    mentions = Map.map (Set.filter (\a -> a `Map.notMember` breaking && a `Set.notMember` passed)) deps.named
    retypedAddresses = Set.fromList (map retypedObject retyped)
    around = viewsAroundTypeChanges deps [(r.table, r.column, retypedObject r) | r <- retyped]
    withViews c = c{viewsAround = Map.map snd around}
    aroundOf addr = fold [members | r <- retyped, retypedObject r == addr, Just (members, _) <- [Map.lookup (r.table, r.column) around]]
    foreignKeysFor addr = Map.findWithDefault [] addr ev.foreignKeyBreaks
    renameHazard = MayFail ("breaks running code that still uses the old name" <> foldMap ("; " <>) env.config.renameHint)
    renameSteps =
        [ Step DropPhase (-1) addr (renameSql table (old, new)) Shared [renameHazard] (definitionKey addr) (Changed def def)
        | (old, new) <- ev.pendingRenames
        , let addr = Column table new
              def = Map.findWithDefault (Printed "") (definitionKey addr) edit.desired
        ]
    steps = renameSteps <> map withTypeFacts (planWith withViews Map.empty edit.current edit.desired [] [] edit.changes)
    same :: Text
    same
        | opts.compareWith == LiveDatabase = "the file matches the table"
        | not (null ev.branchChanges) = "this branch's changes already cover the file"
        | otherwise = let base = opts.base in [i|the file has no schema changes since #{base}|]
    columnsWhere p = [(c, def.colType) | (k, ch) <- Map.toList edit.changes, k.aspect == Definition, p ch, Column _ c <- [k.object], Just def <- [asColumn (changeText ch)]]
    changeText = \case Added d -> d; Dropped d -> d; Changed _ d -> d
    renameLike =
        [ (old, new)
        | null opts.renames
        , opts.sameTypeSwap == AskIfRename
        , (old, oldType) <- columnsWhere (\case Dropped _ -> True; _ -> False)
        , (new, newType) <- columnsWhere (\case Added _ -> True; _ -> False)
        , oldType == newType
        ]
    renameNotes =
        [ [i|#{old} is dropped and #{new} added with the same type; pass --rename #{old}:#{new} if it is a rename, or --drop-and-add if it is not|]
        | (oldName, newName) <- renameLike
        , let old = nameText oldName
              new = nameText newName
        ]
    dropped addr = case Map.lookup (definitionKey addr) edit.changes of
        Just (Dropped _) -> True
        _ -> False
    -- What goes when a dropped column goes: its indexes and its table's constraints.
    goesWith = \case
        Index _ -> True
        TableConstraint t _ -> t == table
        _ -> False
    listed = fold dependents <> fold mentions
    -- Routines are listed by name, with their arguments only when the name is overloaded.
    overloaded = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(withoutArgs a, 1) | a <- Set.toList listed]))
    withoutArgs = \case
        Routine kind q _ -> Routine kind q []
        a -> a
    short a =
        let addr = toAddress a
            objectType = addr.objectType
            kind = if isRoutine a then "" else [i|#{objectType} |] :: Text
            name = T.intercalate "." addr.objectNames
            args = if withoutArgs a `Set.member` overloaded then let list = T.intercalate ", " addr.objectArgs in [i|(#{list})|] else "" :: Text
         in [i|#{kind}#{name}#{args}|] :: Text
    item d = [i|    #{short d}|] :: Text
    detail t = [i|      #{t}|] :: Text
    section :: Text -> [ObjectId] -> [Text]
    section heading = \case
        [] -> []
        ds -> [i|  #{heading}|] : map item ds
    breaksSection = \case
        [] -> []
        fs -> "  breaks" : concat [[item f, detail (Map.findWithDefault "" f breaking)] | f <- fs]
    foreignKeySection = \case
        [] -> []
        ks -> "  cannot be rebuilt with the new type" : concat [[item k, detail err] | (k, err) <- ks]
    -- A trigger on a view that is created again comes back with it.
    recreated addr d =
        d `Set.member` aroundOf addr || case d of
            Trigger t _ -> any (\v -> isView v && isRelation v == Just t) (aroundOf addr)
            _ -> False
    rebuiltWith = \case
        Index _ -> True
        TableConstraint{} -> True
        Sequence _ -> True
        _ -> False
    usage =
        concat
            [ T.intercalate "." (toAddress addr).objectNames
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
    sqitchPlan <- timed "write.readPlan" $ liftIO (readPlan (root </> env.layout.planFile))
    basePlan <- timed "write.basePlan" $ liftIO (parsePlan <$> showFile env.repo opts.base (root </> env.layout.planFile))
    now <- liftIO getCurrentTime
    let inverseCtx = f.withViews (context Map.empty edit.desired edit.current [] (Map.map invert edit.changes))
        renameBack =
            [ Step DropPhase (-1) (Column table old) (renameSql table (new, old)) Shared [] s.key s.change
            | (s, (old, new)) <- zip f.renameSteps f.newRenames
            ]
        withRenameBack u
            | any (`elem` f.renameSteps) u.deploy = u{revert = u.revert <> renameBack}
            | otherwise = u
        isIndex s = case s.object of Index _ -> True; _ -> False
        written = map withRenameBack (units env.config.names isIndex inverseCtx sqitchPlan now f.steps)
        tagName = fromMaybe (defaultTag now) opts.tableTag
        entries p = length (filter (== changeName) p.changes)
        alreadyReworked = entries sqitchPlan > entries basePlan
    previous <- parseFile env.file (atBase opts) env.original
    oldFileFails <- timed "table.compare3" $ liftIO (withEditedTable conn baseline table env.statements (pure ()) (const (firstFailure conn previous)))
    let baseRev = opts.base
    for_ oldFileFails \err ->
        say [i|the file at #{baseRev} fails on the edited table (#{err}); the rework's revert does nothing and the migrations revert the table|]
    timed "write.writeUnits" $ liftIO (writeUnits env.config.templates root env.layout.topDir sqitchPlan tagName written)
    unless alreadyReworked do
        afterUnits <- liftIO (readPlan (root </> env.layout.planFile))
        timed "write.reworkEdited"
            . liftIO
            $ reworkEdited root env.layout.topDir afterUnits tagName changeName [i|sqiff: #{changeName} as edited|] env.original (maybe PreviousFileRuns (const PreviousFileFails) oldFileFails)
    when alreadyReworked . liftIO $ moveToEnd (root </> env.layout.planFile) changeName
    for_ written \u -> let name = u.changeName in say [i|added #{name}|]
    say
        ( if alreadyReworked
            then [i|moved the branch's rework of #{changeName} after the new migrations; if this branch is deployed locally, revert it to before the rework first|]
            else [i|reworked #{changeName}|]
        )


-- | Whether a sqitch change is about the table, by the names sqiff gives: one of its
-- indexes, or <schema>.<table> in a migration's name.
forTable :: Names -> QName -> Text -> Bool
forTable names t n =
    let key = dotted t
        indexes = fill names.index [("schema", nameText t.schema), ("table", nameText t.name), ("name", "")]
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
            <*> dbOption "db" "A database with the table as it is now; changes are rolled back"
            <*> ( TableOptions
                    <$> O.flag PrintMigration WriteMigration (O.long "write" <> O.help "Add the migration with sqitch and rework the table's change")
                    <*> O.flag FileAtBase LiveDatabase (O.long "against-database" <> O.help "Compare with the live table instead of the file at --base; shows drift too")
                    <*> O.flag ReportDrops AllowDrops (O.long "allow-drops" <> O.help "With --against-database, drop columns and constraints that are not in the file")
                    <*> O.strOption (O.long "base" <> O.metavar "REV" <> O.value "master" <> O.help "Git revision with the file before your edit")
                    <*> projectOption
                    <*> topDirOption
                    <*> planFileOption
                    <*> O.optional (O.strOption (O.long "tag" <> O.metavar "NAME" <> O.help "Tag to add when the rework needs one"))
                    <*> O.many (O.option (O.eitherReader renameArg) (O.long "rename" <> O.metavar "OLD:NEW" <> O.help "Column OLD is renamed to NEW; repeat for more columns"))
                    <*> O.flag AskIfRename DropAndAdd (O.long "drop-and-add" <> O.help "A column dropped and one added with the same type are not a rename")
                    <*> O.flag MigrateToFile RewriteFromDatabase (O.long "rewrite-file" <> O.help "Print the file rewritten to create the table as --db has it; with --write, write it and rework the change")
                    <*> O.optional (connectionString <$> O.strOption (O.long "live-from" <> O.metavar "CONNINFO" <> O.help "With --rewrite-file, read the table from this database, read-only, instead of --db; --db still builds the file"))
                )
    renameArg s = case T.split (== ':') (T.pack s) of
        [old, new] | not (T.null old), not (T.null new) -> Right (identifier old, identifier new)
        _ -> Left "expected OLD:NEW"
    dbOption name help =
        connectionString
            <$> O.strOption
                ( O.long name
                    <> O.metavar "CONNINFO"
                    <> O.value ""
                    <> O.help (help <> "; left out, libpq's PGHOST, PGPORT, PGUSER, PGDATABASE or PGSERVICE name it")
                )
    projectOption = O.strOption (O.long "project" <> O.metavar "DIR" <> O.value "." <> O.help "Where sqitch.conf and sqiff.yaml are")
    topDirOption = O.optional (O.strOption (O.long "top-dir" <> O.metavar "DIR" <> O.help "sqitch's top directory; sqitch config's core.top_dir when left out"))
    planFileOption = O.optional (O.strOption (O.long "plan-file" <> O.metavar "FILE" <> O.help "sqitch's plan; sqitch config's core.plan_file when left out"))
    sqitchOptions =
        SqitchOptions
            <$> projectOption
            <*> topDirOption
            <*> planFileOption
            <*> O.optional (O.strOption (O.long "tag" <> O.metavar "NAME" <> O.help "Tag to add when a rework needs one"))
            <*> O.switch (O.long "dry-run" <> O.help "List the changes without writing them")
    twoDatabases constructor =
        constructor
            <$> fmap
                source
                ( O.strOption
                    ( O.long "from"
                        <> O.metavar "CONNINFO|master|rev:REV"
                        <> O.help "The database to change; master or rev:REV builds or updates the baseline database at that revision and uses it"
                    )
                )
            <*> dbOption "to" "The database with the desired schema"
            <*> scopeParser
    describeParser =
        Describe
            <$> dbOption "db" "The database to read"
            <*> O.optional (O.strOption (O.long "out" <> O.metavar "DIR"))
            <*> O.flag KeepExisting ReplaceOwn (O.long "replace" <> O.help "Clear --out first, if sqiff wrote it")
            <*> scopeParser
    diffParser = twoDatabases Diff
    scopeParser =
        Scope
            <$> O.many (O.strOption (O.long "exclude-schema" <> O.metavar "SCHEMA" <> O.help "Leave this schema out, besides sqiff.yaml's exclude-schemas"))
            <*> O.flag SkipPartitions IncludePartitions (O.long "include-partitions" <> O.help "Also compare partitions and their columns")
            <*> O.flag
                CheckBodies
                SkipBodyChecks
                ( O.long "no-body-check"
                    <> O.help "Skip recreating SQL-standard function bodies in rolled-back transactions on --to"
                )


report :: ObjectKey -> Change Definition -> Set ObjectId -> IO ()
report key change dependents = do
    let name = render key.object
    TIO.putStrLn [i|#{verb} #{name}#{suffix}|]
    case change of
        Changed old new ->
            for_ (getDiff (T.lines (printed old)) (T.lines (printed new))) \case
                First line -> TIO.putStrLn [i|  - #{line}|]
                Second line -> TIO.putStrLn [i|  + #{line}|]
                Both _ _ -> pure ()
        _ -> pure ()
    unless (Set.null dependents) do
        TIO.putStrLn "  depended on by:"
        for_ (L.sortOn toAddress (Set.toList dependents)) \addr -> TIO.putStrLn [i|    #{render addr}|]
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


-- | The tag a rework gets when none is named: today's date with @_sqiff@.
defaultTag :: UTCTime -> Text
defaultTag now = let day = formatTime defaultTimeLocale "%Y-%m-%d" now in [i|#{day}_sqiff|]


summary :: Changes -> Text
summary changes = [i|#{count isAdded} added, #{count isDropped} dropped, #{count isChanged} changed|]
  where
    count p = L.length (filter p (Map.elems changes))
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False
    isChanged = \case Changed _ _ -> True; _ -> False
