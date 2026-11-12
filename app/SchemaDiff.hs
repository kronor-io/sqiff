{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SchemaDiff (main) where

import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Hasql.Connection qualified as Connection
import Kronor.SchemaDiff.Baseline (ensureBaseline)
import Kronor.SchemaDiff.Body (BodyCheck, checkBodies, sameDefinition)
import Kronor.SchemaDiff.Catalog (Address (..), Addresses, Aspect (..), Edge (..), ObjectKey (..), PlFunction (..), PlFunctions (..), Schema, Skipped, UserError (..), analyseBodies, bodyErrors, bodyUses, connect, dependenciesFrom, dependenciesOn, describeOn, edgesBetween, failWith, plpgsqlFunctions, relationColumns, render, tempTableStatements)
import Kronor.SchemaDiff.Diff (BodyText (..), Change (..), affected, bodiesToAnalyse, bodyText, dependentsBy, diff, reachedFrom)
import Kronor.SchemaDiff.Folder (writeFolder)
import Kronor.SchemaDiff.Object (Object (..), QName (..), fromAddress, qualified, quoteIdent)
import Kronor.SchemaDiff.Plan (context, invert, plan, planWith, renderStep, topoOrder)
import Kronor.SchemaDiff.Recipe (ColumnDef (..), Context (viewsAround), Hazard (..), Phase (..), Step (..), Txn (..), parseColumn, recipe, referencedTable, revokeGranted)
import Kronor.SchemaDiff.Script (splitStatements, withoutTransactionControl)
import Kronor.SchemaDiff.Sqitch (SqitchPlan (..), Unit (..), UnitKind (..), parsePlan, readPlan, requireSqitch, reworkEdited, units, writeUnits)
import Kronor.SchemaDiff.TableEdit (Baseline (..), TableEdit (..), aside, compareTable, firstFailure, foreignKeyError, typeChangeFacts, withEditedTable)
import Kronor.SchemaDiff.Timing (measure, timed)
import Options.Applicative qualified as O
import RIO
import RIO.Directory (makeAbsolute)
import RIO.FilePath (makeRelative, (</>))
import RIO.HashSet qualified as HS
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.NonEmpty (nonEmpty)
import RIO.Set qualified as Set
import RIO.Text qualified as T
import System.Environment (lookupEnv)
import System.Process.Typed (proc, readProcess, setWorkingDir)


data Scope = Scope
    { excluded :: [Text]
    , includePartitions :: Bool
    , checkFunctionBodies :: Bool
    }


-- | Which edges a command uses.
data Needed
    = -- | Edges from @pg_depend@, and the body edges 'affected' can reach from these.
      ReachableFrom (Set Address)
    | -- | Edges from @pg_depend@, and edges from the bodies of these functions only.
      BodiesOf (Set Address)
    | -- | None; plpgsql_check still lists the functions it cannot analyse.
      NoEdges


-- | The edges a command needs, given the objects that differ between the databases.
-- @diff@ follows dependents of dropped and changed objects through every edge. @plan@
-- orders steps by edges between objects in the diff, and asks only about constraints and
-- indexes being depended on, which PL/pgSQL bodies never reference.
neededFor :: Bool -> Map ObjectKey Change -> Needed
neededFor forPlan changes
    | Map.null changes = NoEdges
    | forPlan = BodiesOf (Set.fromList [k.address | k <- Map.keys changes])
    | null starts = NoEdges
    | otherwise = ReachableFrom (Set.fromList starts)
  where
    starts = [k.address | (k, c) <- Map.toList changes, k.aspect == Definition, removesOrChanges c]
    removesOrChanges = \case Added _ -> False; _ -> True


-- | Edges from @pg_depend@ and, when plpgsql_check is installed, from PL/pgSQL bodies,
-- with notes for stderr. @label@ names the database in the notes.
edgesOf :: Text -> Scope -> Needed -> Connected -> IO ([Edge], [Text])
edgesOf label scope needed connected@(Connected conn extra _ addresses made) = timed ("edgesOf " <> label) case needed of
    ReachableFrom starts -> let Connected _ _ schema _ _ = connected in reachableEdges label scope starts connected (routineWords schema)
    _ -> do
        let wantedIds wanted = [ids | (ids, addr) <- Map.toList addresses, addr `Set.member` wanted]
            only = case needed of
                BodiesOf wanted -> Just (Set.fromList [oid | (classid, oid, _) <- wantedIds wanted, classid == pgProc])
                _ -> Just Set.empty
        (edges, body) <-
            concurrently
                ( timed ("edgesOf.dependOn " <> label) case needed of
                    BodiesOf wanted -> dependenciesOn conn addresses (wantedIds wanted)
                    _ -> pure []
                )
                (timed ("edgesOf.bodyUses " <> label) $ fmap (first (edgesBetween addresses)) <$> bodyUses (fromMaybe (conn :| []) (nonEmpty extra)) scope.excluded only made)
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


-- | The edges from @pg_depend@ reachable from @starts@, and the body edges of the
-- PL/pgSQL functions that 'bodiesToAnalyse' picks. A body edge can bring a function into
-- the closure, whose dependents are then read, and those can make other functions
-- matter, so it repeats until nothing new is reached or picked.
reachableEdges :: Text -> Scope -> Set Address -> Connected -> IO (Map Address BodyText) -> IO ([Edge], [Text])
reachableEdges label scope starts (Connected conn extra schema addresses made) splitWords = timed ("reachableEdges " <> label) do
    let idsOf = Map.fromListWith (<>) [(addr, [ids]) | (ids, addr) <- Map.toList addresses]
        idsFor = concatMap (\a -> Map.findWithDefault [] a idsOf) . Set.toList
        -- depend_from.sql reads the dependents of a relation's columns with the relation's.
        columnsOf = Map.fromListWith (<>) [(rel, [addr]) | ((classid, objid, sub), addr) <- Map.toList addresses, classid == pgClass, sub > 0, Just rel <- [Map.lookup (pgClass, objid, 0) addresses]]
        withColumns xs = xs <> Set.fromList (concatMap (\a -> Map.findWithDefault [] a columnsOf) (Set.toList xs))
    -- The definitions are split into words while pg_depend is read, in parts on every core.
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
                -- @explored@ holds the objects whose dependents are in @pgEdges@.
                go explored pgEdges done bodyEdges failed = do
                    let dependents = dependentsBy (pgEdges <> bodyEdges)
                        frontier = (starts <> reachedFrom dependents starts) `Set.difference` explored
                    more <- timed "reach.dependFrom" $ dependenciesFrom conn addresses (idsFor frontier)
                    let pgEdges' = pgEdges <> more
                        explored' = explored <> withColumns (frontier <> Set.fromList (map (.dependent) more))
                        dependents' = dependentsBy (pgEdges' <> bodyEdges)
                        -- No name leads to a cast's or operator's function, so reaching
                        -- one means analysing every body.
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
dependentsScope = Scope{excluded = ["hdb_catalog"], includePartitions = False, checkFunctionBodies = False}


-- | For each dropped or changed definition in @changes@, the objects in the database
-- @described@ gives that depend on it, directly or through others, by @pg_depend@ and
-- PL/pgSQL bodies; and for each dropped or changed column and dropped relation, the other
-- functions whose definition names it, with its table for a column, or names a view that
-- reads it, with the view's schema. A view that keeps the column's name must be named with it.
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


-- | For each column whose type changes, the statements that drop the views reading it, the
-- policies and triggers on it, and the routines PostgreSQL ties to the column or to those
-- views, dependents first, and the ones that create them again with their grants, indexes,
-- triggers and comments.
viewsAroundTypeChanges :: Dependents -> [(QName, Text, Address)] -> Map (QName, Text) (Set Address, ([Text], [Text]))
viewsAroundTypeChanges deps retyped =
    Map.fromList
        [ ((t, c), (members, (map dropView (reverse order), concatMap createView order)))
        | (t, c, addr) <- retyped
        , let closure = Map.findWithDefault Set.empty addr deps.exact
              -- PostgreSQL refuses the type change while a policy or a trigger's when clause
              -- or column list uses the column.
              onColumn = Set.filter (\a -> a.objectType `elem` ["policy", "trigger"] && any (\e -> e.dependent == a && e.referenced == addr) deps.dbEdges) closure
              members = held addr closure (Set.filter isView closure <> onColumn)
              order = topoOrder members [(d, r) | e <- deps.dbEdges, let d = relation e.dependent, let r = relation e.referenced, d `Set.member` members, r `Set.member` members]
        , not (Set.null members)
        ]
  where
    isView a = a.objectType `elem` ["view", "materialized view"]
    isRoutine a = a.objectType `elem` ["function", "procedure"]
    -- A SQL-standard body records what it reads in pg_depend; any other routine is held
    -- only by a view's row type in its signature.
    held column closure members =
        let views = Set.filter isView members
            holds r = case Map.lookup (ObjectKey Definition r) deps.dbSchema of
                Just def
                    | standardBody def -> any (\e -> e.dependent == r && (e.referenced == column || relation e.referenced `Set.member` members)) deps.dbEdges
                    | otherwise -> any (\v -> qualifiedAddress v `T.isInfixOf` signature def) views
                Nothing -> False
            more = Set.filter (\r -> isRoutine r && r `Set.notMember` members && holds r) closure
         in if Set.null more then members else held column closure (members <> more)
    signature = T.unlines . takeWhile (\l -> not (" AS " `T.isPrefixOf` l || "BEGIN ATOMIC" `T.isInfixOf` l)) . T.lines
    relation a = case (a.objectType, a.objectNames) of
        (kind, [s, t, _]) | Just rel <- T.stripSuffix " column" kind -> Address rel [s, t] []
        _ -> a
    ctx0 = context Map.empty Map.empty Map.empty [] Map.empty
    dropView a
        | isRoutine a = let kind = a.objectType; args = T.intercalate ", " a.objectArgs in [i|drop #{kind} #{qualifiedAddress a}(#{args})|]
        | [s, t, n] <- a.objectNames, a.objectType `elem` ["policy", "trigger"] = "drop " <> a.objectType <> " " <> quoteIdent n <> " on " <> qualified (QName s t)
        | otherwise = "drop " <> a.objectType <> " " <> qualifiedAddress a
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


-- | A described database with its open connections, and the statements that make
-- plpgsql_check's temp tables there, worked out on a connection of their own while it is
-- described; 'Nothing' when that fails.
data Connected = Connected Connection.Connection [Connection.Connection] Schema Addresses (IO (Maybe Text))


-- | @local@ names the dev stack's database, on @KRONOR_PG_PORT@ (5432 when unset).
resolveDb :: Text -> IO Text
resolveDb = \case
    "local" -> do
        port <- fromMaybe "5432" <$> lookupEnv "KRONOR_PG_PORT"
        pure [i|host=localhost port=#{port} user=kronor dbname=kronor|]
    db -> pure db


-- | A database's schema over 'describeConnections' connections.
describeDatabase :: Scope -> Text -> IO Connected
describeDatabase scope name = do
    db <- resolveDb name
    tempTables <- async (either (const Nothing) Just <$> tryAny (bracket (connect db) Connection.release tempTableStatements))
    (conn, extra) <- concurrently (connect db) (replicateConcurrently (describeConnections - 1) (connect db))
    (schema, addresses) <- describeOn (conn : extra) scope.excluded scope.includePartitions Nothing
    pure (Connected conn extra schema addresses (wait tempTables))


-- | Reads both databases at the same time, then the edges each command needs, given what
-- differs, alongside the body checks; the edges of @--to@ only when @forPlan@.
readBoth :: Scope -> Source -> Text -> Bool -> IO ((Schema, [Edge]), (Schema, [Edge]), Map ObjectKey BodyCheck)
readBoth scope from toDb forPlan = do
    -- @--to@ is described while @--from@'s connection string is found, which can mean
    -- checking or building the baseline database; that comes first if both fail.
    toDescribed <- async (timed "describe --to" $ describeDatabase scope toDb)
    fromDb <- timed "connInfoOf" $ connInfoOf from
    (fromDatabase@(Connected _ _ fromSchema _ _), toDatabase@(Connected toConn _ toSchema _ _)) <-
        timed "readBoth.describe" $ concurrently (timed "describe --from" $ describeDatabase scope fromDb) (wait toDescribed)
    let plainChanges = diff (\_ a b -> a == b) fromSchema toSchema
        needed = neededFor forPlan plainChanges
    measure "readBoth.plainDiff" (Map.size plainChanges)
    (((fromEdges, fromNotes), (toEdges, toNotes)), checks) <-
        timed "readBoth.edgesAndChecks"
            $ concurrently
                ( concurrently
                    (edgesOf "--from" scope needed fromDatabase)
                    (if forPlan then edgesOf "--to" scope needed toDatabase else pure ([], []))
                )
                (bodyChecks scope toConn toSchema plainChanges)
    for_ (fromNotes <> toNotes) (TIO.hPutStrLn stderr)
    pure ((fromSchema, fromEdges), (toSchema, toEdges), checks)


-- | Body checks for the definitions that are added or changed: no other check is consulted.
bodyChecks :: Scope -> Connection.Connection -> Schema -> Map ObjectKey Change -> IO (Map ObjectKey BodyCheck)
bodyChecks scope conn toSchema plainChanges
    | scope.checkFunctionBodies = timed "bodyChecks" $ checkBodies conn (Map.restrictKeys toSchema addedOrChanged)
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
    = Describe Text (Maybe FilePath) Bool Scope
    | Diff Source Text Scope
    | Plan Source Text Scope
    | Sqitch Source Text Scope SqitchOptions
    | TableFile FilePath Text TableOptions


data TableOptions = TableOptions
    { write :: Bool
    , againstDatabase :: Bool
    , allowDrops :: Bool
    , base :: Text
    , project :: FilePath
    , top :: FilePath
    , planPath :: FilePath
    , tableTag :: Maybe Text
    , renames :: [(Text, Text)]
    , dropAndAdd :: Bool
    }


data SqitchOptions = SqitchOptions
    { projectDir :: FilePath
    , topDir :: FilePath
    , planFile :: FilePath
    , tag :: Maybe Text
    , dryRun :: Bool
    }


main :: IO ()
main =
    handle (\(UserError message) -> TIO.hPutStrLn stderr ("schema-diff: " <> message) >> exitWith (ExitFailure 2))
        $ O.execParser (O.info (commandParser O.<**> O.helper) O.fullDesc)
        >>= \case
            Describe name out replace scope -> do
                db <- resolveDb name
                conns <- timed "describe.connect" $ replicateConcurrently describeConnections (connect db)
                (schema, _) <- timed "describe.describeOn" $ describeOn conns scope.excluded scope.includePartitions Nothing
                case out of
                    Just dir -> writeFolder replace dir schema
                    Nothing ->
                        for_ (Map.toList (Map.fromListWith (+) [(k.address.objectType, 1 :: Int) | k <- Map.keys schema])) \(kind, n) ->
                            TIO.putStrLn [i|#{n}\t#{kind}|]
            Diff from toDb scope -> do
                ((fromSchema, edges), (toSchema, _), checks) <- timed "readBoth" $ readBoth scope from toDb False
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
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- timed "readBoth" $ readBoth scope from toDb True
                let steps = plan checks fromSchema toSchema fromEdges toEdges (diff (sameDefinition checks) fromSchema toSchema)
                measure "plan.planRender" (T.length (T.unlines (map renderStep steps)))
                for_ steps (TIO.putStrLn . renderStep)
            Sqitch from toDb scope opts -> do
                unless opts.dryRun requireSqitch
                sqitchPlan <- readPlan (opts.projectDir </> opts.planFile)
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- timed "readBoth" $ readBoth scope from toDb True
                now <- getCurrentTime
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    steps = plan checks fromSchema toSchema fromEdges toEdges changes
                    inverseCtx = context Map.empty toSchema fromSchema toEdges (Map.map invert changes)
                    written = units True inverseCtx sqitchPlan now steps
                    tagName = fromMaybe (T.pack (formatTime defaultTimeLocale "%Y-%m-%d" now) <> "_schema_diff") opts.tag
                measure "sqitch.planUnits" (sum [length u.deploy + length u.revert | u <- written])
                if opts.dryRun
                    then for_ written \u -> do
                        let kind = case u.kind of AddChange -> "add" :: Text; ReworkChange -> "rework"
                            count = length u.deploy
                            changeName = u.changeName
                            note = u.note
                        TIO.putStrLn [i|#{kind}\t#{changeName}\t#{count} statements\t#{note}|]
                    else writeUnits opts.projectDir opts.topDir sqitchPlan tagName written
            TableFile file db opts -> do
                when opts.write requireSqitch
                root <- makeAbsolute opts.project
                absFile <- makeAbsolute file
                let deployDir = root </> opts.top </> "deploy"
                    relative = T.pack (makeRelative deployDir absFile)
                changeName <- maybe (failWith (T.pack file <> " is not under " <> T.pack deployDir)) pure (T.stripSuffix ".sql" relative)
                table <- case T.split (== '/') changeName of
                    [s, "table", t] | not ("@" `T.isInfixOf` t) -> pure (QName s t)
                    _ -> failWith (T.pack file <> " is not a table's own deploy file (<schema>/table/<name>.sql)")
                let parse label = either (\e -> failWith (T.pack file <> label <> ": " <> e)) pure . (withoutTransactionControl <=< splitStatements)
                statements <- parse "" =<< readFileUtf8 absFile
                -- Other connections describe the database while the table is compared, which
                -- happens in a transaction that is rolled back; waited for only when needed.
                let describeWithWords = describeDatabase dependentsScope db >>= \c@(Connected _ _ schema _ _) -> (c,) <$> routineWords schema
                early <- if opts.againstDatabase || not (null opts.renames) then Just <$> async describeWithWords else pure Nothing
                original <- timed "table.jjShow" $ jjShow root opts.base (makeRelative root absFile)
                -- A file unchanged since --base has nothing to describe the database for.
                described <- case early of
                    Just a -> pure (wait a)
                    Nothing
                        | Right statements == (withoutTransactionControl <=< splitStatements) original -> pure describeWithWords
                        | otherwise -> wait <$> async describeWithWords
                when (opts.againstDatabase && not (null opts.renames)) $ failWith "--rename works with the file comparison, not --against-database"
                let renameSql (old, new) = [i|alter table #{qualified table} rename column #{quoteIdent old} to #{quoteIdent new}|]
                baseline <-
                    if opts.againstDatabase
                        then pure (LiveTable opts.allowDrops)
                        else BaseFile . (<> map renameSql opts.renames) <$> parse (" at " <> opts.base) original
                conn <- timed "table.connect" $ connect =<< resolveDb db
                edit <- timed "table.compare1" $ compareTable conn baseline table statements
                let columnAddress c = Address "table column" [table.schema, table.name, c] []
                    renamedFrom = Set.fromList [columnAddress old | (old, _) <- opts.renames]
                -- A renamed column counts as dropped for finding what names it; nothing else sees that.
                deps <- timed "table.dependentsIn" $ dependentsIn described (edit.changes <> Map.fromSet (const (Dropped "")) (Set.map (ObjectKey Definition) renamedFrom))
                -- The PL/pgSQL functions and SQL functions with a string body that depend on the
                -- edit or name it, checked before the edit and after it; an error that only the
                -- edited table brings means the function breaks.
                let definitionOf a = Map.lookup (ObjectKey Definition a) deps.dbSchema
                    routines = Set.toList (Set.filter (\a -> a.objectType `elem` ["function", "procedure"]) (fold deps.named <> fold deps.exact))
                    plpgsql = [a | a <- routines, Just def <- [definitionOf a], "LANGUAGE plpgsql" `T.isInfixOf` def]
                    sqlBodies = [a | a <- routines, Just def <- [definitionOf a], "LANGUAGE sql" `T.isInfixOf` def, not (standardBody def)]
                    signature a = case a.objectNames of
                        [s, n] -> let args = T.intercalate ", " a.objectArgs in [i|#{quoteIdent s}.#{quoteIdent n}(#{args})|]
                        _ -> render a
                    bySignature = Map.fromList [(signature a, a) | a <- plpgsql <> sqlBodies]
                    check made = timed [i|table.bodyErrors n=#{Map.size bySignature}|] $ bodyErrors conn aside (qualified table) made (map signature plpgsql) (map signature sqlBodies)
                    -- The rebuilt table has a new row type, so a call that passes its rows finds no function.
                    rowTypeCall message = "function " `T.isPrefixOf` message && any (\end -> (qualified table <> end) `T.isInfixOf` message) [")", ","]
                checked <-
                    if null plpgsql && null sqlBodies
                        then pure Nothing
                        else
                            timed "table.compare2"
                                $ fmap (Set.filter (\(_, _, message) -> not (rowTypeCall message)))
                                <$> withEditedTable conn baseline table statements (check deps.dbTempTables) (\before -> (\after -> Set.difference <$> after <*> before) <$> check (pure Nothing))
                let retyped =
                        [ (t, c, k.address, n.colType)
                        | (k, Changed old new) <- Map.toList edit.changes
                        , k.aspect == Definition
                        , let o = parseColumn old
                              n = parseColumn new
                        , o.colType /= n.colType || o.collation /= n.collation
                        , Column t c <- [fromAddress k.address]
                        ]
                    typeIn s t c = case [ty | (QName s' t', c', _, ty) <- retyped, (s', t', c') == (s, t, c)] of
                        ty : _ -> Just ty
                        [] -> (.colType) . parseColumn <$> Map.lookup (ObjectKey Definition (Address "table column" [s, t, c] [])) deps.dbSchema
                -- A foreign key on a retyped column has to be rebuilt with the new type.
                foreignKeyBreaks <-
                    timed "table.foreignKeys" $ Map.fromListWith (<>) . catMaybes <$> for
                        [ (addr, k, types)
                        | (_, _, addr, _) <- retyped
                        , k <- Set.toList (Map.findWithDefault Set.empty addr deps.exact)
                        , k.objectType == "table constraint"
                        , Just def <- [Map.lookup (ObjectKey Definition k) deps.dbSchema]
                        , Just types <- [foreignKeyTypes typeIn k def]
                        ]
                        \(addr, k, types) -> fmap (\err -> (addr, [(k, err)])) <$> foreignKeyError conn types
                -- Whether each type change rewrites the table, as PostgreSQL decides it.
                typeFacts <-
                    timed "table.typeChangeFacts" $ Map.fromList . catMaybes <$> for retyped \(_, _, addr, newType) ->
                        case parseColumn <$> Map.lookup (ObjectKey Definition addr) edit.current of
                            Just o -> fmap (addr,) <$> typeChangeFacts conn o.colType newType
                            Nothing -> pure Nothing
                let otherTables addr =
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
                    withTypeFacts s = case Map.lookup s.address typeFacts of
                        Just (rewrites, domain)
                            | MayRewriteTable `elem` s.hazards ->
                                let kept = concatMap (\h -> if h == MayRewriteTable then [RewritesTable | rewrites] else [h | h /= convertNote || rewrites || domain]) s.hazards
                                 in s{hazards = kept <> concat [LocksOther o : [RechecksForeignKey o | rewrites] | o <- otherTables s.address]}
                        _ -> s
                let breaking = Map.fromListWith (\_ earlier -> earlier) [(a, message) | (s, _, message) <- maybe [] Set.toList checked, Just a <- [Map.lookup s bySignature]]
                    passed = Set.fromList [a | isJust checked, a <- plpgsql <> sqlBodies, a `Map.notMember` breaking, Just def <- [definitionOf a], not ("execute" `T.isInfixOf` T.toLower def)]
                    breaksFor addr = [a | a <- Set.toList (Map.findWithDefault Set.empty addr deps.named <> Map.findWithDefault Set.empty addr deps.exact), a `Map.member` breaking]
                    dependents = deps.exact
                    mentions = Map.map (Set.filter (\a -> a `Map.notMember` breaking && a `Set.notMember` passed)) deps.named
                    retypedAddresses = Set.fromList [a | (_, _, a, _) <- retyped]
                    around = viewsAroundTypeChanges deps [(t, c, a) | (t, c, a, _) <- retyped]
                    withViews c = c{viewsAround = Map.map snd around}
                    aroundOf addr = fold [members | (t, c, a, _) <- retyped, a == addr, Just (members, _) <- [Map.lookup (t, c) around]]
                    foreignKeysFor addr = Map.findWithDefault [] addr foreignKeyBreaks
                    renameSteps =
                        [ Step DropPhase (-1) addr (Column table new) (renameSql (old, new)) Shared [] (ObjectKey Definition addr) (Changed def def)
                        | (old, new) <- opts.renames
                        , let addr = columnAddress new
                              def = Map.findWithDefault "" (ObjectKey Definition addr) edit.desired
                        ]
                    steps = renameSteps <> map withTypeFacts (planWith withViews Map.empty edit.current edit.desired [] [] edit.changes)
                    same = if opts.againstDatabase then "the file matches the table" else "the file has no schema changes since " <> opts.base
                    columnsWhere p = [(c, (parseColumn (changeText ch)).colType) | (k, ch) <- Map.toList edit.changes, k.aspect == Definition, p ch, Column _ c <- [fromAddress k.address]]
                    changeText = \case Added d -> d; Dropped d -> d; Changed _ d -> d
                    renameLike =
                        [ (old, new)
                        | null opts.renames
                        , not opts.dropAndAdd
                        , (old, oldType) <- columnsWhere (\case Dropped _ -> True; _ -> False)
                        , (new, newType) <- columnsWhere (\case Added _ -> True; _ -> False)
                        , oldType == newType
                        ]
                    renameNotes = [[i|#{old} is dropped and #{new} added with the same type; pass --rename #{old}:#{new} if it is a rename, or --drop-and-add if it is not|] | (old, new) <- renameLike]
                    dropped addr = case Map.lookup (ObjectKey Definition addr) edit.changes of
                        Just (Dropped _) -> True
                        _ -> False
                    -- DROP COLUMN also drops the table's indexes and constraints on the column.
                    goesWith d = d.objectType == "index" || (d.objectType == "table constraint" && take 2 d.objectNames == [table.schema, table.name])
                    listed = fold dependents <> fold mentions
                    overloaded = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [((a.objectType, a.objectNames), 1) | a <- Set.toList listed]))
                    short a =
                        (if a.objectType `elem` ["function", "procedure"] then "" else a.objectType <> " ")
                            <> T.intercalate "." a.objectNames
                            <> if (a.objectType, a.objectNames) `Set.member` overloaded then let args = T.intercalate ", " a.objectArgs in [i|(#{args})|] else ""
                    section heading = \case
                        [] -> []
                        ds -> ("  " <> heading) : map (("    " <>) . short) ds
                    breaksSection = \case
                        [] -> []
                        fs -> "  breaks" : concat [["    " <> short f, "      " <> Map.findWithDefault "" f breaking] | f <- fs]
                    foreignKeySection = \case
                        [] -> []
                        ks -> "  cannot be rebuilt with the new type" : concat [["    " <> short k, "      " <> err] | (k, err) <- ks]
                    isView d = d.objectType `elem` ["view", "materialized view"]
                    recreated addr d = d `Set.member` aroundOf addr || (d.objectType == "trigger" && any (\v -> isView v && v.objectNames == take 2 d.objectNames) (aroundOf addr))
                    -- ALTER COLUMN TYPE rebuilds the column's indexes and constraints itself.
                    rebuiltWith d = d.objectType `elem` ["index", "table constraint", "sequence"]
                    usage =
                        concat
                            [ T.intercalate "." addr.objectNames
                                : section (if addr `Set.member` renamedFrom then "used by, and follows the rename" else "used by") users
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
                            <> [k | ks <- Map.elems foreignKeyBreaks, (k, _) <- ks]
                measure "table.planRender" (T.length (T.unlines (map renderStep steps <> usage)) + length blocking)
                -- An object left alone takes its comment with it.
                let leftAlone = Set.fromList [k.address | (k, _) <- edit.ignored, k.aspect == Definition]
                for_ [(k, reason) | (k, reason) <- edit.ignored, not (k.aspect == Comment && k.address `Set.member` leftAlone)] \(k, reason) -> do
                    let name = render k.address <> if k.aspect == Privileges then " (privileges)" else ""
                    TIO.putStrLn [i|-- left alone: #{name}: #{reason}|]
                if not opts.write
                    then do
                        for_ usage \line -> TIO.putStrLn ("-- " <> line)
                        unless (null usage) $ TIO.putStrLn ""
                        if null steps
                            then TIO.putStrLn ("-- " <> same)
                            else for_ steps (TIO.putStrLn . renderStep)
                    else do
                        when (null steps) $ failWith ("nothing to write: " <> same)
                        for_ usage (TIO.hPutStrLn stderr)
                        unless (null blocking) do
                            let n = length blocking
                            failWith [i|#{n} dependencies on what the edit drops or retypes; change or drop the dependents first|]
                        unless (null renameLike) $ failWith "a column is dropped and another added with the same type; say whether it is a rename with --rename or --drop-and-add"
                        sqitchPlan <- readPlan (root </> opts.planPath)
                        basePlan <- parsePlan <$> jjShow root opts.base opts.planPath
                        now <- getCurrentTime
                        let inverseCtx = withViews (context Map.empty edit.desired edit.current [] (Map.map invert edit.changes))
                            renameBack =
                                [ Step DropPhase (-1) (columnAddress old) (Column table old) (renameSql (new, old)) Shared [] s.key s.change
                                | (s, (old, new)) <- zip renameSteps opts.renames
                                ]
                            withRenameBack u
                                | any (`elem` renameSteps) u.deploy = u{revert = u.revert <> renameBack}
                                | otherwise = u
                            written = map withRenameBack (units False inverseCtx sqitchPlan now steps)
                            tagName = fromMaybe (T.pack (formatTime defaultTimeLocale "%Y-%m-%d" now) <> "_schema_diff") opts.tableTag
                            entries p = length (filter (== changeName) p.changes)
                            alreadyReworked = entries sqitchPlan > entries basePlan
                        -- sqitch reverts the reworked change before the migrations, so its
                        -- revert, the previous file, meets the edited table.
                        previous <- parse (" at " <> opts.base) original
                        oldFileFails <- timed "table.compare3" $ withEditedTable conn baseline table statements (pure ()) (const (firstFailure conn previous))
                        let baseRev = opts.base
                        for_ oldFileFails \err ->
                            TIO.putStrLn [i|the file at #{baseRev} fails on the edited table (#{err}); the rework's revert does nothing and the migrations revert the table|]
                        writeUnits root opts.top sqitchPlan tagName written
                        unless alreadyReworked
                            $ reworkEdited root opts.top sqitchPlan tagName changeName [i|schema-diff: #{changeName} as edited|] original (isJust oldFileFails)
                        for_ written \u -> TIO.putStrLn ("added " <> u.changeName)
                        TIO.putStrLn (if alreadyReworked then "already reworked on this branch: " <> changeName else "reworked " <> changeName)


-- | A file as it is at a jj revision.
jjShow :: FilePath -> Text -> FilePath -> IO Text
jjShow root rev path = do
    (code, out, err) <- readProcess (setWorkingDir root (proc "jj" ["--ignore-working-copy", "file", "show", "-r", T.unpack rev, path]))
    case code of
        ExitSuccess -> pure (decodeUtf8Lenient (toStrictBytes out))
        ExitFailure _ -> failWith ("jj file show -r " <> rev <> " " <> T.pack path <> ": " <> decodeUtf8Lenient (toStrictBytes err))


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
                    <$> O.switch (O.long "write" <> O.help "Add the migration with sqitch and rework the table's change")
                    <*> O.switch (O.long "against-database" <> O.help "Compare with the live table instead of the file at --base; shows drift too")
                    <*> O.switch (O.long "allow-drops" <> O.help "With --against-database, drop columns and constraints that are not in the file")
                    <*> O.strOption (O.long "base" <> O.metavar "REV" <> O.value "master" <> O.help "Revision with the file before your edit (jj)")
                    <*> O.strOption (O.long "project" <> O.metavar "DIR" <> O.value ".")
                    <*> O.strOption (O.long "top-dir" <> O.metavar "DIR" <> O.value "database")
                    <*> O.strOption (O.long "plan-file" <> O.metavar "FILE" <> O.value "database/sqitch.plan")
                    <*> O.optional (O.strOption (O.long "tag" <> O.metavar "NAME" <> O.help "Tag to add when the rework needs one"))
                    <*> O.many (O.option (O.eitherReader renameArg) (O.long "rename" <> O.metavar "OLD:NEW" <> O.help "Column OLD is renamed to NEW; repeat for more columns"))
                    <*> O.switch (O.long "drop-and-add" <> O.help "A column dropped and one added with the same type are not a rename")
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
            <*> O.switch (O.long "replace" <> O.help "Clear --out first, if schema-diff wrote it")
            <*> scopeParser
    diffParser = twoDatabases Diff
    scopeParser =
        Scope
            <$> fmap
                ("hdb_catalog" :)
                (O.many (O.strOption (O.long "exclude-schema" <> O.metavar "SCHEMA" <> O.help "Leave this schema out; Hasura's hdb_catalog always is")))
            <*> O.switch (O.long "include-partitions" <> O.help "Also compare partitions and their columns")
            <*> fmap
                not
                ( O.switch
                    ( O.long "no-body-check"
                        <> O.help "Skip recreating SQL-standard function bodies in rolled-back transactions on --to"
                    )
                )


report :: ObjectKey -> Change -> Set Address -> IO ()
report key change dependents = do
    let name = render key.address
    TIO.putStrLn [i|#{verb} #{name}#{suffix}|]
    case change of
        Changed old new ->
            for_ (getDiff (T.lines old) (T.lines new)) \case
                First line -> TIO.putStrLn ("  - " <> line)
                Second line -> TIO.putStrLn ("  + " <> line)
                Both _ _ -> pure ()
        _ -> pure ()
    unless (Set.null dependents) do
        TIO.putStrLn "  depended on by:"
        for_ (Set.toList dependents) \addr -> TIO.putStrLn ("    " <> render addr)
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


summary :: Map ObjectKey Change -> Text
summary changes = [i|#{count isAdded} added, #{count isDropped} dropped, #{count isChanged} changed|]
  where
    count p = L.length (filter p (Map.elems changes))
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False
    isChanged = \case Changed _ _ -> True; _ -> False
