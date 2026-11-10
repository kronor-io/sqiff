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
import Kronor.SchemaDiff.Catalog (Address (..), Addresses, Aspect (..), Edge (..), ObjectKey (..), PlFunction (..), PlFunctions (..), Schema, Skipped, UserError (..), analyseBodies, bodyUses, connect, dependenciesFrom, dependenciesOn, describeOn, edgesBetween, failWith, plpgsqlFunctions, render)
import Kronor.SchemaDiff.Diff (BodyText (..), Change (..), affected, bodiesToAnalyse, bodyText, dependentsBy, diff, reachedFrom)
import Kronor.SchemaDiff.Folder (writeFolder)
import Kronor.SchemaDiff.Object (QName (..))
import Kronor.SchemaDiff.Plan (context, invert, plan, renderStep)
import Kronor.SchemaDiff.Script (splitStatements, withoutTransactionControl)
import Kronor.SchemaDiff.Sqitch (SqitchPlan (..), Unit (..), UnitKind (..), parsePlan, readPlan, requireSqitch, reworkEdited, units, writeUnits)
import Kronor.SchemaDiff.TableEdit (Baseline (..), TableEdit (..), compareTable)
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
edgesOf label scope needed connected@(Connected conn extra _ addresses) = case needed of
    ReachableFrom starts -> reachableEdges label scope starts connected
    _ -> do
        let wantedIds wanted = [ids | (ids, addr) <- Map.toList addresses, addr `Set.member` wanted]
            only = case needed of
                BodiesOf wanted -> Just (Set.fromList [oid | (classid, oid, _) <- wantedIds wanted, classid == pgProc])
                _ -> Just Set.empty
        (edges, body) <-
            concurrently
                ( case needed of
                    BodiesOf wanted -> dependenciesOn conn addresses (wantedIds wanted)
                    _ -> pure []
                )
                (fmap (first (edgesBetween addresses)) <$> bodyUses (fromMaybe (conn :| []) (nonEmpty extra)) scope.excluded only)
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
reachableEdges :: Text -> Scope -> Set Address -> Connected -> IO ([Edge], [Text])
reachableEdges label scope starts (Connected conn extra schema addresses) = do
    let idsOf = Map.fromListWith (<>) [(addr, [ids]) | (ids, addr) <- Map.toList addresses]
        idsFor = concatMap (\a -> Map.findWithDefault [] a idsOf) . Set.toList
        -- depend_from.sql reads the dependents of a relation's columns with the relation's.
        columnsOf = Map.fromListWith (<>) [(rel, [addr]) | ((classid, objid, sub), addr) <- Map.toList addresses, classid == pgClass, sub > 0, Just rel <- [Map.lookup (pgClass, objid, 0) addresses]]
        withColumns xs = xs <> Set.fromList (concatMap (\a -> Map.findWithDefault [] a columnsOf) (Set.toList xs))
    -- The definitions are split into words while pg_depend is read, in parts on every core.
    (edges, functions) <-
        concurrently
            (dependenciesFrom conn addresses (idsFor starts))
            ( plpgsqlFunctions (fromMaybe conn (listToMaybe extra)) scope.excluded
                >>= traverse \fs -> do
                    let split part = do
                            let described =
                                    [ (f, addr, bodyText (Map.findWithDefault "" (ObjectKey Definition addr) schema), trigger)
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
                    more <- dependenciesFrom conn addresses (idsFor frontier)
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
                            (uses, failed') <- analyseBodies (conn :| extra) fs todo
                            go explored' pgEdges' (done <> Set.fromList (map (.oid) todo)) (bodyEdges <> edgesBetween addresses uses) (failed <> failed')
            (allEdges, failed) <- go (withColumns (starts <> Set.fromList (map (.dependent) edges))) edges Set.empty [] []
            pure (allEdges, bodyNotes label (Just (fs.unchecked <> L.sortOn (\(name, _) -> Map.lookup name position) failed)))


-- | For each dropped or changed definition in @changes@, the objects in the database @db@
-- that depend on it, directly or through others, by @pg_depend@ and PL/pgSQL bodies.
dependentsIn :: Text -> Map ObjectKey Change -> IO (Map Address (Set Address))
dependentsIn db changes
    | Set.null starts = pure Map.empty
    | otherwise = do
        connected@(Connected _ _ schema _) <- describeDatabase scope db
        (edges, notes) <- reachableEdges "--db" scope starts connected
        for_ notes (TIO.hPutStrLn stderr)
        let known = Set.fromList [k.address | k <- Map.keys schema, k.aspect == Definition]
        pure (affected known edges changes)
  where
    scope = Scope{excluded = ["hdb_catalog"], includePartitions = False, checkFunctionBodies = False}
    starts = Set.fromList [k.address | (k, c) <- Map.toList changes, k.aspect == Definition, removesOrChanges c]
    removesOrChanges = \case Added _ -> False; _ -> True


-- | Connections per database; the describe queries run over all of them at once.
describeConnections :: Int
describeConnections = 4


-- | A described database with its open connections.
data Connected = Connected Connection.Connection [Connection.Connection] Schema Addresses


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
    (conn, extra) <- concurrently (connect db) (replicateConcurrently (describeConnections - 1) (connect db))
    (schema, addresses) <- describeOn (conn : extra) scope.excluded scope.includePartitions Nothing
    pure (Connected conn extra schema addresses)


-- | Reads both databases at the same time, then the edges each command needs, given what
-- differs, alongside the body checks; the edges of @--to@ only when @forPlan@.
readBoth :: Scope -> Text -> Text -> Bool -> IO ((Schema, [Edge]), (Schema, [Edge]), Map ObjectKey BodyCheck)
readBoth scope fromDb toDb forPlan = do
    (fromDatabase@(Connected _ _ fromSchema _), toDatabase@(Connected toConn _ toSchema _)) <-
        concurrently (describeDatabase scope fromDb) (describeDatabase scope toDb)
    let plainChanges = diff (\_ a b -> a == b) fromSchema toSchema
        needed = neededFor forPlan plainChanges
    (((fromEdges, fromNotes), (toEdges, toNotes)), checks) <-
        concurrently
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
    | scope.checkFunctionBodies = checkBodies conn (Map.restrictKeys toSchema addedOrChanged)
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
                conns <- replicateConcurrently describeConnections (connect db)
                (schema, _) <- describeOn conns scope.excluded scope.includePartitions Nothing
                case out of
                    Just dir -> writeFolder replace dir schema
                    Nothing ->
                        for_ (Map.toList (Map.fromListWith (+) [(k.address.objectType, 1 :: Int) | k <- Map.keys schema])) \(kind, n) ->
                            TIO.putStrLn [i|#{n}\t#{kind}|]
            Diff from toDb scope -> do
                fromDb <- connInfoOf from
                ((fromSchema, edges), (toSchema, _), checks) <- readBoth scope fromDb toDb False
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    known = Set.fromList [k.address | k <- Map.keys fromSchema, k.aspect == Definition]
                    affectedBy = affected known edges changes
                    dependentsOf key = case key.aspect of
                        Definition -> Map.findWithDefault Set.empty key.address affectedBy
                        Privileges -> Set.empty
                for_ (Map.toList changes) \(key, change) -> report key change (dependentsOf key)
                TIO.putStrLn (summary changes)
                unless (Map.null changes) $ exitWith (ExitFailure 1)
            Plan from toDb scope -> do
                fromDb <- connInfoOf from
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- readBoth scope fromDb toDb True
                let steps = plan checks fromSchema toSchema fromEdges toEdges (diff (sameDefinition checks) fromSchema toSchema)
                for_ steps (TIO.putStrLn . renderStep)
            Sqitch from toDb scope opts -> do
                unless opts.dryRun requireSqitch
                sqitchPlan <- readPlan (opts.projectDir </> opts.planFile)
                fromDb <- connInfoOf from
                ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- readBoth scope fromDb toDb True
                now <- getCurrentTime
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    steps = plan checks fromSchema toSchema fromEdges toEdges changes
                    inverseCtx = context Map.empty toSchema fromSchema toEdges (Map.map invert changes)
                    written = units True inverseCtx sqitchPlan now steps
                    tagName = fromMaybe (T.pack (formatTime defaultTimeLocale "%Y-%m-%d" now) <> "_schema_diff") opts.tag
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
                original <- jjShow root opts.base (makeRelative root absFile)
                baseline <-
                    if opts.againstDatabase
                        then pure (LiveTable opts.allowDrops)
                        else BaseFile <$> parse (" at " <> opts.base) original
                conn <- connect =<< resolveDb db
                edit <- compareTable conn baseline table statements
                dependents <- dependentsIn db edit.changes
                let steps = plan Map.empty edit.current edit.desired [] [] edit.changes
                    same = if opts.againstDatabase then "the file matches the table" else "the file has no schema changes since " <> opts.base
                    dropped addr = case Map.lookup (ObjectKey Definition addr) edit.changes of
                        Just (Dropped _) -> True
                        _ -> False
                    -- DROP COLUMN also drops the table's indexes and constraints on the column.
                    goesWith d = d.objectType == "index" || (d.objectType == "table constraint" && take 2 d.objectNames == [table.schema, table.name])
                    uses =
                        [ if dropped addr && goesWith d
                            then (False, [i|#{render d} depends on #{render addr}, and is dropped with it|])
                            else (dropped addr, [i|#{render d} depends on #{render addr}|])
                        | (addr, ds) <- Map.toList dependents
                        , d <- Set.toList ds
                        ]
                    blocking = [line | (True, line) <- uses]
                for_ edit.ignored \(k, reason) -> do
                    let name = render k.address <> if k.aspect == Privileges then " (privileges)" else ""
                    TIO.putStrLn [i|-- left alone: #{name}: #{reason}|]
                if not opts.write
                    then do
                        for_ uses \(_, line) -> TIO.putStrLn ("-- " <> line)
                        unless (null uses) $ TIO.putStrLn ""
                        if null steps
                            then TIO.putStrLn ("-- " <> same)
                            else for_ steps (TIO.putStrLn . renderStep)
                    else do
                        when (null steps) $ failWith ("nothing to write: " <> same)
                        for_ uses \(_, line) -> TIO.hPutStrLn stderr ("schema-diff: " <> line)
                        unless (null blocking) do
                            let n = length blocking
                            failWith [i|#{n} dependencies on what the edit drops; change or drop the dependents first|]
                        sqitchPlan <- readPlan (root </> opts.planPath)
                        basePlan <- parsePlan <$> jjShow root opts.base opts.planPath
                        now <- getCurrentTime
                        let inverseCtx = context Map.empty edit.desired edit.current [] (Map.map invert edit.changes)
                            written = units False inverseCtx sqitchPlan now steps
                            tagName = fromMaybe (T.pack (formatTime defaultTimeLocale "%Y-%m-%d" now) <> "_schema_diff") opts.tableTag
                            entries p = length (filter (== changeName) p.changes)
                            alreadyReworked = entries sqitchPlan > entries basePlan
                        writeUnits root opts.top sqitchPlan tagName written
                        unless alreadyReworked
                            $ reworkEdited root opts.top sqitchPlan tagName changeName [i|schema-diff: #{changeName} as edited|] original
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
                )
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


summary :: Map ObjectKey Change -> Text
summary changes = [i|#{count isAdded} added, #{count isDropped} dropped, #{count isChanged} changed|]
  where
    count p = L.length (filter p (Map.elems changes))
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False
    isChanged = \case Changed _ _ -> True; _ -> False
