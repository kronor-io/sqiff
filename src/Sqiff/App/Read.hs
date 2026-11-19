{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Reads databases: their schemas over a few connections at once, and the dependency
-- edges a command needs. Connections live in scopes, so they are released however the
-- command ends.
module Sqiff.App.Read (
    Connected (..),
    withDatabase,
    describeConnections,
    Purpose (..),
    SchemaSnapshot (..),
    ComparisonInputs (..),
    readBoth,
    reachableEdges,
    routineWords,
    withSource,
) where

import Data.String.Interpolate (i)
import Hasql.Connection qualified as Connection
import RIO
import RIO.HashSet qualified as HS
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.NonEmpty (nonEmpty)
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.App.Env (AppEnv (..), BodyChecks (..), Scope (..), Source (..), note)
import Sqiff.Baseline (withBaseline)
import Sqiff.Body (BodyCheck, checkBodies)
import Sqiff.Catalog (Addresses, CatalogId (..), dependenciesFrom, dependenciesOn, describeOn, edgesBetween, pgClass, pgProc)
import Sqiff.Catalog.PlpgsqlCheck (PlFunction (..), PlFunctions (..), Skipped, analyseBodies, bodyUses, plpgsqlFunctions, tempTableStatements)
import Sqiff.Config (Config (..))
import Sqiff.Connection (withConnection, withConnections, withSharedSnapshot)
import Sqiff.ConnectionString (ConnectionString)
import Sqiff.Definition (printed)
import Sqiff.Dependencies (Dependencies, indexEdges, reachableFrom)
import Sqiff.Diff (BodyCandidate (..), BodyText (..), Change (..), Changes, bodiesToAnalyse, bodyText, diff)
import Sqiff.Model
import Sqiff.Timing (measure, timed)


-- | Connections per database; the describe queries run over all of them at once.
describeConnections :: Int
describeConnections = 4


-- | A described database and its open connections, valid inside 'withDatabase'.
data Connected = Connected
    { conn :: Connection.Connection
    , extra :: [Connection.Connection]
    -- ^ More connections to spread queries over.
    , schema :: Schema
    , addresses :: Addresses
    , tempTableScript :: IO (Maybe Text)
    -- ^ The statements that make plpgsql_check's temp tables there.
    }


-- | Runs @action@ with the database described over 'describeConnections' connections. The
-- connections open and the description starts the first time @action@ asks for it; all
-- are released when @action@ returns or fails.
withDatabase :: Scope -> ConnectionString -> (IO Connected -> IO a) -> IO a
withDatabase scope db = onDemand \use ->
    withConnections db describeConnections \(conn :| extra) ->
        withAsync (withConnection db tempTableStatements) \tempTables -> do
            (schema, addresses) <- withSharedSnapshot (conn :| extra) $ describeOn (conn : extra) scope.excluded scope.partitions Nothing
            use Connected{conn, extra, schema, addresses, tempTableScript = wait tempTables}


-- | Runs @action@ with a way to get what @acquire@ makes. @acquire@ runs, in a thread of its
-- own, the first time @action@ asks, and what it holds is released when @action@ ends.
onDemand :: ((a -> IO ()) -> IO ()) -> (IO a -> IO b) -> IO b
onDemand acquire action = do
    wanted <- newEmptyMVar
    ready <- newEmptyMVar
    done <- newEmptyMVar
    withAsync (readMVar wanted >> acquire \a -> putMVar ready a >> readMVar done) \holder -> do
        let get = do
                void (tryPutMVar wanted ())
                race (wait holder) (readMVar ready) >>= \case
                    Right a -> pure a
                    Left () -> throwString "the resource was released before it was ready"
        action get `finally` tryPutMVar done ()


-- | Which edges a command uses.
data Needed
    = -- | Edges from @pg_depend@, and the body edges 'Sqiff.Diff.affected' can reach from these.
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


-- | How far 'reachableEdges' has got.
data Reach = Reach
    { explored :: Set ObjectId
    -- ^ The objects whose @pg_depend@ edges have been read.
    , catalogEdges :: [Edge]
    , bodyEdges :: [Edge]
    -- ^ The edges plpgsql_check found in the bodies analysed.
    , analysed :: Set Int64
    -- ^ The functions whose bodies have been analysed, by oid.
    , failed :: Skipped
    }


reachedSoFar :: Reach -> Dependencies
reachedSoFar reach = indexEdges (reach.catalogEdges <> reach.bodyEdges)


-- | A database's addresses, indexed both ways 'reachableEdges' looks them up. Built once.
data AddressIndex = AddressIndex
    { addresses :: Addresses
    , idsOf :: Map ObjectId [CatalogId]
    -- ^ The catalog ids of each object.
    , columnsOf :: Map ObjectId [ObjectId]
    -- ^ The columns of each relation.
    }


addressIndex :: Addresses -> AddressIndex
addressIndex addresses =
    AddressIndex
        { addresses
        , idsOf = Map.fromListWith (<>) [(object, [ids]) | (ids, object) <- Map.toList addresses]
        , columnsOf = Map.fromListWith (<>) [(rel, [object]) | (ids, object) <- Map.toList addresses, ids.classId == pgClass, ids.subId > 0, Just rel <- [relationWith ids]]
        }
  where
    relationWith ids = Map.lookup (CatalogId pgClass ids.objectOid 0) addresses


-- | The catalog ids of these objects, to read their @pg_depend@ edges by.
idsFor :: AddressIndex -> Set ObjectId -> [CatalogId]
idsFor index = concatMap (\a -> Map.findWithDefault [] a index.idsOf) . Set.toList


-- | These objects and the columns of those that are relations.
withColumns :: AddressIndex -> Set ObjectId -> Set ObjectId
withColumns index xs = xs <> Set.fromList (concatMap (\a -> Map.findWithDefault [] a index.columnsOf) (Set.toList xs))


-- | What every round of 'discoverReachable' reads from; it does not change between rounds.
data ReachInput = ReachInput
    { conn :: Connection.Connection
    , extra :: [Connection.Connection]
    , index :: AddressIndex
    , tempTableScript :: IO (Maybe Text)
    , starts :: Set ObjectId
    , functions :: PlFunctions
    , candidates :: [BodyCandidate PlFunction]
    , implicit :: [ObjectId]
    -- ^ The functions behind user-defined casts and operators.
    }


-- | The @pg_depend@ edges reachable from @starts@, and the body edges of the PL/pgSQL
-- functions 'bodiesToAnalyse' picks, repeated until nothing new is reached or picked.
reachableEdges :: Text -> Scope -> Set ObjectId -> Connected -> IO (Map ObjectId BodyText) -> IO ([Edge], [Text])
reachableEdges label scope starts Connected{conn, extra, schema, addresses, tempTableScript} splitWords = timed [i|reachableEdges #{label}|] do
    let index = addressIndex addresses
    (edges, functions) <-
        concurrently
            (timed "reach.dependFrom0" $ dependenciesFrom conn addresses (idsFor index starts))
            ( timed "reach.functionsAndWords"
                $ plpgsqlFunctions (fromMaybe conn (listToMaybe extra)) scope.excluded
                >>= traverse \fs -> do
                    split_ <- splitWords
                    candidates <- bodyCandidates schema addresses split_ fs.checkable
                    pure (fs, candidates)
            )
    case functions of
        Nothing -> pure (edges, bodyNotes label Nothing)
        Just (fs, candidates) -> do
            let input =
                    ReachInput
                        { conn
                        , extra
                        , index
                        , tempTableScript
                        , starts
                        , functions = fs
                        , candidates
                        , implicit = [a | o <- fs.implicit, Just a <- [Map.lookup (CatalogId pgProc o 0) addresses]]
                        }
            reached <- discoverReachable input (firstReach index starts edges)
            pure (reached.catalogEdges <> reached.bodyEdges, bodyNotes label (Just (fs.unchecked <> inCheckableOrder fs reached.failed)))


-- | The checkable functions as 'bodiesToAnalyse' sees them. Their words are split over
-- eight threads and forced there.
bodyCandidates :: Schema -> Addresses -> Map ObjectId BodyText -> [PlFunction] -> IO [BodyCandidate PlFunction]
bodyCandidates schema addresses split_ checkable =
    concat <$> pooledMapConcurrently forced (eightParts checkable)
  where
    forced part = do
        let candidates = mapMaybe (bodyCandidate addresses (wordsAt schema split_)) part
        _ <- evaluate (foldl' (\n c -> n + HS.size c.body.wordSet) (0 :: Int) candidates)
        pure candidates


-- | A function with its object, its words, and the table it is a trigger on; 'Nothing'
-- when it has no address.
bodyCandidate :: Addresses -> (ObjectId -> BodyText) -> PlFunction -> Maybe (BodyCandidate PlFunction)
bodyCandidate addresses wordsOfObject f = do
    object <- Map.lookup (CatalogId pgProc f.oid 0) addresses
    pure
        BodyCandidate
            { value = f
            , object
            , body = wordsOfObject object
            , triggerRelation = f.relid >>= \r -> Map.lookup (CatalogId pgClass r 0) addresses
            }


-- | The words of a routine: split already, or split from its definition here.
wordsAt :: Schema -> Map ObjectId BodyText -> ObjectId -> BodyText
wordsAt schema split_ object = fromMaybe (bodyText (maybe "" printed (Map.lookup (definitionKey object) schema))) (Map.lookup object split_)


-- | Where the search starts: the first @pg_depend@ edges read, nothing analysed yet.
firstReach :: AddressIndex -> Set ObjectId -> [Edge] -> Reach
firstReach index starts edges =
    Reach
        { explored = withColumns index (starts <> Set.fromList (map (.dependent) edges))
        , catalogEdges = edges
        , bodyEdges = []
        , analysed = Set.empty
        , failed = []
        }


-- | One round: read the @pg_depend@ edges of what is newly reached, then analyse the
-- bodies that may reach further; repeated until neither adds anything.
discoverReachable :: ReachInput -> Reach -> IO Reach
discoverReachable input reach = do
    let frontier = (input.starts <> reachableFrom (reachedSoFar reach) input.starts) `Set.difference` reach.explored
    more <- timed "reach.dependFrom" $ dependenciesFrom input.conn input.index.addresses (idsFor input.index frontier)
    let read_ = reach{explored = reach.explored <> withColumns input.index (frontier <> Set.fromList (map (.dependent) more)), catalogEdges = reach.catalogEdges <> more}
    case [f | f <- picked input (reachedSoFar read_), f.oid `Set.notMember` read_.analysed] of
        []
            | Set.null frontier -> pure read_
            | otherwise -> discoverReachable input read_
        todo -> analyse input todo read_ >>= discoverReachable input


-- | The functions whose bodies may reach further. All of them once a cast or operator
-- function is reached, since bodies use those without naming them.
picked :: ReachInput -> Dependencies -> [PlFunction]
picked input deps
    | any (`Set.member` (input.starts <> reachableFrom deps input.starts)) input.implicit = [f.value | f <- input.candidates]
    | otherwise = bodiesToAnalyse deps input.starts input.candidates


-- | Analyses the bodies of @todo@ and adds the edges found.
analyse :: ReachInput -> [PlFunction] -> Reach -> IO Reach
analyse input todo reach = do
    let fs = input.functions
    (uses, failed) <- timed [i|reach.analyseBodies n=#{length todo}|] $ analyseBodies (input.conn :| input.extra) fs{tempTables = input.tempTableScript >>= maybe fs.tempTables pure} todo
    pure
        reach
            { analysed = reach.analysed <> Set.fromList (map (.oid) todo)
            , bodyEdges = reach.bodyEdges <> edgesBetween input.index.addresses uses
            , failed = reach.failed <> failed
            }


-- | The functions plpgsql_check could not analyse, in the order it lists the checkable ones.
inCheckableOrder :: PlFunctions -> Skipped -> Skipped
inCheckableOrder fs = L.sortOn (\(name, _) -> Map.lookup name position)
  where
    position = Map.fromList (zip [f.name | f <- fs.checkable] [0 :: Int ..])


-- | The words of every function's and procedure's definition, split on every core.
routineWords :: Schema -> IO (Map ObjectId BodyText)
routineWords schema = do
    let routines = [(k.object, printed def) | (k, def) <- Map.toList schema, k.aspect == Definition, isRoutine k.object]
        split part = do
            let split_ = [(addr, bodyText def) | (addr, def) <- part]
            _ <- evaluate (foldl' (\n (_, b) -> n + HS.size b.wordSet + T.length b.lowered) (0 :: Int) split_)
            pure split_
    Map.fromList . concat <$> pooledMapConcurrently split (eightParts routines)


-- | The list dealt into eight parts, in turn: work for eight threads.
eightParts :: [a] -> [[a]]
eightParts xs = [[x | (n, x) <- zip [0 :: Int ..] xs, n `mod` 8 == k] | k <- [0 .. 7]]


-- | A database's schema and the dependency edges read from it.
data SchemaSnapshot = SchemaSnapshot
    { schema :: Schema
    , dependencies :: Dependencies
    }


-- | What 'readBoth' reads: @--from@, @--to@, and the body checks of @--to@.
data ComparisonInputs = ComparisonInputs
    { source :: SchemaSnapshot
    , target :: SchemaSnapshot
    , bodyChecks :: Map ObjectKey BodyCheck
    }


-- | Reads both databases at the same time, then the edges the command needs, given what
-- differs, alongside the body checks; the edges of @--to@ only for 'ForPlan'.
readBoth :: Source -> ConnectionString -> Purpose -> RIO AppEnv ComparisonInputs
readBoth from toDb purpose = do
    env <- ask
    let scope = env.scope
    liftIO $ withDatabase scope toDb \describeTo -> withAsync (timed "describe --to" describeTo) \toDescribed -> do
        withSource env.config from \fromDb -> withDatabase scope fromDb \describeFrom -> do
            (fromDatabase, toDatabase) <-
                timed "readBoth.describe" $ concurrently (timed "describe --from" describeFrom) (wait toDescribed)
            let fromSchema = fromDatabase.schema
                toSchema = toDatabase.schema
                plainChanges = diff (\_ a b -> a == b) fromSchema toSchema
                -- What depends on the changes in --from, to the end of the chain, so views
                -- on views of a retyped column are found; in --to, for ordering, what the
                -- changed objects use.
                fromNeeded = neededFor ForDiff plainChanges
                toNeeded = neededFor ForPlan plainChanges
            measure "readBoth.plainDiff" (Map.size plainChanges)
            (((fromEdges, fromNotes), (toEdges, toNotes)), checks) <-
                timed "readBoth.edgesAndChecks"
                    $ concurrently
                        ( concurrently
                            (edgesOf "--from" scope fromNeeded fromDatabase)
                            (if purpose == ForPlan then edgesOf "--to" scope toNeeded toDatabase else pure ([], []))
                        )
                        (bodyChecks scope toDb toSchema plainChanges)
            for_ (fromNotes <> toNotes) note
            pure
                ComparisonInputs
                    { source = SchemaSnapshot{schema = fromSchema, dependencies = indexEdges fromEdges}
                    , target = SchemaSnapshot{schema = toSchema, dependencies = indexEdges toEdges}
                    , bodyChecks = checks
                    }


-- | Body checks for the definitions that are added or changed: no other check is consulted.
bodyChecks :: Scope -> ConnectionString -> Schema -> Changes -> IO (Map ObjectKey BodyCheck)
bodyChecks scope toDb toSchema plainChanges
    | scope.bodyCheck == CheckBodies =
        withConnection toDb \conn ->
            timed "bodyChecks" $ checkBodies conn (Map.restrictKeys toSchema addedOrChanged)
    | otherwise = pure Map.empty
  where
    addedOrChanged = Map.keysSet (Map.filter (\case Dropped _ -> False; _ -> True) plainChanges)


-- | Runs @action@ with the database @--from@ names, holding the baseline for it when that
-- is what it names.
withSource :: Config -> Source -> (ConnectionString -> IO a) -> IO a
withSource config source action = case source of
    Database db -> action db
    BaselineAt rev -> timed "baseline" (withBaseline config.baseline rev action)
