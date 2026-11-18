{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Orders recipe steps: drops before creates, each after what it depends on.
module Sqiff.Plan (
    plan,
    planWith,
    DependencyCycle (..),
    cycleMessage,
    context,
    invert,
    renderStep,
    topoOrder,
) where

import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Body (BodyCheck)
import Sqiff.Definition
import Sqiff.Diff (Change (..), Changes, invert)
import Sqiff.Graph (schedule, topoOrder)
import Sqiff.Model
import Sqiff.Recipe


-- | Objects whose steps depend on each other in a circle, so no order works.
newtype DependencyCycle = DependencyCycle (NonEmpty ObjectId)
    deriving stock (Eq, Show)


cycleMessage :: DependencyCycle -> Text
cycleMessage (DependencyCycle objects) =
    "no order satisfies the dependencies between these objects; change them by hand: "
        <> T.intercalate ", " (map render (toList objects))


-- | @fromEdges@ come from the database being changed; @toEdges@ and @checks@ from the desired one.
plan :: Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> [Edge] -> Changes -> Either DependencyCycle [Step]
plan = planWith id


-- | 'plan' with the context adjusted first.
--
-- Drops come before creates. Within each phase a step waits for what it depends on:
-- creating an object waits for the objects it uses, and dropping one waits for its
-- dependents' drops. Among the steps that are ready, the kind of object decides (its
-- rank), then the dependency order of the objects, then the order the recipes gave.
planWith :: (Context -> Context) -> Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> [Edge] -> Changes -> Either DependencyCycle [Step]
planWith adjust checks fromSchema toSchema fromEdges toEdges changes = do
    drops <- inPhase DropPhase [(b, a) | (a, b) <- withOwners fromEdges, not (owns a b)]
    creates <- inPhase CreatePhase (withOwners toEdges)
    pure (drops <> creates)
  where
    ctx = adjust (context checks fromSchema toSchema fromEdges changes)
    steps = concatMap (uncurry (recipe ctx)) (Map.toList changes)
    stepObjects = Set.fromList [k.object | k <- Map.keys changes]

    withOwners edges =
        [(e.dependent, e.referenced) | e <- edges]
            <> [(o, Table t) | o <- Set.toList stepObjects, Just t <- [owningTable o]]

    createOrder = indexOf (topoOrder stepObjects (withOwners toEdges))
    dropOrder = indexOf (topoOrder stepObjects [(b, a) | (a, b) <- withOwners fromEdges])
    indexOf xs = Map.fromList (zip xs [0 :: Int ..])
    position s = Map.findWithDefault maxBound s.object (if s.phase == DropPhase then dropOrder else createOrder)

    -- The phase's steps in order, where @(a, b)@ in @pairs@ means a's steps wait for b's.
    inPhase phase pairs =
        let numbered = [(n, s) | (n, s) <- zip [0 :: Int ..] steps, s.phase == phase]
            priority (n, s) = (s.rank, position s, n)
            ordered = L.sortOn priority numbered
            slot = Map.fromList (zip (map fst ordered) [0 :: Int ..])
            byObject = Map.fromListWith (flip (<>)) [(s.object, [n]) | (n, s) <- ordered]
            stepsOf o = Map.findWithDefault [] o byObject
            at n = Map.lookup n slot
            -- An object's own steps keep their order.
            chained = [(l, e) | ns <- Map.elems byObject, (earlier, later) <- zip ns (drop 1 ns), Just l <- [at later], Just e <- [at earlier]]
            -- a's first step waits for b's main step when creating, as b exists from
            -- then on, and for b's last step when dropping, as b is gone after it. A new
            -- table's columns have no steps: the table's waits for what they use.
            waits =
                [ (w, d)
                | (a, b) <- pairs
                , a /= b
                , let a' = if null (stepsOf a) then maybe a Table (owningTable a) else a
                , a' /= b
                , waiting <- waitingSteps phase a' b (stepsOf a')
                , waited <- if phase == CreatePhase then mainStep b else take 1 (reverse (stepsOf b))
                , Just w <- [at waiting]
                , Just d <- [at waited]
                ]
            -- The step that makes or replaces the object, not one that changes its
            -- settings, grants or comment.
            mainStep b = take 1 [n | n <- stepsOf b, Just s <- [Map.lookup n stepAt], s.rank == createRank b]
         in case schedule [(priority x, snd x) | x <- ordered] (chained <> waits) of
                Right placed -> Right placed
                Left (_, b : bs) -> Left (DependencyCycle (b.object :| filter (/= b.object) (nubOrd (map (.object) bs))))
                Left (placed, []) -> Right placed

    stepAt = Map.fromList (zip [0 :: Int ..] steps)

    -- The steps of a that wait for b: its first, or, for a sequence's ownership by a
    -- column, the step that sets the owner. The sequence itself comes first, as the
    -- column's default uses it.
    waitingSteps phase a b ns
        | phase == CreatePhase, owns a b = [n | n <- ns, Just s <- [Map.lookup n stepAt], s.rank == sequenceOwnerRank]
        | otherwise = take 1 ns

    -- A sequence owned by a column: dropping the column drops the sequence.
    owns a b = case (a, b) of
        (Sequence _, Column{}) -> True
        _ -> False


-- | @checks@ were made against @toSchema@'s definitions; @fromEdges@ come from the
-- database being changed.
context :: Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> Changes -> Context
context checks fromSchema toSchema fromEdges changes =
    Context
        { bodyChecks = Map.intersectionWith (,) (Map.map printed toSchema) checks
        , withDependents = Set.fromList [e.referenced | e <- fromEdges]
        , recreatedViews =
            Map.fromList
                [ (v, (indexesOn v, (grantsKey,) <$> Map.lookup grantsKey toSchema))
                | (k, Changed _ _) <- Map.toList changes
                , k.aspect == Definition
                , let grantsKey = k{aspect = Privileges}
                , MaterializedView v <- [k.object]
                ]
        , keptComments =
            Map.fromList
                [ (k.object, printed def)
                | (k, def) <- Map.toList toSchema
                , k.aspect == Comment
                , k `Map.notMember` changes
                ]
        , newTables = tablesWith isAdded
        , droppedTables = tablesWith isDropped
        , partitionedTables =
            Set.fromList
                [ t
                | (k, TableDefinition def) <- Map.toList fromSchema <> Map.toList toSchema
                , k.aspect == Definition
                , isJust def.partitionBy
                , Table t <- [k.object]
                ]
        , droppedColumns = Set.fromList [(t, c) | (Column t c, Dropped _) <- definitions]
        , retypedColumns =
            Set.fromList
                [ (t, c)
                | (Column t c, Changed (ColumnDefinition o) (ColumnDefinition n)) <- definitions
                , o.colType /= n.colType || o.collation /= n.collation
                ]
        , viewsAround = Map.empty
        , droppedObjects = Set.fromList [k.object | (k, Dropped _) <- Map.toList changes, k.aspect == Definition]
        , newTableColumns =
            Map.map (L.sortOn (\(_, def) -> positionNumber def.position))
                . Map.fromListWith (flip (<>))
                $ [(t, [(c, def)]) | (Column t c, Added (ColumnDefinition def)) <- definitions, t `Set.member` tablesWith isAdded]
        }
  where
    definitions = [(k.object, change) | (k, change) <- Map.toList changes, k.aspect == Definition]
    indexesOn v =
        [ (q, def.statement)
        | (k, IndexDefinition def) <- Map.toList toSchema
        , k.aspect == Definition
        , k `Map.notMember` changes
        , Index q <- [k.object]
        , def.table == v
        ]
    tablesWith p = Set.fromList [t | (Table t, change) <- definitions, p change]
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False


renderStep :: Step -> Text
renderStep s =
    T.unlines
        $ ["-- " <> render s.object]
        <> ["-- " <> note | note <- txnNote <> map hazardText s.hazards]
        <> [s.sql <> if "--" `T.isPrefixOf` s.sql then "" else ";"]
  where
    txnNote = case s.txn of
        Shared -> []
        Own -> ["own transaction"]
        None -> ["outside a transaction"]
