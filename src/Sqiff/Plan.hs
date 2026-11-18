{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Orders recipe steps: drops before creates, then by rank, then by dependencies.
module Sqiff.Plan (plan, planWith, context, invert, renderStep, topoOrder) where

import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Body (BodyCheck)
import Sqiff.Definition
import Sqiff.Diff (Change (..), Changes, invert)
import Sqiff.Model
import Sqiff.Recipe


-- | @fromEdges@ come from the database being changed; @toEdges@ and @checks@ from the desired one.
plan :: Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> [Edge] -> Changes -> [Step]
plan = planWith id


-- | 'plan' with the context adjusted first.
planWith :: (Context -> Context) -> Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> [Edge] -> Changes -> [Step]
planWith adjust checks fromSchema toSchema fromEdges toEdges changes =
    map snd
        $ L.sortOn
            fst
            [ ((s.phase, s.rank, position s, n), s)
            | (n, s) <- zip [0 :: Int ..] (concatMap (uncurry (recipe (adjust (context checks fromSchema toSchema fromEdges changes)))) (Map.toList changes))
            ]
  where
    stepObjects = Set.fromList [k.object | k <- Map.keys changes]

    withOwners edges =
        [(e.dependent, e.referenced) | e <- edges]
            <> [(o, Table t) | o <- Set.toList stepObjects, Just t <- [owningTable o]]

    createOrder = indexOf (topoOrder stepObjects (withOwners toEdges))
    dropOrder = indexOf (topoOrder stepObjects [(b, a) | (a, b) <- withOwners fromEdges])
    indexOf xs = Map.fromList (zip xs [0 :: Int ..])

    position s = Map.findWithDefault maxBound s.object (if s.phase == DropPhase then dropOrder else createOrder)


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
            Map.fromListWith
                (flip (<>))
                [(t, [(c, def)]) | (Column t c, Added (ColumnDefinition def)) <- definitions, t `Set.member` tablesWith isAdded]
        }
  where
    definitions = [(k.object, change) | (k, change) <- Map.toList changes, k.aspect == Definition]
    indexesOn v =
        [ def.statement
        | (k, IndexDefinition def) <- Map.toList toSchema
        , k.aspect == Definition
        , k `Map.notMember` changes
        , def.table == v
        ]
    tablesWith p = Set.fromList [t | (Table t, change) <- definitions, p change]
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False


-- | Kahn's algorithm over @(x, y)@ pairs meaning x comes after y. Ties go to the smaller
-- node; nodes caught in a cycle come last.
topoOrder :: Ord a => Set a -> [(a, a)] -> [a]
topoOrder nodes pairs = go ready0 indegree0 []
  where
    relevant = Set.fromList [(x, y) | (x, y) <- pairs, x /= y, x `Set.member` nodes, y `Set.member` nodes]
    successors = Map.fromListWith (<>) [(y, Set.singleton x) | (x, y) <- Set.toList relevant]
    indegree0 = Map.unionWith (+) (Map.fromSet (const (0 :: Int)) nodes) (Map.fromListWith (+) [(x, 1) | (x, _) <- Set.toList relevant])
    ready0 = Map.keysSet (Map.filter (== 0) indegree0)

    go ready indegree acc = case Set.minView ready of
        Nothing ->
            let done = reverse acc
             in done <> Set.toList (nodes `Set.difference` Set.fromList done)
        Just (n, rest) ->
            let (indegree', newlyReady) = foldl' release (indegree, Set.empty) (Map.findWithDefault Set.empty n successors)
             in go (rest <> newlyReady) indegree' (n : acc)

    release (m, r) s =
        let d = Map.findWithDefault 0 s m - 1
         in (Map.insert s d m, if d == 0 then Set.insert s r else r)


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
