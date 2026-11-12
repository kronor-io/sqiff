{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Orders recipe steps: drops before creates, then by rank, then by dependencies.
module Kronor.SchemaDiff.Plan (plan, planWith, context, invert, renderStep, topoOrder) where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Kronor.SchemaDiff.Body (BodyCheck)
import Kronor.SchemaDiff.Catalog (Aspect (..), Edge (..), ObjectKey (..), Schema, render)
import Kronor.SchemaDiff.Diff (Change (..))
import Kronor.SchemaDiff.Object
import Kronor.SchemaDiff.Recipe
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set


-- | @fromEdges@ come from the database being changed; @toEdges@ and @checks@ from the desired one.
plan :: Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> [Edge] -> Map ObjectKey Change -> [Step]
plan = planWith id


-- | 'plan' with the context adjusted first.
planWith :: (Context -> Context) -> Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> [Edge] -> Map ObjectKey Change -> [Step]
planWith adjust checks fromSchema toSchema fromEdges toEdges changes =
    map snd
        $ L.sortOn
            fst
            [ ((s.phase, s.rank, position s, n), s)
            | (n, s) <- zip [0 :: Int ..] (concatMap (uncurry (recipe (adjust (context checks fromSchema toSchema fromEdges changes)))) (Map.toList changes))
            ]
  where
    stepObjects = Set.fromList [fromAddress k.address | k <- Map.keys changes]

    withOwners edges =
        [(fromAddress e.dependent, fromAddress e.referenced) | e <- edges]
            <> [(o, Table t) | o <- Set.toList stepObjects, Just t <- [owningTable o]]

    createOrder = indexOf (topoOrder stepObjects (withOwners toEdges))
    dropOrder = indexOf (topoOrder stepObjects [(b, a) | (a, b) <- withOwners fromEdges])
    indexOf xs = Map.fromList (zip xs [0 :: Int ..])

    position s = Map.findWithDefault maxBound s.object (if s.phase == DropPhase then dropOrder else createOrder)


-- | @checks@ were made against @toSchema@'s definitions; @fromEdges@ come from the
-- database being changed.
context :: Map ObjectKey BodyCheck -> Schema -> Schema -> [Edge] -> Map ObjectKey Change -> Context
context checks fromSchema toSchema fromEdges changes =
    Context
        { bodyChecks = Map.intersectionWith (,) toSchema checks
        , withDependents = Set.fromList [fromAddress e.referenced | e <- fromEdges]
        , recreatedViews =
            Map.fromList
                [ (v, (indexesOn v, (grantsKey,) <$> Map.lookup grantsKey toSchema))
                | (k, Changed _ _) <- Map.toList changes
                , k.aspect == Definition
                , let grantsKey = k{aspect = Privileges}
                , MaterializedView v <- [fromAddress k.address]
                ]
        , newTables = tablesWith isAdded
        , droppedTables = tablesWith isDropped
        , partitionedTables =
            Set.fromList
                [ t
                | (k, def) <- Map.toList fromSchema <> Map.toList toSchema
                , k.aspect == Definition
                , any ("partition by " `T.isPrefixOf`) (T.lines def)
                , Table t <- [fromAddress k.address]
                ]
        , droppedColumns = Set.fromList [(t, c) | (Column t c, Dropped _) <- definitions]
        , retypedColumns =
            Set.fromList
                [ (t, c)
                | (Column t c, Changed old new) <- definitions
                , let o = parseColumn old
                      n = parseColumn new
                , o.colType /= n.colType || o.collation /= n.collation
                ]
        , viewsAround = Map.empty
        , droppedObjects = Set.fromList [k.address | (k, Dropped _) <- Map.toList changes, k.aspect == Definition]
        , newTableColumns =
            Map.fromListWith
                (flip (<>))
                [(t, [(c, parseColumn def)]) | (Column t c, Added def) <- definitions, t `Set.member` tablesWith isAdded]
        }
  where
    definitions = [(fromAddress k.address, change) | (k, change) <- Map.toList changes, k.aspect == Definition]
    indexesOn v =
        [ def
        | (k, def) <- Map.toList toSchema
        , k.aspect == Definition
        , k `Map.notMember` changes
        , [i| ON #{qualified v} |] `T.isInfixOf` def
        , Index _ <- [fromAddress k.address]
        ]
    tablesWith p = Set.fromList [t | (Table t, change) <- definitions, p change]
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False


-- | The change that undoes this one.
invert :: Change -> Change
invert = \case
    Added new -> Dropped new
    Dropped old -> Added old
    Changed old new -> Changed new old


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
        $ ["-- " <> render s.address]
        <> ["-- " <> note | note <- txnNote <> map hazardNote s.hazards]
        <> [s.sql <> if "--" `T.isPrefixOf` s.sql then "" else ";"]
  where
    txnNote = case s.txn of
        Shared -> []
        Own -> ["own transaction"]
        None -> ["outside a transaction"]
    hazardNote = \case
        DeletesData -> "hazard: deletes data"
        MayRewriteTable -> "hazard: may rewrite the table"
        LongLock -> "hazard: holds a lock that blocks writes while it runs"
        IndexBuild -> "hazard: builds an index"
        MayFail why -> "hazard: " <> why
        Manual why -> "manual: " <> why
