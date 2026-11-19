{-# LANGUAGE NoImplicitPrelude #-}

-- | The graph algorithms the planner and the dependency reports share.
module Sqiff.Graph (
    reachedFrom,
    topoOrder,
    schedule,
) where

import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set


-- | The nodes reachable from @starts@ by one step or more of @next@; a start is in it only
-- when it is reachable from a start.
reachedFrom :: Ord a => (a -> [a]) -> Set a -> Set a
reachedFrom next starts = go Set.empty (step starts)
  where
    step = Set.fromList . concatMap next . Set.toList
    go seen frontier
        | Set.null frontier = seen
        | otherwise =
            let seen' = seen <> frontier
             in go seen' (step frontier `Set.difference` seen')


-- | The nodes with each after the ones @pairs@ name for it, as @(after, before)@. Ties go
-- to the smaller node; nodes caught in a cycle come last.
topoOrder :: Ord a => Set a -> [(a, a)] -> [a]
topoOrder nodes pairs = case schedule id id (Set.toList nodes) pairs of
    Right ordered -> ordered
    Left (placed, blocked) -> placed <> blocked


-- | Kahn's algorithm: the items, each after the ones @pairs@ name for it, as
-- @(after, before)@ keys; pairs naming a key no item has are ignored. Among the items
-- that are ready, the one with the smallest priority goes first, then the smallest key.
-- When a cycle blocks some items, 'Left' has the ones placed and the blocked ones, in
-- priority order. Keys are distinct.
schedule :: forall k p a. (Ord k, Ord p) => (a -> k) -> (a -> p) -> [a] -> [(k, k)] -> Either ([a], [a]) [a]
schedule keyOf priorityOf items pairs = go (queue ready0) waiting0 []
  where
    byKey = Map.fromList [(keyOf x, x) | x <- items]
    relevant = Set.fromList [(x, y) | (x, y) <- pairs, x /= y, x `Map.member` byKey, y `Map.member` byKey]
    successors = Map.fromListWith (<>) [(y, [x]) | (x, y) <- Set.toList relevant]
    -- How many items each item still waits for; items waiting for none are not in it.
    waiting0 = Map.fromListWith (+) [(x, 1 :: Int) | (x, _) <- Set.toList relevant]
    ready0 = [k | k <- Map.keys byKey, k `Map.notMember` waiting0]

    -- The items with these keys, by priority.
    queue :: [k] -> Map (p, k) a
    queue ks = Map.fromList [((priorityOf x, k), x) | k <- ks, Just x <- [Map.lookup k byKey]]

    go ready waiting placed = case Map.minViewWithKey ready of
        Nothing
            | Map.null waiting -> Right (reverse placed)
            | otherwise -> Left (reverse placed, Map.elems (queue (Map.keys waiting)))
        Just (((_, k), x), rest) ->
            let (released, waiting') = foldl' release ([], waiting) (Map.findWithDefault [] k successors)
             in go (rest <> queue released) waiting' (x : placed)

    release (released, waiting) k = case Map.lookup k waiting of
        Just 1 -> (k : released, Map.delete k waiting)
        Just n -> (released, Map.insert k (n - 1) waiting)
        Nothing -> (released, waiting)
