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
import RIO.Vector qualified as V
import RIO.Vector.Partial qualified as V


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


-- | Kahn's algorithm over @(x, y)@ pairs meaning x comes after y. Ties go to the smaller
-- node; nodes caught in a cycle come last.
topoOrder :: Ord a => Set a -> [(a, a)] -> [a]
topoOrder nodes pairs = case schedule [(n, n) | n <- Set.toList nodes] indexPairs of
    Right ordered -> ordered
    Left (done, blocked) -> done <> blocked
  where
    index = Map.fromList (zip (Set.toList nodes) [0 :: Int ..])
    indexPairs = [(ix, iy) | (x, y) <- pairs, Just ix <- [Map.lookup x index], Just iy <- [Map.lookup y index]]


-- | The items in an order where each comes after the ones @pairs@ name for it, as
-- @(after, before)@ positions in the list; among the items that are ready, the one with
-- the smallest priority goes first. When a cycle blocks some items, 'Left' has the ones
-- placed and the blocked ones, in priority order.
schedule :: forall p a. Ord p => [(p, a)] -> [(Int, Int)] -> Either ([a], [a]) [a]
schedule items pairs = go initial indegree0 []
  where
    -- Positions come from the list, so indexing it is safe.
    keyed = V.fromList items :: Vector (p, a)
    count = V.length keyed
    valid x = x >= 0 && x < count
    relevant = Set.fromList [(x, y) | (x, y) <- pairs, x /= y, valid x, valid y]
    successors = Map.fromListWith (<>) [(y, [x]) | (x, y) <- Set.toList relevant]
    indegree0 = Map.unionWith (+) (Map.fromList [(n, 0) | n <- [0 .. count - 1]]) (Map.fromListWith (+) [(x, 1 :: Int) | (x, _) <- Set.toList relevant])
    key n = (fst (keyed V.! n), n)
    item n = snd (keyed V.! n)
    initial = Set.fromList [key n | (n, 0) <- Map.toList indegree0]

    go ready indegree acc = case Set.minView ready of
        Nothing
            | length acc == count -> Right (reverse acc)
            | otherwise ->
                let blocked = Set.fromList [key n | (n, d) <- Map.toList indegree, d > 0]
                 in Left (reverse acc, [item n | (_, n) <- Set.toList blocked])
        Just ((_, n), rest) ->
            let (indegree', ready') = foldl' release (Map.insert n (-1) indegree, rest) (Map.findWithDefault [] n successors)
             in go ready' indegree' (item n : acc)

    release (m, r) s =
        let d = Map.findWithDefault 0 s m - 1
         in (Map.insert s d m, if d == 0 then Set.insert (key s) r else r)
