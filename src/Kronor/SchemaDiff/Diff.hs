{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Kronor.SchemaDiff.Diff (
    Change (..),
    diff,
    affected,
) where

import Data.Map.Merge.Strict qualified as Merge
import Kronor.SchemaDiff.Catalog (Address, Aspect (..), Edge (..), ObjectKey (..), Schema)
import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set


data Change
    = Added Text
    | Dropped Text
    | Changed Text Text
    deriving stock (Eq, Show)


-- | What turns @from@ into @to@, with @same key old new@ deciding when two definitions match.
diff :: (ObjectKey -> Text -> Text -> Bool) -> Schema -> Schema -> Map ObjectKey Change
diff same =
    Merge.merge
        (Merge.mapMissing \_ old -> Dropped old)
        (Merge.mapMissing \_ new -> Added new)
        (Merge.zipWithMaybeMatched \k old new -> if same k old new then Nothing else Just (Changed old new))


-- | For each dropped or changed definition, the objects in @known@ that depend on it,
-- directly or through others, and are not part of the diff themselves. Edges and
-- @known@ come from the database being changed.
affected :: Set Address -> [Edge] -> Map ObjectKey Change -> Map Address (Set Address)
affected known edges changes =
    Map.filter (not . Set.null)
        $ Map.fromList
            [ (key.address, closure key.address `Set.difference` inDiff)
            | (key, change) <- Map.toList changes
            , key.aspect == Definition
            , removesOrChanges change
            ]
  where
    inDiff = Set.fromList (map (.address) (Map.keys changes))

    dependents =
        Map.fromListWith
            (<>)
            [ (edge.referenced, Set.singleton edge.dependent)
            | edge <- edges
            , edge.referenced `Set.member` known
            , edge.dependent `Set.member` known
            ]

    direct addr = Map.findWithDefault Set.empty addr dependents

    closure start = go Set.empty (direct start)
      where
        go seen frontier
            | Set.null frontier = seen
            | otherwise =
                let seen' = seen <> frontier
                 in go seen' (foldMap direct frontier `Set.difference` seen')

    removesOrChanges = \case
        Added _ -> False
        Dropped _ -> True
        Changed _ _ -> True
