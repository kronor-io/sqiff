{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Which objects depend on which, indexed once so each question about it is a lookup.
module Sqiff.Dependencies (
    Dependencies,
    indexEdges,
    noDependencies,
    edges,
    restrictedTo,
    directDependents,
    dependentsOf,
    referencesOf,
    dependsOn,
    reachableFrom,
) where

import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Graph (reachedFrom)
import Sqiff.Identifier (QName)
import Sqiff.Model (Edge (..), ObjectId (..), isRelation)


-- | The edges between objects, by what they reference and by what depends on them.
data Dependencies = Dependencies
    { edgeList :: [Edge]
    , dependents :: Map ObjectId (Set ObjectId)
    , references :: Map ObjectId (Set ObjectId)
    , columnsUsed :: Map QName (Set ObjectId)
    -- ^ For each relation, its columns that something depends on.
    }


indexEdges :: [Edge] -> Dependencies
indexEdges es =
    Dependencies
        { edgeList = es
        , dependents = Map.fromListWith (<>) [(e.referenced, Set.singleton e.dependent) | e <- es]
        , references = Map.fromListWith (<>) [(e.dependent, Set.singleton e.referenced) | e <- es]
        , columnsUsed = Map.fromListWith (<>) [(t, Set.singleton e.referenced) | e <- es, Column t _ <- [e.referenced]]
        }


-- | No object depends on another.
noDependencies :: Dependencies
noDependencies = indexEdges []


-- | Every edge, as the catalog gave them.
edges :: Dependencies -> [Edge]
edges = (.edgeList)


-- | Only the edges whose two ends are both in @known@.
restrictedTo :: Set ObjectId -> Dependencies -> Dependencies
restrictedTo known deps = indexEdges [e | e <- deps.edgeList, e.dependent `Set.member` known, e.referenced `Set.member` known]


-- | What depends on the object itself.
directDependents :: Dependencies -> ObjectId -> Set ObjectId
directDependents deps object = Map.findWithDefault Set.empty object deps.dependents


-- | What depends on the object; for a relation, also what depends on its columns.
dependentsOf :: Dependencies -> ObjectId -> Set ObjectId
dependentsOf deps object = directDependents deps object <> foldMap (directDependents deps) columns
  where
    columns = maybe Set.empty (\t -> Map.findWithDefault Set.empty t deps.columnsUsed) (isRelation object)


-- | What the object depends on directly.
referencesOf :: Dependencies -> ObjectId -> Set ObjectId
referencesOf deps object = Map.findWithDefault Set.empty object deps.references


-- | Whether @dependent@ depends on @referenced@ directly.
dependsOn :: Dependencies -> ObjectId -> ObjectId -> Bool
dependsOn deps dependent referenced = referenced `Set.member` referencesOf deps dependent


-- | Everything that depends on @starts@, directly or through others, by 'dependentsOf'.
reachableFrom :: Dependencies -> Set ObjectId -> Set ObjectId
reachableFrom deps = reachedFrom (Set.toList . dependentsOf deps)
