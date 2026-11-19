{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What depends on an edit's dropped or changed objects, in the database it runs on.
module Sqiff.App.Dependents (
    Dependents (..),
    exactDependentsOf,
    routinesNaming,
    dependentsScope,
    dependentsIn,
) where

import RIO
import RIO.HashSet qualified as HS
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.App.Env (BodyChecks (..), Scope (..), note)
import Sqiff.App.Read (Connected (..), reachableEdges)
import Sqiff.Catalog (Partitions (..), relationColumns)
import Sqiff.Dependencies (Dependencies, indexEdges, noDependencies)
import Sqiff.Diff (BodyText (..), Change (..), Changes, affected)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Timing (measure, timed)


data Dependents = Dependents
    { exact :: Map ObjectId (Set ObjectId)
    -- ^ For each object the edit drops or changes, what depends on it, directly or not,
    -- by @pg_depend@ and by the PL/pgSQL bodies plpgsql_check reads.
    , named :: Map ObjectId (Set ObjectId)
    -- ^ For each such object, the routines whose bodies name it, which the catalog does
    -- not track.
    , dbSchema :: Schema
    , dbDeps :: Dependencies
    , dbTempTables :: IO (Maybe Text)
    -- ^ The statements that make plpgsql_check's temp tables there.
    }


-- | How 'dependentsIn' describes the database, leaving out @excluded@ schemas.
dependentsScope :: [Text] -> Scope
dependentsScope excluded = Scope{excluded, partitions = SkipPartitions, bodyCheck = SkipBodyChecks}


-- | What depends on @object@, by @pg_depend@ and PL/pgSQL bodies.
exactDependentsOf :: ObjectId -> Dependents -> Set ObjectId
-- [allow:SqiffNamedQueries] this is the query's definition
exactDependentsOf object deps = Map.findWithDefault Set.empty object deps.exact


-- | The routines whose bodies name @object@.
routinesNaming :: ObjectId -> Dependents -> Set ObjectId
-- [allow:SqiffNamedQueries] this is the query's definition
routinesNaming object deps = Map.findWithDefault Set.empty object deps.named


-- | For each dropped or changed definition, its dependents by @pg_depend@ and PL/pgSQL
-- bodies, and the other functions whose text names it. @described@ is the database, as
-- 'dependentsScope' reads it, with its functions' words; it is only asked for when
-- something is dropped or changed.
dependentsIn :: [Text] -> IO (Connected, Map ObjectId BodyText) -> Changes -> IO Dependents
dependentsIn excluded described changes
    | Set.null starts = pure Dependents{exact = Map.empty, named = Map.empty, dbSchema = Map.empty, dbDeps = noDependencies, dbTempTables = pure Nothing}
    | otherwise = do
        (connected, split_) <- timed "deps.describeDatabase" described
        (edges, notes) <- timed "deps.reachableEdges" $ reachableEdges "--db" (dependentsScope excluded) starts connected (pure split_)
        for_ notes note
        let schema = connected.schema
            known = Set.fromList [k.object | k <- Map.keys schema, k.aspect == Definition]
            deps = indexEdges edges
            exact = affected known deps changes
            viewsOf object = [v | a <- Set.toList (Map.findWithDefault Set.empty object exact), isView a, Just v <- [isRelation a]]
        measure "deps.affected" (Map.foldl' (\n s -> n + Set.size s) (0 :: Int) exact)
        viewColumns <- timed "deps.relationColumns" $ relationColumns connected.conn (Set.toList (Set.fromList (concatMap viewsOf (Set.toList starts))))
        let routines = Map.toList split_
            skip = Set.fromList (map (.object) (Map.keys changes)) <> fold exact
            lower = T.toLower . nameText
            throughViews column object =
                [ lower v.schema : lower v.name : [lower c | Just c <- [column], keepsName c]
                | v <- viewsOf object
                , let keepsName c = c `Set.member` Map.findWithDefault Set.empty v viewColumns
                ]
            -- The words a function body must contain to use the object: a column by its
            -- table and name, a dropped relation by its name, and both through views.
            named object = case object of
                Column t c -> [lower t.name, lower c] : throughViews (Just c) object
                _ | Just q <- isRelation object, droppedHere object -> [lower q.name] : throughViews Nothing object
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
        pure Dependents{exact, named = textual, dbSchema = schema, dbDeps = deps, dbTempTables = connected.tempTableScript}
  where
    droppedHere object = case Map.lookup (definitionKey object) changes of
        Just (Dropped _) -> True
        _ -> False
    starts = Set.fromList [k.object | (k, c) <- Map.toList changes, k.aspect == Definition, removesOrChanges c]
    removesOrChanges = \case Added _ -> False; _ -> True
