{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The commands that compare whole databases: describe one, list what differs between
-- two, print the migration, or write it as sqitch changes.
module Sqiff.App.Compare (
    runDescribe,
    runDiff,
    runPlan,
    runSqitch,
    report,
    summary,
) where

import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Data.String.Interpolate (i)
import Data.Time (getCurrentTime)
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.App.Env (AppEnv (..), Scope (..), Source, say)
import Sqiff.App.Options (DescribeOptions (..), SqitchOptions (..))
import Sqiff.App.Read (Purpose (..), describeConnections, readBoth)
import Sqiff.Body (BodyCheck, sameDefinition)
import Sqiff.Catalog (describeOn)
import Sqiff.Config (Config (..))
import Sqiff.Connection (failWith, withConnections, withSharedSnapshot)
import Sqiff.ConnectionString (ConnectionString)
import Sqiff.Definition (Definition, printed)
import Sqiff.Diff (Change (..), Changes, affected, diff)
import Sqiff.Folder (writeFolder)
import Sqiff.Model
import Sqiff.Plan (context, cycleMessage, invert, planWith, renderStep)
import Sqiff.Recipe (Step)
import Sqiff.Recreate (withViewsAround, withViewsReplaced)
import Sqiff.Sqitch (withProject, writeUnits)
import Sqiff.Sqitch.Check (Tagging (..), batchProblems)
import Sqiff.Sqitch.Layout (findLayout, planPath)
import Sqiff.Sqitch.Plan (readPlan)
import Sqiff.Sqitch.Units (Unit (..), UnitKind (..), units)
import Sqiff.Timing (measure, timed)


-- | Writes one file per object to @--out@, or counts the objects by kind.
runDescribe :: DescribeOptions -> RIO AppEnv ()
runDescribe opts = do
    scope <- asks (.scope)
    schema <- timed "describe.describeOn" $ withConnections opts.db describeConnections \conns ->
        liftIO (fst <$> withSharedSnapshot conns (describeOn (toList conns) scope.excluded scope.partitions Nothing))
    case opts.out of
        Just dir -> liftIO (writeFolder opts.existing dir schema)
        Nothing ->
            for_ (Map.toList (Map.fromListWith (+) [((toAddress k.object).objectType, 1 :: Int) | k <- Map.keys schema])) \(kind, n) ->
                say [i|#{n}\t#{kind}|]


-- | Lists what differs, with what depends on it; exits 1 when something does.
runDiff :: Source -> ConnectionString -> RIO AppEnv ()
runDiff from toDb = do
    ((fromSchema, edges), (toSchema, _), checks) <- timed "readBoth" $ readBoth from toDb ForDiff
    let changes = diff (sameDefinition checks) fromSchema toSchema
        known = Set.fromList [k.object | k <- Map.keys fromSchema, k.aspect == Definition]
        affectedBy = affected known edges changes
        dependentsOf key = case key.aspect of
            Definition -> Map.findWithDefault Set.empty key.object affectedBy
            _ -> Set.empty
    measure "diff.diffAndAffected" (Map.size changes + Map.foldl' (\n s -> n + Set.size s) 0 affectedBy)
    -- In catalog order: by aspect, then kind and names as the catalog spells them.
    for_ (L.sortOn (\(key, _) -> (key.aspect, toAddress key.object)) (Map.toList changes)) \(key, change) ->
        report key change (dependentsOf key)
    say (summary changes)
    unless (Map.null changes) $ exitWith (ExitFailure 1)


-- | Prints the migration's steps.
runPlan :: Source -> ConnectionString -> RIO AppEnv ()
runPlan from toDb = do
    planned <- planBoth from toDb
    measure "plan.planRender" (T.length (T.unlines (map renderStep planned.steps)))
    for_ planned.steps (say . renderStep)


-- | Writes the migration as sqitch changes, or lists them with @--dry-run@.
runSqitch :: Source -> ConnectionString -> SqitchOptions -> RIO AppEnv ()
runSqitch from toDb opts = do
    config <- asks (.config)
    layout <- liftIO (findLayout opts.projectDir opts.topDir opts.planFile)
    sqitchPlan <- liftIO (readPlan (planPath layout))
    planned <- planBoth from toDb
    now <- liftIO getCurrentTime
    let inverse = Map.map invert planned.changes
        -- A retype's views are the same on both sides, so its revert recreates them
        -- around the column as the plan does.
        inverseCtx =
            withViewsReplaced planned.fromSchema planned.toEdges inverse
                . withViewsAround planned.fromSchema planned.fromEdges planned.changes
                $ context Map.empty planned.toSchema planned.fromSchema planned.toEdges inverse
        written = units config.names (const True) inverseCtx planned.changes sqitchPlan now planned.steps
        tagging = Tagging{given = opts.tag, now}
    measure "sqitch.planUnits" (sum [length u.deploy + length u.revert | u <- written])
    if opts.dryRun
        then for_ written \u -> do
            let kind = case u.kind of AddChange -> "add" :: Text; ReworkChange -> "rework"
                count = length u.deploy
                changeName = u.changeName
                unitNote = u.note
            say [i|#{kind}\t#{changeName}\t#{count} statements\t#{unitNote}|]
        else liftIO (withProject layout \project -> writeUnits project config.templates sqitchPlan tagging written)
    -- A dry run reports what would stop the write, too.
    case batchProblems sqitchPlan written of
        problems@(_ : _) | opts.dryRun -> failWith (T.intercalate "; " problems)
        _ -> pure ()


-- | Both databases, what differs and the steps that migrate it.
data Planned = Planned
    { fromSchema :: Schema
    , fromEdges :: [Edge]
    , toSchema :: Schema
    , toEdges :: [Edge]
    , changes :: Changes
    , steps :: [Step]
    }


planBoth :: Source -> ConnectionString -> RIO AppEnv Planned
planBoth from toDb = do
    ((fromSchema, fromEdges), (toSchema, toEdges), checks) <- timed "readBoth" $ readBoth from toDb ForPlan
    let changes = diff (sameDefinition checks) fromSchema toSchema
    steps <- either (failWith . cycleMessage) pure (planWith (withViewsReplaced toSchema fromEdges changes . withViewsAround fromSchema fromEdges changes) (checks :: Map ObjectKey BodyCheck) fromSchema toSchema fromEdges toEdges changes)
    pure Planned{fromSchema, fromEdges, toSchema, toEdges, changes, steps}


report :: MonadIO m => ObjectKey -> Change Definition -> Set ObjectId -> m ()
report key change dependents = do
    let name = render key.object
    say [i|#{verb} #{name}#{suffix}|]
    case change of
        Changed old new ->
            for_ (getDiff (T.lines (printed old)) (T.lines (printed new))) \case
                First line -> say [i|  - #{line}|]
                Second line -> say [i|  + #{line}|]
                Both _ _ -> pure ()
        _ -> pure ()
    unless (Set.null dependents) do
        say "  depended on by:"
        for_ (L.sortOn toAddress (Set.toList dependents)) \addr -> say [i|    #{render addr}|]
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
        Comment -> " (comment)"


summary :: Changes -> Text
summary changes = [i|#{count isAdded} added, #{count isDropped} dropped, #{count isChanged} changed|]
  where
    count p = L.length (filter p (Map.elems changes))
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False
    isChanged = \case Changed _ _ -> True; _ -> False
