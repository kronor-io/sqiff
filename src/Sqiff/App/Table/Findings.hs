{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What the table command reports about an edit, from what the comparison and the checks
-- found: the migration's steps, what depends on the edit, and what blocks it.
module Sqiff.App.Table.Findings (
    Retyped (..),
    retypedObject,
    retypedColumns,
    BodyResults (..),
    Evidence (..),
    Findings (..),
    findingsFor,
    foreignKeyTypes,
) where

import Data.String.Interpolate (i)
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.App.Dependents (Dependents (..))
import Sqiff.App.Options (Comparison (..), MigrateOptions (..), SameTypeSwap (..), TableOptions (..), renamesOf)
import Sqiff.App.Table.Env
import Sqiff.Config (Config (..))
import Sqiff.Definition hiding (DomainType)
import Sqiff.Diff (Change (..))
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (DependencyCycle, planWith)
import Sqiff.Recipe (Context (viewsAround), Hazard (..), Phase (..), Step (..), Txn (..))
import Sqiff.Recreate (viewsAroundTypeChanges)
import Sqiff.TableEdit (TableEdit (..), TableRewrite (..), TargetType (..), TypeChange (..))


-- | A column whose type the edit changes.
data Retyped = Retyped
    { table :: QName
    , column :: Identifier
    , newType :: TypeName
    }


retypedObject :: Retyped -> ObjectId
retypedObject r = Column r.table r.column


-- | The functions the body check finds broken by the edit, and the ones it passes.
data BodyResults = BodyResults
    { breaking :: Map ObjectId Text
    , passed :: Set ObjectId
    }


-- | What the comparison and the checks found.
data Evidence = Evidence
    { edit :: TableEdit
    , deps :: Dependents
    , bodies :: BodyResults
    , retyped :: [Retyped]
    , foreignKeyBreaks :: Map ObjectId [(ObjectId, Text)]
    , typeFacts :: Map ObjectId TypeChange
    , branchChanges :: [(Text, [Text])]
    , pendingRenames :: [(Identifier, Identifier)]
    , renamedFrom :: Set ObjectId
    }


-- | The migration's steps and what the command reports about them.
data Findings = Findings
    { steps :: [Step]
    , renameSteps :: [Step]
    , newRenames :: [(Identifier, Identifier)]
    , usage :: [Text]
    , blocking :: [ObjectId]
    , renameLike :: [(Identifier, Identifier)]
    , same :: Text
    , withViews :: Context -> Context
    }


retypedColumns :: TableEdit -> [Retyped]
retypedColumns edit =
    [ Retyped{table = t, column = c, newType = n.colType}
    | (k, Changed old new) <- Map.toList edit.changes
    , k.aspect == Definition
    , ColumnDefinition o <- [old]
    , ColumnDefinition n <- [new]
    , o.colType /= n.colType || o.collation /= n.collation
    , Column t c <- [k.object]
    ]


findingsFor :: TableEnv -> MigrateOptions -> Evidence -> Either DependencyCycle Findings
findingsFor env migrate ev = do
    planned <- planWith withViews Map.empty edit.current edit.desired [] [] edit.changes
    let steps = renameSteps <> map withTypeFacts planned
    pure Findings{steps, renameSteps, newRenames = ev.pendingRenames, usage, blocking, renameLike, same, withViews}
  where
    opts = env.opts
    table = env.table
    edit = ev.edit
    deps = ev.deps
    breaking = ev.bodies.breaking
    passed = ev.bodies.passed
    retyped = ev.retyped
    otherTables addr =
        Set.toList
            . Set.fromList
            $ [ other
              | k@(TableConstraint onTable _) <- Set.toList (Map.findWithDefault Set.empty addr deps.exact)
              , Just (ConstraintDefinition def) <- [Map.lookup (definitionKey k) deps.dbSchema]
              , Just target <- [def.references]
              , let other = if onTable == table then qualified target.table else qualified onTable
              , other /= qualified table
              ]
    convertNote = MayFail "fails when an existing value does not convert to the new type"
    withTypeFacts s = case Map.lookup s.object ev.typeFacts of
        Just (TypeChange rewrite target)
            | MayRewriteTable `elem` s.hazards ->
                let rewrites = rewrite == RewritesFiles
                    domain = target == DomainType
                 in let kept = concatMap (\h -> if h == MayRewriteTable then [RewritesTable | rewrites] else [h | h /= convertNote || rewrites || domain]) s.hazards
                     in s{hazards = kept <> concat [LocksOther o : [RechecksForeignKey o | rewrites] | o <- otherTables s.object]}
        _ -> s
    breaksFor addr = [a | a <- Set.toList (Map.findWithDefault Set.empty addr deps.named <> Map.findWithDefault Set.empty addr deps.exact), a `Map.member` breaking]
    dependents = deps.exact
    mentions = Map.map (Set.filter (\a -> a `Map.notMember` breaking && a `Set.notMember` passed)) deps.named
    retypedAddresses = Set.fromList (map retypedObject retyped)
    around = viewsAroundTypeChanges deps.exact deps.dbSchema deps.dbEdges [(r.table, r.column, retypedObject r) | r <- retyped]
    withViews c = c{viewsAround = Map.map snd around}
    aroundOf addr = fold [members | r <- retyped, retypedObject r == addr, Just (members, _) <- [Map.lookup (r.table, r.column) around]]
    foreignKeysFor addr = Map.findWithDefault [] addr ev.foreignKeyBreaks
    renameHazard = MayFail ("breaks running code that still uses the old name" <> foldMap ("; " <>) env.config.renameHint)
    renameSteps =
        [ Step DropPhase (-1) addr (renameSql table (old, new)) Shared [renameHazard] (definitionKey addr) (Changed def def)
        | (old, new) <- ev.pendingRenames
        , let addr = Column table new
              def = Map.findWithDefault (Printed "") (definitionKey addr) edit.desired
        ]
    same :: Text
    same
        | WithLiveTable _ <- migrate.comparison = "the file matches the table"
        | not (null ev.branchChanges) = "this branch's changes already cover the file"
        | otherwise = let base = opts.base in [i|the file has no schema changes since #{base}|]
    columnsWhere p = [(c, def.colType) | (k, ch) <- Map.toList edit.changes, k.aspect == Definition, p ch, Column _ c <- [k.object], Just def <- [asColumn (changeText ch)]]
    changeText = \case Added d -> d; Dropped d -> d; Changed _ d -> d
    renameLike =
        [ (old, new)
        | null (renamesOf migrate.comparison)
        , migrate.sameTypeSwap == AskIfRename
        , (old, oldType) <- columnsWhere (\case Dropped _ -> True; _ -> False)
        , (new, newType) <- columnsWhere (\case Added _ -> True; _ -> False)
        , oldType == newType
        ]
    renameNotes =
        [ [i|#{old} is dropped and #{new} added with the same type; pass --rename #{old}:#{new} if it is a rename, or --drop-and-add if it is not|]
        | (oldName, newName) <- renameLike
        , let old = nameText oldName
              new = nameText newName
        ]
    dropped addr = case Map.lookup (definitionKey addr) edit.changes of
        Just (Dropped _) -> True
        _ -> False
    -- What goes when a dropped column goes: its indexes and its table's constraints.
    goesWith = \case
        Index _ -> True
        TableConstraint t _ -> t == table
        _ -> False
    listed = fold dependents <> fold mentions
    -- Routines are listed by name, with their arguments only when the name is overloaded.
    overloaded = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(withoutArgs a, 1) | a <- Set.toList listed]))
    withoutArgs = \case
        Routine kind q _ -> Routine kind q []
        a -> a
    short a =
        let addr = toAddress a
            objectType = addr.objectType
            kind = if isRoutine a then "" else [i|#{objectType} |] :: Text
            name = T.intercalate "." addr.objectNames
            args = if withoutArgs a `Set.member` overloaded then let list = T.intercalate ", " addr.objectArgs in [i|(#{list})|] else "" :: Text
         in [i|#{kind}#{name}#{args}|] :: Text
    item d = [i|    #{short d}|] :: Text
    detail t = [i|      #{t}|] :: Text
    section :: Text -> [ObjectId] -> [Text]
    section heading = \case
        [] -> []
        ds -> [i|  #{heading}|] : map item ds
    breaksSection = \case
        [] -> []
        fs -> "  breaks" : concat [[item f, detail (Map.findWithDefault "" f breaking)] | f <- fs]
    foreignKeySection = \case
        [] -> []
        ks -> "  cannot be rebuilt with the new type" : concat [[item k, detail err] | (k, err) <- ks]
    -- A trigger on a view that is created again comes back with it.
    recreated addr d =
        d `Set.member` aroundOf addr || case d of
            Trigger t _ -> any (\v -> isView v && isRelation v == Just t) (aroundOf addr)
            _ -> False
    rebuiltWith = \case
        Index _ -> True
        TableConstraint{} -> True
        Sequence _ -> True
        _ -> False
    usage =
        concat
            [ T.intercalate "." (toAddress addr).objectNames
                : section (if addr `Set.member` ev.renamedFrom then "used by, and follows the rename" else "used by") users
                    <> section "dropped with it" with
                    <> section "dropped and created again around it" again
                    <> breaksSection (breaksFor addr)
                    <> foreignKeySection (foreignKeysFor addr)
                    <> section "named in" (Set.toList (Map.findWithDefault Set.empty addr mentions))
            | addr <- Set.toList (Map.keysSet dependents <> Map.keysSet deps.named)
            , not (null (Map.findWithDefault Set.empty addr dependents)) || not (null (breaksFor addr)) || not (null (Map.findWithDefault Set.empty addr mentions))
            , let (with, rest) = L.partition (\d -> dropped addr && goesWith d) (Set.toList (Map.findWithDefault Set.empty addr dependents))
                  (again, users) = L.partition (recreated addr) (filter (`Map.notMember` breaking) rest)
            ]
            <> renameNotes
    blocking =
        Set.toList
            . Set.fromList
            $ [d | (addr, ds) <- Map.toList dependents, dropped addr, d <- Set.toList ds, not (goesWith d)]
            <> [d | (addr, ds) <- Map.toList dependents, addr `Set.member` retypedAddresses, d <- Set.toList ds, not (recreated addr d), not (rebuiltWith d), d `Set.notMember` passed]
            <> Map.keys breaking
            <> [k | ks <- Map.elems ev.foreignKeyBreaks, (k, _) <- ks]


-- | For a foreign key on @table@, each of its columns' types paired with the referenced
-- column's, by @typeIn table column@.
foreignKeyTypes :: (QName -> Identifier -> Maybe Text) -> QName -> ConstraintDef -> Maybe [(Text, Text)]
foreignKeyTypes typeIn table def = do
    target <- def.references
    guard (length def.columns == length target.columns && not (null def.columns))
    for (zip def.columns target.columns) \(a, b) -> (,) <$> typeIn table a <*> typeIn target.table b
