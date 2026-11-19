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
    RenameCandidate (..),
    findingsFor,
    foreignKeyTypes,
) where

import Data.String.Interpolate (i)
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.App.Dependents (Dependents (..), catalogDependentsOf, routinesNaming)
import Sqiff.App.Options (Comparison (..), MigrateOptions (..), SameTypeSwap (..), TableOptions (..), renamesOf)
import Sqiff.App.Table.Env
import Sqiff.Config (Config (..))
import Sqiff.Definition hiding (DomainType)
import Sqiff.Dependencies (noDependencies)
import Sqiff.Diff (Change (..))
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (DependencyCycle, planWith)
import Sqiff.Recipe (Context (viewsAround), Hazard (..), Phase (..), Step (..), Txn (..), Undo (..))
import Sqiff.Recreate (Recreation (..), viewsAroundTypeChanges)
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
    , usage :: [Text]
    -- ^ The report on what uses the edited objects, line by line.
    , blocking :: [ObjectId]
    -- ^ What the migration would break.
    , renameLike :: [RenameCandidate]
    , same :: Text
    -- ^ What to say when there is nothing to migrate.
    , withViews :: Context -> Context
    }


-- | A dropped column and an added one of the same type, which may be a rename.
data RenameCandidate = RenameCandidate
    { droppedColumn :: Identifier
    , addedColumn :: Identifier
    }


retypedColumns :: TableEdit -> [Retyped]
retypedColumns edit =
    [ Retyped{table = t, column = c, newType = n.colType}
    | (k, Changed old new) <- Map.toList edit.changes
    , k.aspect == Definition
    , ColumnDefinition o <- [old]
    , ColumnDefinition n <- [new]
    , columnTypeChanged o n
    , Column t c <- [k.object]
    ]


findingsFor :: TableEnv -> MigrateOptions -> Evidence -> Either DependencyCycle Findings
findingsFor env migrate ev = do
    planned <- planWith withViews Map.empty ev.edit.current ev.edit.desired noDependencies noDependencies ev.edit.changes
    pure
        Findings
            { steps = renames <> map (refineHazards env.table ev) planned
            , usage = usageReport effects ev <> map renameNote candidates
            , blocking = Set.toList (blockers effects ev)
            , renameLike = candidates
            , same = nothingToMigrate env migrate ev
            , withViews
            }
  where
    effects = effectsOf env.table ev
    renames = map (renameStep env ev.edit) ev.pendingRenames
    candidates = renameCandidates migrate ev.edit
    withViews c = c{viewsAround = Map.map (.statements) effects.recreations}


-- | What the edit does to the objects it changes, which decides what happens to what
-- depends on them.
data Effects = Effects
    { table :: QName
    , dropped :: Set ObjectId
    , recreations :: Map (QName, Identifier) Recreation
    -- ^ What goes and comes back around each retyped column.
    , recreatedAround :: Map ObjectId (Set ObjectId)
    -- ^ For each retyped column, the objects that go and come back around it.
    }


effectsOf :: QName -> Evidence -> Effects
effectsOf table ev =
    Effects
        { table
        , dropped = Set.fromList [k.object | (k, Dropped _) <- Map.toList ev.edit.changes, k.aspect == Definition]
        , recreations
        , recreatedAround =
            Map.fromListWith
                (<>)
                [ (retypedObject r, maybe Set.empty (.members) (Map.lookup (r.table, r.column) recreations))
                | r <- ev.retyped
                ]
        }
  where
    recreations =
        viewsAroundTypeChanges ev.deps.exact ev.deps.dbSchema ev.deps.dbDeps [(r.table, r.column, retypedObject r) | r <- ev.retyped]


-- | Whether @dependent@ is dropped with a dropped column of @table@: its indexes and its
-- table's constraints.
goesWithColumn :: QName -> ObjectId -> Bool
goesWithColumn table = \case
    Index _ -> True
    TableConstraint t _ -> t == table
    _ -> False


-- | Whether @dependent@ of @column@ is dropped and created again around its type change.
-- A trigger on a view that is created again comes back with it.
recreatedWith :: Effects -> ObjectId -> ObjectId -> Bool
recreatedWith effects column dependent =
    dependent `Set.member` around || case dependent of
        Trigger t _ -> any (\v -> isView v && isRelation v == Just t) around
        _ -> False
  where
    around = Map.findWithDefault Set.empty column effects.recreatedAround


-- | Whether PostgreSQL rebuilds @dependent@ itself when a column it uses changes type.
rebuiltWithColumn :: ObjectId -> Bool
rebuiltWithColumn = \case
    Index _ -> True
    TableConstraint{} -> True
    Sequence _ -> True
    _ -> False


-- | What the migration would break: what depends on a dropped column and does not go
-- with it, what depends on a retyped column and is neither created again, rebuilt nor
-- checked, the routines whose bodies break, and the foreign keys that cannot be rebuilt.
blockers :: Effects -> Evidence -> Set ObjectId
blockers effects ev = onDropped <> onRetyped <> Map.keysSet ev.bodies.breaking <> unbuildableForeignKeys
  where
    onDropped =
        Set.fromList
            [ d
            | (object, ds) <- Map.toList ev.deps.exact
            , object `Set.member` effects.dropped
            , d <- Set.toList ds
            , not (goesWithColumn effects.table d)
            ]
    onRetyped =
        Set.fromList
            [ d
            | (object, ds) <- Map.toList ev.deps.exact
            , object `Map.member` effects.recreatedAround
            , d <- Set.toList ds
            , not (recreatedWith effects object d)
            , not (rebuiltWithColumn d)
            , d `Set.notMember` ev.bodies.passed
            ]
    unbuildableForeignKeys = Set.fromList [k | ks <- Map.elems ev.foreignKeyBreaks, (k, _) <- ks]


-- | A retyping step's hazards, made exact by what PostgreSQL says the type change does:
-- whether it rewrites the table, and so whether a value can fail to convert, and the
-- other tables its foreign keys lock and check again.
refineHazards :: QName -> Evidence -> Step -> Step
refineHazards table ev step = case Map.lookup step.object ev.typeFacts of
    Just (TypeChange rewrite target)
        | MayRewriteTable `elem` step.hazards ->
            let rewrites = rewrite == RewritesFiles
                exact = \case
                    MayRewriteTable -> [RewritesTable | rewrites]
                    ConversionMayFail -> [ConversionMayFail | rewrites || target == DomainType]
                    h -> [h]
                onOther other = LocksOther other : [RechecksForeignKey other | rewrites]
             in step{hazards = concatMap exact step.hazards <> concatMap onOther (foreignKeyPartners table ev.deps step.object)}
    _ -> step


-- | The other tables that foreign keys depending on @column@ tie to @table@, as SQL
-- names them, in order.
foreignKeyPartners :: QName -> Dependents -> ObjectId -> [Text]
foreignKeyPartners table deps column =
    Set.toList
        . Set.fromList
        $ [ qualified other
          | k@(TableConstraint onTable _) <- Set.toList (catalogDependentsOf column deps)
          , Just (ConstraintDefinition def) <- [Map.lookup (definitionKey k) deps.dbSchema]
          , Just target <- [def.references]
          , let other = if onTable == table then target.table else onTable
          , other /= table
          ]


-- | The step that renames a column from @old@ to @new@ before anything else, undone by
-- renaming it back.
renameStep :: TableEnv -> TableEdit -> (Identifier, Identifier) -> Step
renameStep env edit (old, new) =
    Step
        { phase = DropPhase
        , rank = -1
        , object = renamed
        , sql = renameSql env.table (old, new)
        , txn = Shared
        , hazards = [MayFail ("breaks running code that still uses the old name" <> foldMap ("; " <>) env.config.renameHint)]
        , key = definitionKey renamed
        , change = Changed def def
        , undo =
            UndoBy
                [ Step
                    { phase = DropPhase
                    , rank = -1
                    , object = Column env.table old
                    , sql = renameSql env.table (new, old)
                    , txn = Shared
                    , hazards = []
                    , key = definitionKey renamed
                    , change = Changed def def
                    , undo = UndoObject
                    }
                ]
        }
  where
    renamed = Column env.table new
    def = Map.findWithDefault (Printed "") (definitionKey renamed) edit.desired


-- | The dropped and added columns of the same type, unless renames were given or the
-- user said such a pair is a drop and an add.
renameCandidates :: MigrateOptions -> TableEdit -> [RenameCandidate]
renameCandidates migrate edit =
    [ RenameCandidate{droppedColumn, addedColumn}
    | null (renamesOf migrate.comparison)
    , migrate.sameTypeSwap == AskIfRename
    , (droppedColumn, oldType) <- columnsWhere \case Dropped d -> Just d; _ -> Nothing
    , (addedColumn, newType) <- columnsWhere \case Added d -> Just d; _ -> Nothing
    , oldType == newType
    ]
  where
    columnsWhere side =
        [ (c, def.colType)
        | (k, change) <- Map.toList edit.changes
        , k.aspect == Definition
        , Column _ c <- [k.object]
        , Just def <- [asColumn =<< side change]
        ]


renameNote :: RenameCandidate -> Text
renameNote candidate =
    let old = nameText candidate.droppedColumn
        new = nameText candidate.addedColumn
     in [i|#{old} is dropped and #{new} added with the same type; pass --rename #{old}:#{new} if it is a rename, or --drop-and-add if it is not|]


-- | What to say when the file needs no migration.
nothingToMigrate :: TableEnv -> MigrateOptions -> Evidence -> Text
nothingToMigrate env migrate ev
    | WithLiveTable _ <- migrate.comparison = "the file matches the table"
    | not (null ev.branchChanges) = "this branch's changes already cover the file"
    | otherwise = let base = env.opts.base in [i|the file has no schema changes since #{base}|]


-- | What happens to the objects that use one the edit changes.
data Usage = Usage
    { object :: ObjectId
    , followsRename :: Bool
    -- ^ The object is a column being renamed, which its users follow.
    , usedBy :: [ObjectId]
    , droppedWith :: [ObjectId]
    , recreatedAround :: [ObjectId]
    , breaks :: [(ObjectId, Text)]
    -- ^ Routines whose bodies break, with PostgreSQL's error.
    , unbuildableForeignKeys :: [(ObjectId, Text)]
    -- ^ Foreign keys that cannot be rebuilt with the new type, with PostgreSQL's error.
    , namedIn :: [ObjectId]
    -- ^ Routines that name the object in a body no check covered.
    }


usageOf :: Effects -> Evidence -> ObjectId -> Usage
usageOf effects ev object =
    Usage
        { object
        , followsRename = object `Set.member` ev.renamedFrom
        , usedBy
        , droppedWith
        , recreatedAround
        , breaks = [(d, why) | d <- Set.toList (routinesNaming object ev.deps <> dependents), Just why <- [Map.lookup d ev.bodies.breaking]]
        , unbuildableForeignKeys = Map.findWithDefault [] object ev.foreignKeyBreaks
        , namedIn = Set.toList (uncheckedNaming ev object)
        }
  where
    dependents = catalogDependentsOf object ev.deps
    isDropped = object `Set.member` effects.dropped
    (droppedWith, rest) = L.partition (\d -> isDropped && goesWithColumn effects.table d) (Set.toList dependents)
    (recreatedAround, usedBy) = L.partition (recreatedWith effects object) (filter (`Map.notMember` ev.bodies.breaking) rest)


-- | The routines that name @object@ and that the body check neither found broken nor
-- passed.
uncheckedNaming :: Evidence -> ObjectId -> Set ObjectId
uncheckedNaming ev object = Set.filter unchecked (routinesNaming object ev.deps)
  where
    unchecked r = r `Map.notMember` ev.bodies.breaking && r `Set.notMember` ev.bodies.passed


-- | An object is reported when something uses it, breaks or names it; foreign keys that
-- cannot be rebuilt show only along with those.
reported :: Usage -> Bool
reported u = not (null u.usedBy && null u.droppedWith && null u.recreatedAround && null u.breaks && null u.namedIn)


-- | The usage of every object the edit changes, as report lines.
usageReport :: Effects -> Evidence -> [Text]
usageReport effects ev =
    concatMap (renderUsage (shortName listed))
        . filter reported
        . map (usageOf effects ev)
        $ Set.toList (Map.keysSet ev.deps.exact <> Map.keysSet ev.deps.named)
  where
    listed = fold ev.deps.exact <> foldMap (uncheckedNaming ev) (Map.keys ev.deps.named)


renderUsage :: (ObjectId -> Text) -> Usage -> [Text]
renderUsage name u =
    T.intercalate "." (toAddress u.object).objectNames
        : objects (if u.followsRename then "used by, and follows the rename" else "used by") u.usedBy
            <> objects "dropped with it" u.droppedWith
            <> objects "dropped and created again around it" u.recreatedAround
            <> explained "breaks" u.breaks
            <> explained "cannot be rebuilt with the new type" u.unbuildableForeignKeys
            <> objects "named in" u.namedIn
  where
    objects :: Text -> [ObjectId] -> [Text]
    objects heading = \case
        [] -> []
        ds -> ("  " <> heading) : map item ds
    explained :: Text -> [(ObjectId, Text)] -> [Text]
    explained heading = \case
        [] -> []
        ds -> ("  " <> heading) : concat [[item d, "      " <> why] | (d, why) <- ds]
    item d = "    " <> name d


-- | An object's name in the report: a routine by its name, with its arguments only when
-- one of @listed@ shares the name, and anything else by its kind and name.
shortName :: Set ObjectId -> ObjectId -> Text
shortName listed object = kind <> T.intercalate "." address.objectNames <> args
  where
    address = toAddress object
    kind = if isRoutine object then "" else address.objectType <> " "
    args
        | withoutArgs object `Set.member` overloaded = "(" <> T.intercalate ", " address.objectArgs <> ")"
        | otherwise = ""
    overloaded = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(withoutArgs a, 1) | a <- Set.toList listed]))
    withoutArgs = \case
        Routine k q _ -> Routine k q []
        a -> a


-- | For a foreign key on @table@, each of its columns' types paired with the referenced
-- column's, by @typeIn table column@.
foreignKeyTypes :: (QName -> Identifier -> Maybe Text) -> QName -> ConstraintDef -> Maybe [(Text, Text)]
foreignKeyTypes typeIn table def = do
    target <- def.references
    guard (length def.columns == length target.columns && not (null def.columns))
    for (zip def.columns target.columns) \(a, b) -> (,) <$> typeIn table a <*> typeIn target.table b
