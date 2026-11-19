{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | One fixed recipe per kind of object and kind of change.
module Sqiff.Recipe (
    Phase (..),
    Txn (..),
    Hazard (..),
    hazardText,
    revokeGranted,
    Step (..),
    Role (..),
    defines,
    Undo (..),
    Context (..),
    Surrounding (..),
    KeptOnView (..),
    columnSql,
    recipe,
) where

import Data.ByteString qualified as B
import Data.Char (isAlphaNum)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Body (BodyCheck (..))
import Sqiff.Definition
import Sqiff.Diff (Change (..))
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Sql (doBlock, literal, replaceInCode)


data Phase = DropPhase | CreatePhase
    deriving stock (Eq, Ord, Show)


-- | @Own@ steps must commit apart from what came before them; @None@ steps cannot run
-- inside a transaction at all.
data Txn = Shared | Own | None
    deriving stock (Eq, Ord, Show)


data Hazard
    = DeletesData
    | MayRewriteTable
    | RewritesTable
    | -- | ACCESS EXCLUSIVE for the whole statement.
      LongLock
    | WriteLock
    | LocksOther Text
    | RechecksForeignKey Text
    | EveryPartition
    | IndexBuild
    | -- | An existing value may not convert to the column's new type.
      ConversionMayFail
    | MayFail Text
    | Manual Text
    deriving stock (Eq, Ord, Show)


hazardText :: Hazard -> Text
hazardText = \case
    DeletesData -> "hazard: deletes data"
    MayRewriteTable -> "hazard: may rewrite the table"
    RewritesTable -> "hazard: rewrites the table and its indexes"
    LongLock -> "hazard: holds an ACCESS EXCLUSIVE lock, which blocks reads and writes, while it runs"
    WriteLock -> "hazard: holds a lock that blocks writes while it runs"
    LocksOther t -> [i|hazard: also locks #{t} while it runs|]
    RechecksForeignKey t -> [i|hazard: checks the foreign key between the table and #{t} again, reading every row|]
    EveryPartition -> "hazard: the lock and any rewrite cover every partition"
    IndexBuild -> "hazard: builds an index"
    ConversionMayFail -> "hazard: fails when an existing value does not convert to the new type"
    MayFail why -> "hazard: " <> why
    Manual why -> "manual: " <> why


-- | Takes back from a relation every grant but its owner's, such as the ones default
-- privileges give a view created again, before its own grants are restored.
revokeGranted :: Text -> Text
revokeGranted relation =
    doBlock [i| declare g text; begin for g in select distinct case when x.grantee = 0 then 'public' else quote_ident(pg_get_userbyid(x.grantee)) end from pg_class c cross join aclexplode(c.relacl) x where c.oid = #{literal relation}::regclass and x.grantee <> c.relowner loop execute format('revoke all on table %s from %s', #{literal relation}, g); end loop; end |]


data Step = Step
    { phase :: Phase
    , role :: Role
    , rank :: Int
    -- ^ Orders steps within a phase; dependency order breaks ties.
    , object :: ObjectId
    , sql :: Text
    , txn :: Txn
    , hazards :: [Hazard]
    , key :: ObjectKey
    , change :: Change Definition
    , undo :: Undo
    -- ^ How a migration that holds this step reverts it.
    }
    deriving stock (Eq, Show)


-- | What a step does to its object. Planning and the sqitch output go by it, rather
-- than by a step's rank or SQL.
data Role
    = -- | Creates the object, or changes it in place.
      Creates
    | -- | Creates or replaces the object in one statement.
      Replaces
    | -- | Builds an index the object needs, concurrently.
      BuildsIndex
    | -- | Drops the object: for good, or in the create phase to create it again.
      Drops
    | -- | Renames a column, before anything else runs.
      RenamesColumn
    | -- | Sets the column that owns a sequence, once the column exists.
      OwnsSequence
    | -- | Sets something on an object that exists: a table's settings, grants, a comment.
      Configures
    deriving stock (Eq, Show)


-- | Whether the step creates, replaces or drops the object itself, rather than renaming
-- it or setting what goes with it.
defines :: Step -> Bool
defines s = s.role `elem` [Creates, Replaces, BuildsIndex, Drops]


-- | How a step is reverted when its object's change is split over several migrations,
-- so that reverting one migration returns to the state after the one before it.
data Undo
    = -- | By the object's inverse: as a whole when its migration holds all the change, else
      -- a drop by creating the old object again and a create by dropping the new one.
      UndoObject
    | -- | By these steps, in order.
      UndoBy [Step]
    deriving stock (Eq, Show)


-- | What has to go before a statement and come back after it: the statements that drop
-- the objects in the way, and the ones that create them again.
data Surrounding = Surrounding
    { dropBefore :: [Text]
    , createAfter :: [Text]
    }
    deriving stock (Eq, Show)


-- | What a view dropped and created again gets back: its indexes (name and definition)
-- and its privileges.
data KeptOnView = KeptOnView
    { keptIndexes :: [(QName, Text)]
    , keptGrants :: Maybe (ObjectKey, Definition)
    }


data Context = Context
    { newTables :: Set QName
    , droppedTables :: Set QName
    , partitionedTables :: Set QName
    , droppedColumns :: Set (QName, Identifier)
    , retypedColumns :: Set (QName, Identifier)
    , newTableColumns :: Map QName [(Identifier, ColumnDef)]
    , bodyChecks :: Map ObjectKey (Text, BodyCheck)
    -- ^ Body checks, with the definition each was made for.
    , withDependents :: Set ObjectId
    -- ^ Objects that something else depends on, in the database being changed.
    , recreatedViews :: Map QName KeptOnView
    -- ^ Views dropped and created again, with what to restore on them.
    , keptComments :: Map ObjectId Text
    -- ^ The desired database's comments that do not change, which an object dropped and
    -- created again gets back.
    , viewsAround :: Map (QName, Identifier) Surrounding
    -- ^ For a column whose type changes, the statements that drop the views reading it
    -- and the ones that create them again, with their grants, indexes, triggers and comments.
    , droppedObjects :: Set ObjectId
    , viewDependents :: Map QName Surrounding
    -- ^ For a view whose columns change other than at the end, the statements that drop
    -- what depends on it and the ones that create those again.
    , convertedSequences :: Set QName
    -- ^ Sequences a column's change between serial and identity drops or creates, in
    -- the one step that changes the column.
    }


recipe :: Context -> ObjectKey -> Change Definition -> [Step]
recipe ctx key change = case key.aspect of
    Privileges -> privileges key object change
    Comment -> comment ctx key change
    Definition -> definition Target{context = ctx, objectKey = key, objectChange = change}
  where
    object = key.object


-- Ranks order steps within a phase; dependency order breaks ties.
createRank :: ObjectId -> Int
createRank = \case
    Schema _ -> 0
    Extension _ -> 1
    Type _ -> 2
    Sequence _ -> 3
    Table _ -> 4
    Column _ _ -> 5
    View _ -> 7
    MaterializedView _ -> 8
    -- After views: PostgreSQL records what a view uses, so a view on a new function waits
    -- for it, but not what a function's string body uses, which may be a new view.
    Routine{} -> 9
    TableConstraint _ _ -> 10
    DomainConstraint _ _ -> 10
    Index _ -> 11
    Trigger _ _ -> 12
    Policy _ _ -> 13
    OtherColumn{} -> 20
    Unknown _ -> 20


dropRank :: ObjectId -> Int
dropRank = \case
    Policy _ _ -> 0
    Trigger _ _ -> 1
    Index _ -> 2
    TableConstraint _ _ -> 3
    DomainConstraint _ _ -> 3
    MaterializedView _ -> 4
    View _ -> 5
    Routine{} -> 6
    Column _ _ -> 7
    Table _ -> 8
    Sequence _ -> 9
    Type _ -> 10
    Extension _ -> 11
    Schema _ -> 12
    OtherColumn{} -> 20
    Unknown _ -> 20


sequenceOwnerRank, tableSettingsRank, privilegesRank :: Int
sequenceOwnerRank = 9
tableSettingsRank = 14
privilegesRank = 15


definition :: Target -> [Step]
definition this = case this.objectKey.object of
    Schema s -> case textual this of
        Added _ -> [create this (Shared, []) ("create schema " <> quoted s)]
        Dropped _ -> [remove this (Shared, []) ("drop schema " <> quoted s)]
        Changed _ _ -> []
    Extension e -> extensionRecipe this e
    Table t -> tableRecipe this t
    Column t c
        | t `Set.member` this.context.droppedTables -> []
        | t `Set.member` this.context.partitionedTables -> map onEveryPartition (columnRecipe this t c)
        | otherwise -> columnRecipe this t c
    TableConstraint t c
        | t `Set.member` this.context.droppedTables -> []
        | otherwise -> tableConstraintRecipe this t c
    DomainConstraint d c -> domainConstraintRecipe this d c
    Index ix -> indexRecipe this ix
    Sequence s -> sequenceRecipe this s
    Type t -> typeRecipe this t
    Routine kind q args -> routineRecipe this kind q args
    View v -> viewRecipe this v
    MaterializedView v -> materializedViewRecipe this v
    Trigger t n
        | t `Set.member` this.context.droppedTables -> []
        | otherwise -> triggerRecipe this t n
    Policy t n
        | t `Set.member` this.context.droppedTables -> []
        | otherwise -> policyRecipe this t n
    OtherColumn{} -> noRecipe this
    Unknown _ -> noRecipe this


-- | The object a recipe is for, and the change to it.
data Target = Target
    { context :: Context
    , objectKey :: ObjectKey
    , objectChange :: Change Definition
    }


-- | The definitions as the catalog prints them, for the recipes that read the text.
textual :: Target -> Change Text
textual this = printed <$> this.objectChange


-- | A step of the create phase that creates the object or changes it, at its own rank.
create :: Target -> (Txn, [Hazard]) -> Text -> Step
create this = stepOf this CreatePhase Creates (createRank this.objectKey.object)


-- | A step that replaces the object in one statement.
replace :: Target -> (Txn, [Hazard]) -> Text -> Step
replace this = stepOf this CreatePhase Replaces (createRank this.objectKey.object)


-- | A step that builds an index the object needs, concurrently.
buildIndex :: Target -> (Txn, [Hazard]) -> Text -> Step
buildIndex this = stepOf this CreatePhase BuildsIndex (createRank this.objectKey.object)


-- | A step of the drop phase that drops the object.
remove :: Target -> (Txn, [Hazard]) -> Text -> Step
remove this = stepOf this DropPhase Drops (dropRank this.objectKey.object)


-- | A step of the create phase that drops the object, to create it again right after.
dropToRecreate :: Target -> (Txn, [Hazard]) -> Text -> Step
dropToRecreate this = stepOf this CreatePhase Drops (createRank this.objectKey.object)


-- | A step that sets something on the object once it exists, at @rank@.
configure :: Target -> Int -> (Txn, [Hazard]) -> Text -> Step
configure this = stepOf this CreatePhase Configures


-- | A step that sets the column owning a sequence.
ownSequence :: Target -> (Txn, [Hazard]) -> Text -> Step
ownSequence this = stepOf this CreatePhase OwnsSequence sequenceOwnerRank


stepOf :: Target -> Phase -> Role -> Int -> (Txn, [Hazard]) -> Text -> Step
stepOf this phase role rank (txn, hazards) sql =
    Step
        { phase
        , role
        , rank
        , object = this.objectKey.object
        , sql
        , txn
        , hazards
        , key = this.objectKey
        , change = this.objectChange
        , undo = UndoObject
        }


-- | A step left to a person, saying why.
manual :: Target -> Text -> [Step]
manual this why = [create this (Shared, [Manual why]) ("-- " <> why)]


noRecipe :: Target -> [Step]
noRecipe this = [create this (Shared, [Manual "no recipe for this kind of object"]) ("-- " <> T.pack (show (textual this)))]


undoneBy :: [Step] -> Step -> Step
undoneBy steps s = s{undo = UndoBy steps}


-- | A step another recipe makes, as part of this one: what restores an object created
-- again goes with that, so its migration leaves the object whole.
asPartOfThis :: Target -> Step -> Step
asPartOfThis this s = s{rank = createRank this.objectKey.object, key = this.objectKey, change = this.objectChange, undo = UndoBy []}


-- | A concurrent build is reverted by dropping what it built.
builds :: Target -> QName -> Step -> Step
builds this index = undoneBy [create this (None, []) ("drop index concurrently if exists " <> qualified index)]


-- | The unchanged comments of what this step drops and creates again: they go with the
-- old object, and nothing else in the plan sets them.
restoreComments :: Target -> [ObjectId] -> [Step]
restoreComments this objects =
    [ undoneBy [] (configure this (createRank this.objectKey.object) (Shared, []) ("comment on " <> subject <> " is " <> literal text))
    | o <- objects
    , Just text <- [Map.lookup o this.context.keptComments]
    , Just subject <- [commentTarget o]
    ]


-- | The change as the kind of definition this recipe reads; a definition of another
-- kind is the catalog's mistake, left to a person. The steps keep the change as it was.
typed :: Target -> (Definition -> Maybe a) -> (Change a -> [Step]) -> [Step]
typed this as f = maybe (manual this "the catalog described this object unexpectedly") f (traverse as this.objectChange)


notValid :: Text
notValid = " NOT VALID"


-- | Whether @new@ is @old@ validated.
validates :: ConstraintDef -> ConstraintDef -> Bool
validates old new = old.validity == NotValidated && new.validity == Validated && withoutValidity old == new


-- | A step on a partitioned table's column, whose lock and rewrite cover every partition.
onEveryPartition :: Step -> Step
onEveryPartition s
    | any (`elem` [DeletesData, MayRewriteTable, LongLock]) s.hazards = s{hazards = s.hazards <> [EveryPartition]}
    | otherwise = s


-- | Whether @def@ names a column of @t@ that the plan drops or retypes.
namesAlteredColumn :: Target -> QName -> Text -> Bool
namesAlteredColumn this t = namesAlteredColumnOf this (== t)


namesAlteredColumnOf :: Target -> (QName -> Bool) -> Text -> Bool
namesAlteredColumnOf this onTable def =
    let names = Set.fromList (T.split (\ch -> not (isAlphaNum ch || ch == '_')) def)
     in any (\(t', c) -> onTable t' && nameText c `Set.member` names) (this.context.droppedColumns <> this.context.retypedColumns)


extensionRecipe :: Target -> Identifier -> [Step]
extensionRecipe this e = typed this asExtension \case
    Added def -> [create this (Shared, []) ("create extension if not exists " <> quoted e <> " with schema " <> quoted def.schema <> " version " <> literal def.version)]
    Dropped _ -> [remove this (Shared, []) ("drop extension " <> quoted e)]
    Changed old new
        | old.schema /= new.schema -> manual this "extension moved to another schema"
        | otherwise -> [create this (Own, []) ("alter extension " <> quoted e <> " update to " <> literal new.version)]


tableRecipe :: Target -> QName -> [Step]
tableRecipe this t = typed this asTable \case
    Added def -> createTable this t def
    Dropped _ -> [remove this (Shared, [DeletesData]) ("drop table " <> qualified t)]
    Changed old new -> alterTable this t old new


createTable :: Target -> QName -> TableDef -> [Step]
createTable this t def
    | Just p <- def.partitionOf =
        create this (Shared, []) ("create table " <> qualified t <> " partition of " <> qualified p.parent <> " " <> p.bound) : settings
    | otherwise =
        create
            this
            (Shared, [])
            ( (if def.unlogged then "create unlogged table " else "create table ")
                <> qualified t
                <> " ("
                <> columnList
                <> ")"
                <> (if null def.inherits then "" else " inherits (" <> T.intercalate ", " (map qualified def.inherits) <> ")")
                <> maybe "" (" partition by " <>) def.partitionBy
                <> maybe "" (" " <>) (withOptions def.options)
            )
            : settings
  where
    -- Columns a parent gives it are not declared again.
    columnList = case [col | col@(_, d) <- Map.findWithDefault [] t this.context.newTableColumns, not d.inheritedOnly] of
        [] -> ""
        cols -> [i|\n#{T.intercalate ",\n" (map columnLine cols)}\n|]
    columnLine (c, col) = [i|    #{quoted c} #{columnSql col}|]
    settings =
        [configure this tableSettingsRank (Shared, []) [i|alter table #{qualified t} enable row level security|] | def.rowSecurity]
            <> [configure this tableSettingsRank (Shared, []) [i|alter table #{qualified t} force row level security|] | def.forceRowSecurity]


alterTable :: Target -> QName -> TableDef -> TableDef -> [Step]
alterTable this t old new
    | (old.partitionBy, old.partitionOf) /= (new.partitionBy, new.partitionOf) = manual this "partitioning changed"
    | otherwise =
        toggle (.unlogged) ("set unlogged", "set logged") [MayRewriteTable, LongLock]
            <> toggle (.rowSecurity) ("enable row level security", "disable row level security") []
            <> toggle (.forceRowSecurity) ("force row level security", "no force row level security") []
            <> storageOptions
            <> [create this (Shared, []) [i|alter table #{qualified t} no inherit #{qualified p}|] | p <- old.inherits, p `notElem` new.inherits]
            <> [create this (Shared, []) [i|alter table #{qualified t} inherit #{qualified p}|] | p <- new.inherits, p `notElem` old.inherits]
  where
    toggle setting (enable, disable) hazards
        | setting old == setting new = []
        | setting new = [configure this tableSettingsRank (Shared, hazards) ("alter table " <> qualified t <> " " <> enable)]
        | otherwise = [configure this tableSettingsRank (Shared, hazards) ("alter table " <> qualified t <> " " <> disable)]
    optionKey = T.takeWhile (/= '=')
    removedKeys = [k | o <- old.options, let k = optionKey o, k `notElem` map optionKey new.options]
    newOptions = T.intercalate ", " new.options
    storageOptions =
        [ configure this tableSettingsRank (Shared, []) [i|alter table #{qualified t} set (#{newOptions})|]
        | not (null new.options)
        , L.sort new.options /= L.sort old.options
        ]
            <> [ configure this tableSettingsRank (Shared, []) [i|alter table #{qualified t} reset (#{T.intercalate ", " removedKeys})|]
               | not (null removedKeys)
               ]


columnRecipe :: Target -> QName -> Identifier -> [Step]
columnRecipe this t c = case traverse asColumn this.objectChange of
    Nothing -> manual this "the catalog described the column without its column facts"
    -- The parent's change brings it, changes it or takes it away.
    Just ch | all (.inheritedOnly) ch -> []
    Just (Added parsed)
        | t `Set.member` this.context.newTables -> []
        | otherwise ->
            let rewrites = parsed.rewritesOnAdd || isJust (identityKindOf parsed) || fmap fst (generatedOf parsed) == Just Stored
                hazards =
                    [MayFail "a not null column without a default fails on a table with rows" | failsOnRows parsed]
                        <> [h | rewrites, h <- [MayRewriteTable, LongLock]]
             in [create this (Shared, hazards) (alter <> "add column " <> quoted c <> " " <> columnSql parsed)]
    Just (Dropped _) -> [remove this (Shared, [DeletesData]) (alter <> "drop column " <> quoted c)]
    Just (Changed old new) -> alterColumn old new
  where
    alter = [i|alter table #{qualified t} |]
    col = [i|#{alter}alter column #{quoted c} |]
    alterColumn old new =
        let typeChanged = columnTypeChanged old new
            defaultChanged = defaultExpr old /= defaultExpr new
            serialToIdentity = isNothing (identityKindOf old) && isJust (identityKindOf new) && isJust (serialSequence old)
            identityToSerial = isJust (identityKindOf old) && isNothing (identityKindOf new) && isJust (serialSequence new)
            sequenceMoves = serialToIdentity || identityToSerial
         in [create this (Shared, []) (col <> "drop default") | typeChanged, isJust (defaultExpr old)]
                <> [ create
                        this
                        ( Shared
                        , [MayRewriteTable, LongLock, ConversionMayFail]
                            <> [MayFail "recreating a view fails when its query relies on the old type; then edit the view" | Map.member (t, c) this.context.viewsAround]
                        )
                        (aroundViews (col <> "type " <> typeText new.colType <> maybe "" ((" collate " <>) . collationSql) new.collation <> " using " <> quoted c <> "::" <> typeText new.colType))
                   | typeChanged
                   ]
                <> case defaultExpr new of
                    _ | sequenceMoves -> []
                    Just e | defaultChanged || (typeChanged && isJust (defaultExpr old)) -> [create this (Shared, []) (col <> "set default " <> e)]
                    Nothing | defaultChanged && not typeChanged -> [create this (Shared, []) (col <> "drop default")]
                    _ -> []
                <> notNullSteps old new
                <> case (identityKindOf old, identityKindOf new) of
                    _ | serialToIdentity, Just k <- identityKindOf new -> [create this (Shared, []) (toIdentity (identityKindSql k <> " as identity" <> identityOptionsSql (identityOptionsOf new)))]
                    _ | identityToSerial -> [create this (Shared, []) (toSerial (typeText new.colType))]
                    (Nothing, Just k) -> [create this (Shared, []) (col <> "add generated " <> identityKindSql k <> " as identity" <> identityOptionsSql (identityOptionsOf new))]
                    (Just _, Nothing) -> [create this (Shared, []) (col <> "drop identity")]
                    (Just a, Just b) ->
                        let clauses = ["set generated " <> identityKindSql b | a /= b] <> identityOptionChanges (identityOptionsOf old) (identityOptionsOf new)
                         in [create this (Shared, []) (col <> T.unwords clauses) | not (null clauses)]
                    _ -> []
                <> case (generatedOf old, generatedOf new) of
                    (Just (k, _), Just (k', e))
                        | k == k' && generatedOf old /= generatedOf new ->
                            [create this (Shared, [h | k == Stored, h <- [MayRewriteTable, LongLock]]) [i|#{col}set expression as (#{e})|]]
                    (a, b) | a /= b -> manual this "generated column added or removed"
                    _ -> []
    aroundViews statement = case Map.lookup (t, c) this.context.viewsAround of
        Just around | not (null around.dropBefore) -> T.intercalate ";\n" (around.dropBefore <> [statement] <> around.createAfter)
        _ -> statement
    serialSequence d = defaultExpr d >>= T.stripPrefix "nextval('" >>= T.stripSuffix "'::regclass)"
    tableLit = literal (qualified t)
    columnLit = literal (nameText c)
    serialName = let tableName = nameText t.name in qualified (QName t.schema (identifier [i|#{tableName}_#{nameText c}_seq|]))
    toIdentity k =
        doBlock
            [i|
declare seq text := pg_get_serial_sequence(#{tableLit}, #{columnLit}); last bigint; called boolean; acl aclitem[];
begin
    execute format('select last_value, is_called from %s', seq) into last, called;
    select relacl into acl from pg_class where oid = seq::regclass;
    #{col}drop default;
    execute format('drop sequence %s', seq);
    #{col}add generated #{k};
    perform setval(pg_get_serial_sequence(#{tableLit}, #{columnLit}), last, called);
    #{grantAcl serialOf}
end |]
    toSerial colType =
        doBlock
            [i|
declare last bigint; called boolean; acl aclitem[];
begin
    execute format('select last_value, is_called from %s', pg_get_serial_sequence(#{tableLit}, #{columnLit})) into last, called;
    select relacl into acl from pg_class where oid = pg_get_serial_sequence(#{tableLit}, #{columnLit})::regclass;
    #{col}drop identity;
    create sequence #{serialName} as #{colType} owned by #{qualified t}.#{quoted c};
    perform setval(#{literal serialName}, last, called);
    #{col}set default nextval(#{literal serialName}::regclass);
    #{grantAcl (literal serialName)}
end |]
    serialOf = [i|pg_get_serial_sequence(#{tableLit}, #{columnLit})|] :: Text
    grantAcl :: Text -> Text
    grantAcl target =
        [i|execute coalesce((select string_agg(format('grant %s on sequence %s to %s', x.privilege_type, #{target}, case when x.grantee = 0 then 'public' else quote_ident(pg_get_userbyid(x.grantee)) end), '; ') from aclexplode(acl) x where x.grantee <> x.grantor), 'select');|]
    notNullSteps old new = case (isNotNull old, isNotNull new) of
        (False, True) ->
            create this (Shared, []) [i|#{alter}add #{named new}not null #{quoted c} not valid|]
                : [create this (Own, []) (onNotNull (notNullName new) "validate constraint" "") | notNullValid new]
        (True, False) -> [create this (Shared, []) (col <> "drop not null")]
        (True, True) ->
            [ create this (Shared, []) (onNotNull (notNullName old) "rename constraint" (" to " <> quoted n))
            | notNullName old /= notNullName new
            , let n = fromMaybe (identifier (T.concat [nameText t.name, "_", nameText c, "_not_null"])) (notNullName new)
            ]
                <> case (notNullValid old, notNullValid new) of
                    (False, True) -> [create this (Own, []) (onNotNull (notNullName new) "validate constraint" "")]
                    (True, False) -> manual this "a validated not null constraint cannot be made NOT VALID again"
                    _ -> []
        (False, False) -> []
    named d = maybe "" (\n -> "constraint " <> quoted n <> " ") (notNullName d)
    onNotNull name action rest = case name of
        Just n -> alter <> action <> " " <> quoted n <> rest
        Nothing ->
            let command = literal [i|#{T.replace "%" "%%" (alter <> action)} %I#{T.replace "%" "%%" rest}|]
                tableLiteral = literal (qualified t)
             in doBlock [i| begin execute format(#{command}, (select conname from pg_constraint where conrelid = #{tableLiteral}::regclass and contype = 'n' and conkey = array[(select attnum from pg_attribute where attrelid = #{tableLiteral}::regclass and attname = #{literal (nameText c)})])); end |]


tableConstraintRecipe :: Target -> QName -> Identifier -> [Step]
tableConstraintRecipe this t c = typed this asConstraint \ch -> if all (.inheritedOnly) ch then [] else tableConstraintChange this t c ch


tableConstraintChange :: Target -> QName -> Identifier -> Change ConstraintDef -> [Step]
tableConstraintChange this t c = \case
    Added def -> addConstraint def <> restoreComments this [TableConstraint t c]
    Dropped old -> [dropConstraint old]
    Changed old new
        | validates old new -> [undoneBy [] $ create this (Own, []) (alterT <> "validate constraint " <> quoted c)]
        | validates new old -> manual this "a validated constraint cannot be made NOT VALID again"
        | otherwise -> dropConstraint old : addConstraint new <> restoreComments this [TableConstraint t c, Index (QName t.schema c)]
  where
    alterT = [i|alter table #{qualified t} |]
    dropConstraint old =
        remove
            this
            (Shared, [MayFail "other objects depend on it, such as foreign keys; drop them first" | dependedOn] <> locksOther old)
            (alterT <> "drop constraint " <> quoted c)
    dependedOn = any (`Set.member` this.context.withDependents) [TableConstraint t c, Index (QName t.schema c)]
    locksOther def = [LocksOther (qualified r.table) | Just r <- [def.references], r.table /= t]
    plain hazards def clause = create this (Shared, hazards <> locksOther def) (alterT <> "add constraint " <> quoted c <> " " <> clause)
    addConstraint def
        | t `Set.member` this.context.newTables = [plain [] def def.clause]
        | validatable def && def.validity == NotValidated = [plain [] def def.clause]
        | validatable def =
            [ plain [] def (def.clause <> notValid)
            , undoneBy [] $ create this (Own, []) (alterT <> "validate constraint " <> quoted c)
            ]
        | Just (keyword, cols) <- simpleKey def
        , not (t `Set.member` this.context.partitionedTables) =
            let build = buildIndex this (None, [IndexBuild]) (concurrentBuild (QName t.schema c) [i|create unique index concurrently #{quoted c} on #{qualified t} (#{cols})|])
             in [ builds this (QName t.schema c) build
                , -- Dropping the constraint drops its index, which the build before made.
                  undoneBy [create this (Shared, []) (alterT <> "drop constraint " <> quoted c), build]
                    $ create this (Shared, []) (alterT <> "add constraint " <> quoted c <> " " <> keyword <> " using index " <> quoted c)
                ]
        | otherwise = [plain [LongLock, IndexBuild] def def.clause]
    validatable def = def.kind `elem` [Check, ForeignKey]
    -- A primary key or unique constraint on plain columns and nothing more, which an
    -- index built beforehand can back.
    simpleKey def = do
        keyword <- case def.kind of
            PrimaryKey -> Just "PRIMARY KEY"
            Unique -> Just "UNIQUE"
            _ -> Nothing
        let cols = T.intercalate ", " (map quoted def.columns)
        guard (not (null def.columns) && def.clause == keyword <> " (" <> cols <> ")")
        pure (keyword, cols)


domainConstraintRecipe :: Target -> TypeName -> Identifier -> [Step]
domainConstraintRecipe this d c = typed this asConstraint \case
    Added def -> add def <> restoreComments this [DomainConstraint d c]
    Dropped _ -> [remove this (Shared, []) (alterD <> "drop constraint " <> quoted c)]
    Changed old new
        | validates old new -> [validate]
        | validates new old -> manual this "a validated constraint cannot be made NOT VALID again"
        | otherwise -> remove this (Shared, []) (alterD <> "drop constraint " <> quoted c) : add new <> restoreComments this [DomainConstraint d c]
  where
    alterD = [i|alter domain #{typeText d} |]
    validate = undoneBy [] $ create this (Own, []) (alterD <> "validate constraint " <> quoted c)
    add def
        | def.validity == NotValidated = [create this (Shared, []) (alterD <> "add constraint " <> quoted c <> " " <> def.clause)]
        | otherwise = [create this (Shared, []) (alterD <> "add constraint " <> quoted c <> " " <> def.clause <> notValid), validate]


indexRecipe :: Target -> QName -> [Step]
indexRecipe this ix = typed this asIndex \case
    Added def -> addIndex def <> restoreComments this [Index ix]
    Dropped def
        | def.table `Set.member` this.context.droppedTables -> []
        | def.partitioned -> [remove this (Shared, [LongLock]) ("drop index " <> qualified ix)]
        | otherwise -> [remove this (None, []) ("drop index concurrently " <> qualified ix)]
    Changed old new -> replaceIndex old new <> restoreComments this [Index ix]
  where
    replaceIndex old new
        | new.partitioned || old.partitioned = remove this (Shared, [LongLock]) ("drop index " <> qualified ix) : addIndex new
        | namesAlteredColumnOf this (== old.table) old.statement =
            remove this (None, []) ("drop index concurrently if exists " <> qualified ix) : addIndex new
        | otherwise =
            let tmp = identifier (takeBytes 59 (nameText ix.name) <> "_new")
                oldName = quoted ix.name
             in [ builds this (QName ix.schema tmp)
                    $ buildIndex
                        this
                        (None, [IndexBuild])
                        (concurrentBuild (QName ix.schema tmp) (replaceInCode [i| INDEX #{oldName} ON |] [i| INDEX CONCURRENTLY #{quoted tmp} ON |] new.statement))
                , undoneBy (addIndex old <> restoreComments this [Index ix]) $ dropToRecreate this (None, []) ("drop index concurrently if exists " <> qualified ix)
                , undoneBy [create this (Shared, []) ("alter index " <> qualified ix <> " rename to " <> quoted tmp)]
                    $ create this (Shared, []) ("alter index " <> qualified (QName ix.schema tmp) <> " rename to " <> oldName)
                ]
    addIndex def
        | def.partitioned =
            [ create this (Shared, []) def.statement
            , create
                this
                (Shared, [Manual [i|build the index on each partition with create index concurrently, then alter index #{qualified ix} attach partition each one; it stays invalid until every partition has one|]])
                [i|-- create index concurrently and attach partition for #{qualified ix}|]
            ]
        | def.table `Set.member` this.context.newTables = [create this (Shared, []) def.statement]
        | otherwise = [builds this ix $ buildIndex this (None, [IndexBuild]) (concurrentBuild ix (replaceInCode " INDEX " " INDEX CONCURRENTLY " def.statement))]


sequenceRecipe :: Target -> QName -> [Step]
sequenceRecipe this s = typed this asSequence \case
    -- Made by the column's change to serial; it only needs its options.
    Added def
        | s `Set.member` this.context.convertedSequences ->
            [ownSequence this (Shared, []) ("alter sequence " <> qualified s <> " " <> sequenceOptions def <> " owned by " <> maybe "none" ownerSql def.ownedBy)]
    Added def ->
        create this (Shared, []) ("create sequence " <> qualified s <> " " <> sequenceOptions def)
            : [ownSequence this (Shared, []) ("alter sequence " <> qualified s <> " owned by " <> ownerSql o) | Just o <- [def.ownedBy]]
    Dropped def
        | Just o <- def.ownedBy, ownedByDropped o -> []
        -- Dropped by the column's change to identity.
        | s `Set.member` this.context.convertedSequences -> []
        | otherwise -> [remove this (Shared, []) ("drop sequence " <> qualified s)]
    Changed _ new -> [ownSequence this (Shared, []) ("alter sequence " <> qualified s <> " " <> sequenceOptions new <> " owned by " <> maybe "none" ownerSql new.ownedBy)]
  where
    ownerSql (t, c) = qualified t <> "." <> quoted c
    -- Dropping the column, or its table, drops the sequence with it.
    ownedByDropped (t, c) = t `Set.member` this.context.droppedTables || (t, c) `Set.member` this.context.droppedColumns


typeRecipe :: Target -> TypeName -> [Step]
typeRecipe this typ = typed this asType \case
    Added def -> [create this (Shared, []) (createType def)]
    Dropped (DomainType{}) -> [remove this (Shared, []) ("drop domain " <> t)]
    Dropped _ -> [remove this (Shared, []) ("drop type " <> t)]
    Changed (EnumType oldLabels) (EnumType newLabels)
        | oldLabels `L.isSubsequenceOf` newLabels ->
            [ create this (Own, []) ("alter type " <> t <> " add value " <> literal label <> placement)
            | (label, position) <- zip newLabels [0 :: Int ..]
            , label `notElem` oldLabels
            , let placement = case previous position newLabels of
                    Just before -> " after " <> literal before
                    Nothing -> maybe "" ((" before " <>) . literal) (L.headMaybe (filter (`elem` oldLabels) newLabels))
            ]
    Changed _ _ -> manual this "only adding enum values is automated"
  where
    t = typeText typ
    createType = \case
        DomainType base notNull dflt -> T.unwords (["create domain " <> t <> " as " <> typeText base] <> ["not null" | notNull] <> ["default " <> d | Just d <- [dflt]])
        def@(CompositeType _) -> "create type " <> t <> " as " <> fromMaybe "" (T.stripPrefix "composite " (printed (TypeDefinition def)))
        def -> "create type " <> t <> " as " <> printed (TypeDefinition def)
    -- The label before the one at @position@, if any.
    previous position labels = if position == 0 then Nothing else L.headMaybe (L.drop (position - 1) labels)


routineRecipe :: Target -> RoutineKind -> QName -> [TypeName] -> [Step]
routineRecipe this kind q args = case textual this of
    Added def -> replaceRoutine def []
    Changed old new
        | take 2 (T.lines old) == take 2 (T.lines new) -> replaceRoutine new []
        | otherwise -> replaceRoutine new [MayFail "fails when the return type or argument names change; then drop and create it"]
    Dropped _ -> [remove this (Shared, []) [i|drop #{keyword} #{qualified q}(#{argList args})|]]
  where
    keyword :: Text
    keyword = case kind of
        Function -> "function"
        Procedure -> "procedure"
        Aggregate -> "aggregate"
    replaceRoutine def hazards
        | kind == Aggregate = manual this "aggregates are not automated"
        | otherwise = case Map.lookup this.objectKey this.context.bodyChecks of
            Just (checked, check) | checked == def -> case check of
                DoesNotParse err -> manual this ("PostgreSQL's printed body does not parse back: " <> err)
                Repaired text -> [create this (Shared, hazards) (T.stripEnd text)]
                _ -> [create this (Shared, hazards) (T.stripEnd def)]
            _
                | "BEGIN ATOMIC" `T.isInfixOf` def || any ("RETURN " `T.isPrefixOf`) (T.lines def) ->
                    [create this (Shared, hazards <> [MayFail "a SQL-standard body that was not checked against the desired database"]) (T.stripEnd def)]
                | otherwise -> [create this (Shared, hazards) (T.stripEnd def)]


viewRecipe :: Target -> QName -> [Step]
viewRecipe this v = typed this asView \case
    Added def -> [create this (Shared, []) ("create view " <> viewSql v def)]
    Changed old new
        | extendsColumns old new -> [create this (Shared, []) ("create or replace view " <> viewSql v new)]
        -- create or replace view keeps the columns as they were, adding any at the
        -- end; otherwise the view, and what depends on it, go and come back.
        | otherwise ->
            let dependents = Map.findWithDefault (Surrounding [] []) v this.context.viewDependents
             in map (create this (Shared, [])) dependents.dropBefore
                    <> [ create this (Shared, [MayFail "fails when other objects depend on the view; then drop them first"]) ("drop view " <> qualified v)
                       , create this (Shared, []) ("create view " <> viewSql v new)
                       ]
                    <> restoreOnView this v
                    <> restoreComments this (View v : [o | o@(OtherColumn OfView t _) <- Map.keys this.context.keptComments, t == v])
                    <> map (create this (Shared, [])) dependents.createAfter
    Dropped _ -> [remove this (Shared, []) ("drop view " <> qualified v)]


materializedViewRecipe :: Target -> QName -> [Step]
materializedViewRecipe this v = typed this asView \case
    -- Created again by a revert, it gets back its indexes, grants and comments.
    Added def -> create this (Shared, []) ("create materialized view " <> viewSql v def) : restoreOnView this v <> restoreViewComments this v
    Changed _ new ->
        [ remove this (Shared, [MayFail "fails when other objects depend on it"]) ("drop materialized view " <> qualified v)
        , create this (Shared, [LongLock]) ("create materialized view " <> viewSql v new)
        ]
            <> restoreOnView this v
            <> restoreViewComments this v
    Dropped _ -> [remove this (Shared, []) ("drop materialized view " <> qualified v)]


restoreViewComments :: Target -> QName -> [Step]
restoreViewComments this v = restoreComments this (MaterializedView v : [o | o@(OtherColumn OfMaterializedView t _) <- Map.keys this.context.keptComments, t == v])


restoreOnView :: Target -> QName -> [Step]
restoreOnView this v = case Map.lookup v this.context.recreatedViews of
    Nothing -> []
    Just kept ->
        [create this (Shared, [IndexBuild]) def | (_, def) <- kept.keptIndexes]
            <> restoreComments this [Index q | (q, _) <- kept.keptIndexes]
            <> [configure this (createRank this.objectKey.object) (Shared, []) (revokeGranted (qualified v)) | isJust kept.keptGrants]
            <> maybe [] (\(k, def) -> map (asPartOfThis this) (privileges k k.object (Added def))) kept.keptGrants


triggerRecipe :: Target -> QName -> Identifier -> [Step]
triggerRecipe this t n = case textual this of
    Added def -> create this (Shared, []) def : restoreComments this [Trigger t n]
    Changed old new
        | namesAlteredColumn this t old -> [dropTrigger DropPhase, create this (Shared, []) new] <> restoreComments this [Trigger t n]
        | Just rest <- T.stripPrefix "CREATE TRIGGER " new -> [replace this (Shared, []) ("CREATE OR REPLACE TRIGGER " <> rest)]
        | otherwise -> [dropTrigger CreatePhase, create this (Shared, []) new] <> restoreComments this [Trigger t n]
    Dropped _ -> [dropTrigger DropPhase]
  where
    dropTrigger = \case
        DropPhase -> remove this (Shared, []) dropSql
        CreatePhase -> dropToRecreate this (Shared, []) dropSql
    dropSql = "drop trigger " <> quoted n <> " on " <> qualified t


policyRecipe :: Target -> QName -> Identifier -> [Step]
policyRecipe this t n = case textual this of
    Added def -> createPolicy def : restoreComments this [Policy t n]
    Changed old new
        | namesAlteredColumn this t old -> [remove this (Shared, []) dropPolicy, createPolicy new] <> restoreComments this [Policy t n]
        | otherwise -> [dropToRecreate this (Shared, []) dropPolicy, createPolicy new] <> restoreComments this [Policy t n]
    Dropped _ -> [remove this (Shared, []) dropPolicy]
  where
    dropPolicy = "drop policy " <> quoted n <> " on " <> qualified t
    createPolicy def = create this (Shared, []) ("create policy " <> quoted n <> " on " <> qualified t <> "\n" <> def)


privileges :: ObjectKey -> ObjectId -> Change Definition -> [Step]
privileges key object change = case (target, traverse asAcl change) of
    (Nothing, _) -> []
    (Just onObject, Nothing) -> [step [Manual "the catalog described the privileges without their grants"] ("-- privileges on " <> onObject)]
    (_, Just (Dropped _)) -> []
    (Just onObject, Just (Added new)) -> reconcile onObject DefaultAcl new
    (Just onObject, Just (Changed old new)) -> reconcile onObject old new
  where
    step hazards sql =
        Step{phase = CreatePhase, role = Configures, rank = privilegesRank, object, sql, txn = Shared, hazards, key, change, undo = UndoObject}
    target = case object of
        Table t -> Just ("table " <> qualified t)
        View t -> Just ("table " <> qualified t)
        MaterializedView t -> Just ("table " <> qualified t)
        Sequence s -> Just ("sequence " <> qualified s)
        Routine Function q args -> Just [i|function #{qualified q}(#{argList args})|]
        Routine Procedure q args -> Just [i|procedure #{qualified q}(#{argList args})|]
        Schema s -> Just ("schema " <> quoted s)
        _ -> Nothing
    reconcile onObject old new = case (old, new) of
        (DefaultAcl, DefaultAcl) -> []
        (ExplicitAcl _, DefaultAcl) -> [step [Manual "privileges reset to the defaults"] ("-- reset privileges on " <> onObject)]
        _ -> grantChanges onObject (grantsOf old) (grantsOf new)
    -- What an object has when its ACL is the default, leaving out its owner's own
    -- privileges: PostgreSQL lets anyone execute a new function or procedure.
    grantsOf = \case
        ExplicitAcl grants -> grants
        DefaultAcl
            | isRoutine object -> Set.singleton (Grant Public "EXECUTE" False ByOwner)
            | otherwise -> Set.empty
    -- Grants another role gave are revoked first, and given last: a role passes on only
    -- what it holds with the grant option. PostgreSQL takes no GRANTED BY but the
    -- current role's, so those statements run as that role.
    grantChanges onObject old new =
        let byKey gs = Map.fromList [((g.grantor, g.grantee, g.privilege), g.grantable) | g <- Set.toList gs]
            before = byKey old
            now = byKey new
            onTarget privilege = privilege <> " on " <> onObject
            -- Granted anew, or given the grant option it lacked.
            needsGrant grantable = \case
                Nothing -> True
                Just had -> grantable && not had
            -- How far down a chain of grants a grant is: the owner's are first; one by a
            -- role comes after the grant that gave that role the privilege.
            depthIn held = go Set.empty
              where
                go seen (grantor, _, privilege) = case grantor of
                    ByOwner -> 0 :: Int
                    ByRole r
                        | r `Set.member` seen -> 1
                        | otherwise ->
                            1 + fromMaybe 0 (L.maximumMaybe [go (Set.insert r seen) k | k@(_, grantee, p) <- Map.keys held, grantee == Role r, p == privilege])
            revokes =
                L.sortOn (\(k, _) -> Down (depthIn before k))
                    $ [ (k, "revoke " <> onTarget privilege <> " from " <> granteeSql grantee)
                      | (k@(_, grantee, privilege), _) <- Map.toList (before `Map.difference` now)
                      ]
                    <> [ (k, "revoke grant option for " <> onTarget privilege <> " from " <> granteeSql grantee)
                       | (k@(_, grantee, privilege), (True, False)) <- Map.toList (Map.intersectionWith (,) before now)
                       ]
            grants =
                L.sortOn (\(k, _) -> depthIn now k)
                    $ [ (k, "grant " <> onTarget privilege <> " to " <> granteeSql grantee <> (if grantable then " with grant option" else ""))
                      | (k@(_, grantee, privilege), grantable) <- Map.toList now
                      , needsGrant grantable (Map.lookup k before)
                      ]
            asGrantor ((grantor, _, _), sql) = case grantor of
                ByOwner -> step [] sql
                ByRole r -> step [] (doBlock [i| begin set local role #{quoted r}; #{sql}; reset role; end |])
         in map asGrantor (revokes <> grants)


comment :: Context -> ObjectKey -> Change Definition -> [Step]
comment ctx key change = case (commentTarget key.object, printed <$> change) of
    (Nothing, _) -> []
    (Just _, Dropped _) | key.object `Set.member` ctx.droppedObjects -> []
    (Just subject, Dropped _) -> [step (subject <> " is null")]
    (Just subject, Added new) -> [step (subject <> " is " <> literal new)]
    (Just subject, Changed _ new) -> [step (subject <> " is " <> literal new)]
  where
    step sql =
        Step{phase = CreatePhase, role = Configures, rank = commentRank, object = key.object, sql = "comment on " <> sql, txn = Shared, hazards = [], key, change, undo = UndoObject}


-- | What @comment on@ names the object as.
commentTarget :: ObjectId -> Maybe Text
commentTarget = \case
    Table t -> Just ("table " <> qualified t)
    View t -> Just ("view " <> qualified t)
    MaterializedView t -> Just ("materialized view " <> qualified t)
    Column t c -> Just ("column " <> qualified t <> "." <> quoted c)
    OtherColumn OfView t c -> Just ("column " <> qualified t <> "." <> quoted c)
    OtherColumn OfMaterializedView t c -> Just ("column " <> qualified t <> "." <> quoted c)
    Index ix -> Just ("index " <> qualified ix)
    Sequence s -> Just ("sequence " <> qualified s)
    TableConstraint t c -> Just ("constraint " <> quoted c <> " on " <> qualified t)
    DomainConstraint d c -> Just ("constraint " <> quoted c <> " on domain " <> typeText d)
    Trigger t n -> Just ("trigger " <> quoted n <> " on " <> qualified t)
    Policy t n -> Just ("policy " <> quoted n <> " on " <> qualified t)
    Routine Function q args -> Just [i|function #{qualified q}(#{argList args})|]
    Routine Procedure q args -> Just [i|procedure #{qualified q}(#{argList args})|]
    Routine Aggregate q args -> Just [i|aggregate #{qualified q}(#{argList args})|]
    Type t -> Just ("type " <> typeText t)
    Schema s -> Just ("schema " <> quoted s)
    Extension e -> Just ("extension " <> quoted e)
    _ -> Nothing


-- | A routine's argument types, as its signature lists them.
argList :: [TypeName] -> Text
argList = T.intercalate ", " . map typeText


commentRank :: Int
commentRank = 16


-- | The column's declaration after its name, as @add column@ and @create table@ take it.
columnSql :: ColumnDef -> Text
columnSql c =
    T.unwords
        $ [typeText c.colType]
        <> ["collate " <> collationSql co | Just co <- [c.collation]]
        <> ["generated always as (" <> e <> ") " <> storageSql storage | Just (storage, e) <- [generatedOf c]]
        <> ["generated " <> identityKindSql kind <> " as identity" <> identityOptionsSql options | IdentityColumn kind options <- [c.value]]
        <> [maybe "" (\n -> "constraint " <> quoted n <> " ") (notNullName c) <> "not null" | isNotNull c]
        <> ["default " <> e | Just e <- [defaultExpr c]]


identityOptionsSql :: [Text] -> Text
identityOptionsSql = \case
    [] -> ""
    options -> [i| (#{T.unwords options})|]


-- | The @set@ clauses of @alter column@ that turn the identity options @old@ into @new@.
identityOptionChanges :: [Text] -> [Text] -> [Text]
identityOptionChanges old new =
    ["set " <> o | o <- new, o `notElem` old]
        <> ["set " <> reset k | k <- nubOrd (map optionKey old), k `notElem` map optionKey new]
  where
    optionKey o = if o == "cycle" then o else T.stripEnd (fst (T.breakOnEnd " " o))
    minimum' = fromMaybe "1" (L.headMaybe [v | o <- new, Just v <- [T.stripPrefix "minvalue " o]])
    reset = \case
        "increment by" -> "increment by 1"
        "minvalue" -> "no minvalue"
        "maxvalue" -> "no maxvalue"
        "start with" -> "start with " <> minimum'
        "cache" -> "cache 1"
        "cycle" -> "no cycle"
        k -> k


-- | A view's name, options and query, as @create view@ takes them after its keywords.
viewSql :: QName -> ViewDef -> Text
viewSql v def =
    qualified v
        <> foldMap (\o -> " with (" <> T.intercalate ", " o <> ")") def.options
        <> " as\n"
        <> T.dropWhileEnd (\ch -> ch == ';' || ch == ' ' || ch == '\n') (T.stripStart def.query)


-- | A concurrent index build that first fails when an invalid index of that name is left
-- from an earlier failed build.
concurrentBuild :: QName -> Text -> Text
concurrentBuild index build =
    let regclass = literal (qualified index)
        message = literal ("index " <> qualified index <> " is invalid from an earlier failed build; drop it first")
     in doBlock
            ( T.unlines
                [ ""
                , "begin"
                , "    if exists ("
                , "        select from pg_index"
                , [i|        where indexrelid = to_regclass(#{regclass}) and not indisvalid|]
                , "    ) then"
                , [i|        raise exception using message = #{message};|]
                , "    end if;"
                , "end"
                ]
            )
            <> ";\n\n"
            <> build


-- | The longest prefix of at most @n@ bytes in UTF-8.
takeBytes :: Int -> Text -> Text
takeBytes n = T.pack . go 0 . T.unpack
  where
    go _ [] = []
    go used (ch : rest)
        | used + size ch > n = []
        | otherwise = ch : go (used + size ch) rest
    size ch = B.length (encodeUtf8 (T.singleton ch))
