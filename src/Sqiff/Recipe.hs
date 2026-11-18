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
    Undo (..),
    Context (..),
    columnSql,
    recipe,
    createRank,
    dropRank,
    sequenceOwnerRank,
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
    MayFail why -> "hazard: " <> why
    Manual why -> "manual: " <> why


-- | Takes back from a relation every grant but its owner's, such as the ones default
-- privileges give a view created again, before its own grants are restored.
revokeGranted :: Text -> Text
revokeGranted relation =
    doBlock [i| declare g text; begin for g in select distinct case when x.grantee = 0 then 'public' else quote_ident(pg_get_userbyid(x.grantee)) end from pg_class c cross join aclexplode(c.relacl) x where c.oid = #{literal relation}::regclass and x.grantee <> c.relowner loop execute format('revoke all on table %s from %s', #{literal relation}, g); end loop; end |]


data Step = Step
    { phase :: Phase
    , rank :: Int
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


-- | How a step is reverted when its object's change is split over several migrations,
-- so that reverting one migration returns to the state after the one before it.
data Undo
    = -- | By the object's inverse: as a whole when its migration holds all the change, else
      -- a drop by creating the old object again and a create by dropping the new one.
      UndoObject
    | -- | By these steps, in order.
      UndoBy [Step]
    deriving stock (Eq, Show)


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
    , recreatedViews :: Map QName ([(QName, Text)], Maybe (ObjectKey, Definition))
    -- ^ Materialized views dropped and created again, with the indexes (name and
    -- definition) and privileges to restore on them.
    , keptComments :: Map ObjectId Text
    -- ^ The desired database's comments that do not change, which an object dropped and
    -- created again gets back.
    , viewsAround :: Map (QName, Identifier) ([Text], [Text])
    -- ^ For a column whose type changes, the statements that drop the views reading it
    -- and the ones that create them again, with their grants, indexes, triggers and comments.
    , droppedObjects :: Set ObjectId
    , convertedSequences :: Set QName
    -- ^ Sequences a column's change between serial and identity drops or creates, in
    -- the one step that changes the column.
    }


recipe :: Context -> ObjectKey -> Change Definition -> [Step]
recipe ctx key change = case key.aspect of
    Privileges -> privileges key object change
    Comment -> comment ctx key change
    Definition -> definition ctx key object change
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


definition :: Context -> ObjectKey -> ObjectId -> Change Definition -> [Step]
definition ctx key object change = case object of
    Schema s -> case textual of
        Added _ -> [create (Shared, []) ("create schema " <> quoted s)]
        Dropped _ -> [remove (Shared, []) ("drop schema " <> quoted s)]
        Changed _ _ -> []
    Extension e -> extension e
    Table t -> table t
    Column t c
        | t `Set.member` ctx.droppedTables -> []
        | t `Set.member` ctx.partitionedTables -> map onEveryPartition (column t c)
        | otherwise -> column t c
    TableConstraint t c
        | t `Set.member` ctx.droppedTables -> []
        | otherwise -> tableConstraint t c
    DomainConstraint d c -> domainConstraint d c
    Index ix -> indexRecipe ix
    Sequence s -> sequenceRecipe s
    Type t -> type' t
    Routine kind q args -> routine kind q args
    View v -> viewRecipe v
    MaterializedView v -> materializedView v
    Trigger t n
        | t `Set.member` ctx.droppedTables -> []
        | otherwise -> trigger t n
    Policy t n
        | t `Set.member` ctx.droppedTables -> []
        | otherwise -> policy t n
    OtherColumn{} -> noRecipe
    Unknown _ -> noRecipe
  where
    -- The definitions as the catalog prints them, for the recipes that read the text.
    textual = printed <$> change
    create (txn, hazards) sql = Step CreatePhase (createRank object) object sql txn hazards key change UndoObject
    createAt rank (txn, hazards) sql = Step CreatePhase rank object sql txn hazards key change UndoObject
    remove (txn, hazards) sql = Step DropPhase (dropRank object) object sql txn hazards key change UndoObject
    undoneBy steps s = s{undo = UndoBy steps}
    -- A concurrent build is reverted by dropping what it built.
    builds index = undoneBy [create (None, []) ("drop index concurrently if exists " <> qualified index)]
    manual why = [create (Shared, [Manual why]) ("-- " <> why)]
    -- The unchanged comments of what this step drops and creates again: they go with
    -- the old object, and nothing else in the plan sets them.
    restoreComments objects =
        [ undoneBy [] (createAt commentRank (Shared, []) ("comment on " <> subject <> " is " <> literal text))
        | o <- objects
        , Just text <- [Map.lookup o ctx.keptComments]
        , Just subject <- [commentTarget o]
        ]
    noRecipe = [create (Shared, [Manual "no recipe for this kind of object"]) ("-- " <> T.pack (show textual))]
    -- The change as the kind of definition this recipe reads; a definition of another
    -- kind is the catalog's mistake, left to a person.
    typed :: (Definition -> Maybe a) -> (Change a -> [Step]) -> [Step]
    typed as f = maybe (manual "the catalog described this object unexpectedly") f (traverse as change)
    notValid = " NOT VALID" :: Text
    -- Whether @new@ is @old@ validated.
    validates old new = old.validity == NotValidated && new.validity == Validated && withoutValidity old == new
    onEveryPartition s
        | any (`elem` [DeletesData, MayRewriteTable, LongLock]) s.hazards = s{hazards = s.hazards <> [EveryPartition]}
        | otherwise = s
    namesAlteredColumn t = namesAlteredColumnOf (== t)
    namesAlteredColumnOf onTable def =
        let names = Set.fromList (T.split (\ch -> not (isAlphaNum ch || ch == '_')) def)
         in any (\(t', c) -> onTable t' && nameText c `Set.member` names) (ctx.droppedColumns <> ctx.retypedColumns)

    extension e = typed asExtension \case
        Added def -> [create (Shared, []) ("create extension if not exists " <> quoted e <> " with schema " <> quoted def.schema <> " version " <> literal def.version)]
        Dropped _ -> [remove (Shared, []) ("drop extension " <> quoted e)]
        Changed old new
            | old.schema /= new.schema -> manual "extension moved to another schema"
            | otherwise -> [create (Own, []) ("alter extension " <> quoted e <> " update to " <> literal new.version)]

    table t = typed asTable \case
        Added def -> createTable t def
        Dropped _ -> [remove (Shared, [DeletesData]) ("drop table " <> qualified t)]
        Changed old new -> alterTable t old new

    createTable t def
        | Just p <- def.partitionOf =
            create (Shared, []) ("create table " <> qualified t <> " partition of " <> qualified p.parent <> " " <> p.bound) : settings
        | otherwise =
            create
                (Shared, [])
                ( (if def.unlogged then "create unlogged table " else "create table ")
                    <> qualified t
                    <> " ("
                    <> columnList
                    <> ")"
                    <> maybe "" (" partition by " <>) def.partitionBy
                    <> maybe "" (" " <>) (withOptions def.options)
                )
                : settings
      where
        columnList = case Map.findWithDefault [] t ctx.newTableColumns of
            [] -> ""
            cols -> [i|\n#{T.intercalate ",\n" (map columnLine cols)}\n|]
        columnLine (c, col) = [i|    #{quoted c} #{columnSql col}|]
        settings =
            [createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} enable row level security|] | def.rowSecurity]
                <> [createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} force row level security|] | def.forceRowSecurity]

    alterTable t old new
        | (old.partitionBy, old.partitionOf) /= (new.partitionBy, new.partitionOf) = manual "partitioning changed"
        | otherwise =
            toggle (.unlogged) ("set unlogged", "set logged") [MayRewriteTable, LongLock]
                <> toggle (.rowSecurity) ("enable row level security", "disable row level security") []
                <> toggle (.forceRowSecurity) ("force row level security", "no force row level security") []
                <> storageOptions
      where
        toggle setting (enable, disable) hazards
            | setting old == setting new = []
            | setting new = [createAt tableSettingsRank (Shared, hazards) ("alter table " <> qualified t <> " " <> enable)]
            | otherwise = [createAt tableSettingsRank (Shared, hazards) ("alter table " <> qualified t <> " " <> disable)]
        optionKey = T.takeWhile (/= '=')
        removedKeys = [k | o <- old.options, let k = optionKey o, k `notElem` map optionKey new.options]
        newOptions = T.intercalate ", " new.options
        storageOptions =
            [ createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} set (#{newOptions})|]
            | not (null new.options)
            , L.sort new.options /= L.sort old.options
            ]
                <> [ createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} reset (#{T.intercalate ", " removedKeys})|]
                   | not (null removedKeys)
                   ]

    column t c = case traverse asColumn change of
        Nothing -> manual "the catalog described the column without its column facts"
        Just (Added parsed)
            | t `Set.member` ctx.newTables -> []
            | otherwise ->
                let rewrites = parsed.rewritesOnAdd || isJust (identityKindOf parsed) || fmap fst (generatedOf parsed) == Just Stored
                    hazards =
                        [MayFail "a not null column without a default fails on a table with rows" | failsOnRows parsed]
                            <> [h | rewrites, h <- [MayRewriteTable, LongLock]]
                 in [create (Shared, hazards) (alter <> "add column " <> quoted c <> " " <> columnSql parsed)]
        Just (Dropped _) -> [remove (Shared, [DeletesData]) (alter <> "drop column " <> quoted c)]
        Just (Changed old new) -> alterColumn old new
      where
        alter = [i|alter table #{qualified t} |]
        col = [i|#{alter}alter column #{quoted c} |]
        alterColumn old new =
            let typeChanged = old.colType /= new.colType || old.collation /= new.collation
                defaultChanged = defaultExpr old /= defaultExpr new
                serialToIdentity = isNothing (identityKindOf old) && isJust (identityKindOf new) && isJust (serialSequence old)
                identityToSerial = isJust (identityKindOf old) && isNothing (identityKindOf new) && isJust (serialSequence new)
                sequenceMoves = serialToIdentity || identityToSerial
             in [create (Shared, []) (col <> "drop default") | typeChanged, isJust (defaultExpr old)]
                    <> [ create
                            ( Shared
                            , [MayRewriteTable, LongLock, MayFail "fails when an existing value does not convert to the new type"]
                                <> [MayFail "recreating a view fails when its query relies on the old type; then edit the view" | Map.member (t, c) ctx.viewsAround]
                            )
                            (aroundViews (col <> "type " <> typeText new.colType <> maybe "" ((" collate " <>) . quoted) new.collation <> " using " <> quoted c <> "::" <> typeText new.colType))
                       | typeChanged
                       ]
                    <> case defaultExpr new of
                        _ | sequenceMoves -> []
                        Just e | defaultChanged || (typeChanged && isJust (defaultExpr old)) -> [create (Shared, []) (col <> "set default " <> e)]
                        Nothing | defaultChanged && not typeChanged -> [create (Shared, []) (col <> "drop default")]
                        _ -> []
                    <> notNullSteps old new
                    <> case (identityKindOf old, identityKindOf new) of
                        _ | serialToIdentity, Just k <- identityKindOf new -> [create (Shared, []) (toIdentity (identityKindSql k <> " as identity" <> identityOptionsSql (identityOptionsOf new)))]
                        _ | identityToSerial -> [create (Shared, []) (toSerial (typeText new.colType))]
                        (Nothing, Just k) -> [create (Shared, []) (col <> "add generated " <> identityKindSql k <> " as identity" <> identityOptionsSql (identityOptionsOf new))]
                        (Just _, Nothing) -> [create (Shared, []) (col <> "drop identity")]
                        (Just a, Just b) ->
                            let clauses = ["set generated " <> identityKindSql b | a /= b] <> identityOptionChanges (identityOptionsOf old) (identityOptionsOf new)
                             in [create (Shared, []) (col <> T.unwords clauses) | not (null clauses)]
                        _ -> []
                    <> case (generatedOf old, generatedOf new) of
                        (Just (k, _), Just (k', e))
                            | k == k' && generatedOf old /= generatedOf new ->
                                [create (Shared, [h | k == Stored, h <- [MayRewriteTable, LongLock]]) [i|#{col}set expression as (#{e})|]]
                        (a, b) | a /= b -> manual "generated column added or removed"
                        _ -> []
        aroundViews statement = case Map.lookup (t, c) ctx.viewsAround of
            Just (drops, creates) | not (null drops) -> T.intercalate ";\n" (drops <> [statement] <> creates)
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
                create (Shared, []) [i|#{alter}add #{named new}not null #{quoted c} not valid|]
                    : [create (Own, []) (onNotNull (notNullName new) "validate constraint" "") | notNullValid new]
            (True, False) -> [create (Shared, []) (col <> "drop not null")]
            (True, True) ->
                [ create (Shared, []) (onNotNull (notNullName old) "rename constraint" (" to " <> quoted n))
                | notNullName old /= notNullName new
                , let n = fromMaybe (identifier (T.concat [nameText t.name, "_", nameText c, "_not_null"])) (notNullName new)
                ]
                    <> case (notNullValid old, notNullValid new) of
                        (False, True) -> [create (Own, []) (onNotNull (notNullName new) "validate constraint" "")]
                        (True, False) -> manual "a validated not null constraint cannot be made NOT VALID again"
                        _ -> []
            (False, False) -> []
        named d = maybe "" (\n -> "constraint " <> quoted n <> " ") (notNullName d)
        onNotNull name action rest = case name of
            Just n -> alter <> action <> " " <> quoted n <> rest
            Nothing ->
                let command = literal [i|#{T.replace "%" "%%" (alter <> action)} %I#{T.replace "%" "%%" rest}|]
                    tableLiteral = literal (qualified t)
                 in doBlock [i| begin execute format(#{command}, (select conname from pg_constraint where conrelid = #{tableLiteral}::regclass and contype = 'n' and conkey = array[(select attnum from pg_attribute where attrelid = #{tableLiteral}::regclass and attname = #{literal (nameText c)})])); end |]

    tableConstraint t c = typed asConstraint \case
        Added def -> addConstraint def <> restoreComments [TableConstraint t c]
        Dropped old -> [dropConstraint old]
        Changed old new
            | validates old new -> [undoneBy [] $ create (Own, []) (alterT <> "validate constraint " <> quoted c)]
            | validates new old -> manual "a validated constraint cannot be made NOT VALID again"
            | otherwise -> dropConstraint old : addConstraint new <> restoreComments [TableConstraint t c, Index (QName t.schema c)]
      where
        alterT = [i|alter table #{qualified t} |]
        dropConstraint old =
            remove
                (Shared, [MayFail "other objects depend on it, such as foreign keys; drop them first" | dependedOn] <> locksOther old)
                (alterT <> "drop constraint " <> quoted c)
        dependedOn = any (`Set.member` ctx.withDependents) [TableConstraint t c, Index (QName t.schema c)]
        locksOther def = [LocksOther (qualified r.table) | Just r <- [def.references], r.table /= t]
        plain hazards def clause = create (Shared, hazards <> locksOther def) (alterT <> "add constraint " <> quoted c <> " " <> clause)
        addConstraint def
            | t `Set.member` ctx.newTables = [plain [] def def.clause]
            | validatable def && def.validity == NotValidated = [plain [] def def.clause]
            | validatable def =
                [ plain [] def (def.clause <> notValid)
                , undoneBy [] $ create (Own, []) (alterT <> "validate constraint " <> quoted c)
                ]
            | Just (keyword, cols) <- simpleKey def
            , not (t `Set.member` ctx.partitionedTables) =
                let build = create (None, [IndexBuild]) (concurrentBuild (QName t.schema c) [i|create unique index concurrently #{quoted c} on #{qualified t} (#{cols})|])
                 in [ builds (QName t.schema c) build
                    , -- Dropping the constraint drops its index, which the build before made.
                      undoneBy [create (Shared, []) (alterT <> "drop constraint " <> quoted c), build]
                        $ create (Shared, []) (alterT <> "add constraint " <> quoted c <> " " <> keyword <> " using index " <> quoted c)
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

    domainConstraint d c = typed asConstraint \case
        Added def -> add def <> restoreComments [DomainConstraint d c]
        Dropped _ -> [remove (Shared, []) (alterD <> "drop constraint " <> quoted c)]
        Changed old new
            | validates old new -> [validate]
            | validates new old -> manual "a validated constraint cannot be made NOT VALID again"
            | otherwise -> remove (Shared, []) (alterD <> "drop constraint " <> quoted c) : add new <> restoreComments [DomainConstraint d c]
      where
        alterD = [i|alter domain #{typeText d} |]
        validate = undoneBy [] $ create (Own, []) (alterD <> "validate constraint " <> quoted c)
        add def
            | def.validity == NotValidated = [create (Shared, []) (alterD <> "add constraint " <> quoted c <> " " <> def.clause)]
            | otherwise = [create (Shared, []) (alterD <> "add constraint " <> quoted c <> " " <> def.clause <> notValid), validate]

    indexRecipe ix = typed asIndex \case
        Added def -> addIndex def <> restoreComments [Index ix]
        Dropped def
            | def.table `Set.member` ctx.droppedTables -> []
            | def.partitioned -> [remove (Shared, [LongLock]) ("drop index " <> qualified ix)]
            | otherwise -> [remove (None, []) ("drop index concurrently " <> qualified ix)]
        Changed old new -> replaceIndex old new <> restoreComments [Index ix]
      where
        replaceIndex old new
            | new.partitioned || old.partitioned = remove (Shared, [LongLock]) ("drop index " <> qualified ix) : addIndex new
            | namesAlteredColumnOf (== old.table) old.statement =
                remove (None, []) ("drop index concurrently if exists " <> qualified ix) : addIndex new
            | otherwise =
                let tmp = identifier (takeBytes 59 (nameText ix.name) <> "_new")
                    oldName = quoted ix.name
                 in [ builds (QName ix.schema tmp)
                        $ create
                            (None, [IndexBuild])
                            (concurrentBuild (QName ix.schema tmp) (replaceInCode [i| INDEX #{oldName} ON |] [i| INDEX CONCURRENTLY #{quoted tmp} ON |] new.statement))
                    , undoneBy (addIndex old <> restoreComments [Index ix]) $ create (None, []) ("drop index concurrently if exists " <> qualified ix)
                    , undoneBy [create (Shared, []) ("alter index " <> qualified ix <> " rename to " <> quoted tmp)]
                        $ create (Shared, []) ("alter index " <> qualified (QName ix.schema tmp) <> " rename to " <> oldName)
                    ]
        addIndex def
            | def.partitioned =
                [ create (Shared, []) def.statement
                , create
                    (Shared, [Manual [i|build the index on each partition with create index concurrently, then alter index #{qualified ix} attach partition each one; it stays invalid until every partition has one|]])
                    [i|-- create index concurrently and attach partition for #{qualified ix}|]
                ]
            | def.table `Set.member` ctx.newTables = [create (Shared, []) def.statement]
            | otherwise = [builds ix $ create (None, [IndexBuild]) (concurrentBuild ix (replaceInCode " INDEX " " INDEX CONCURRENTLY " def.statement))]

    sequenceRecipe s = typed asSequence \case
        -- Made by the column's change to serial; it only needs its options.
        Added def
            | s `Set.member` ctx.convertedSequences ->
                [createAt sequenceOwnerRank (Shared, []) ("alter sequence " <> qualified s <> " " <> sequenceOptions def <> " owned by " <> maybe "none" ownerSql def.ownedBy)]
        Added def ->
            create (Shared, []) ("create sequence " <> qualified s <> " " <> sequenceOptions def)
                : [createAt sequenceOwnerRank (Shared, []) ("alter sequence " <> qualified s <> " owned by " <> ownerSql o) | Just o <- [def.ownedBy]]
        Dropped def
            | Just o <- def.ownedBy, ownedByDropped o -> []
            -- Dropped by the column's change to identity.
            | s `Set.member` ctx.convertedSequences -> []
            | otherwise -> [remove (Shared, []) ("drop sequence " <> qualified s)]
        Changed _ new -> [createAt sequenceOwnerRank (Shared, []) ("alter sequence " <> qualified s <> " " <> sequenceOptions new <> " owned by " <> maybe "none" ownerSql new.ownedBy)]
      where
        ownerSql (t, c) = qualified t <> "." <> quoted c
        -- Dropping the column, or its table, drops the sequence with it.
        ownedByDropped (t, c) = t `Set.member` ctx.droppedTables || (t, c) `Set.member` ctx.droppedColumns

    type' typ = typed asType \case
        Added def -> [create (Shared, []) (createType def)]
        Dropped (DomainType{}) -> [remove (Shared, []) ("drop domain " <> t)]
        Dropped _ -> [remove (Shared, []) ("drop type " <> t)]
        Changed (EnumType oldLabels) (EnumType newLabels)
            | oldLabels `L.isSubsequenceOf` newLabels ->
                [ create (Own, []) ("alter type " <> t <> " add value " <> literal label <> placement)
                | (label, position) <- zip newLabels [0 :: Int ..]
                , label `notElem` oldLabels
                , let placement = case previous position newLabels of
                        Just before -> " after " <> literal before
                        Nothing -> maybe "" ((" before " <>) . literal) (L.headMaybe (filter (`elem` oldLabels) newLabels))
                ]
        Changed _ _ -> manual "only adding enum values is automated"
      where
        t = typeText typ
        createType = \case
            DomainType base notNull dflt -> T.unwords (["create domain " <> t <> " as " <> typeText base] <> ["not null" | notNull] <> ["default " <> d | Just d <- [dflt]])
            def@(CompositeType _) -> "create type " <> t <> " as " <> fromMaybe "" (T.stripPrefix "composite " (printed (TypeDefinition def)))
            def -> "create type " <> t <> " as " <> printed (TypeDefinition def)
        -- The label before the one at @position@, if any.
        previous position labels = if position == 0 then Nothing else L.headMaybe (L.drop (position - 1) labels)

    routine kind q args = case textual of
        Added def -> replaceRoutine def []
        Changed old new
            | take 2 (T.lines old) == take 2 (T.lines new) -> replaceRoutine new []
            | otherwise -> replaceRoutine new [MayFail "fails when the return type or argument names change; then drop and create it"]
        Dropped _ -> [remove (Shared, []) [i|drop #{keyword} #{qualified q}(#{argList args})|]]
      where
        keyword :: Text
        keyword = case kind of
            Function -> "function"
            Procedure -> "procedure"
            Aggregate -> "aggregate"
        replaceRoutine def hazards
            | kind == Aggregate = manual "aggregates are not automated"
            | otherwise = case Map.lookup key ctx.bodyChecks of
                Just (checked, check) | checked == def -> case check of
                    DoesNotParse err -> manual ("PostgreSQL's printed body does not parse back: " <> err)
                    Repaired text -> [create (Shared, hazards) (T.stripEnd text)]
                    _ -> [create (Shared, hazards) (T.stripEnd def)]
                _
                    | "BEGIN ATOMIC" `T.isInfixOf` def || any ("RETURN " `T.isPrefixOf`) (T.lines def) ->
                        [create (Shared, hazards <> [MayFail "a SQL-standard body that was not checked against the desired database"]) (T.stripEnd def)]
                    | otherwise -> [create (Shared, hazards) (T.stripEnd def)]

    viewRecipe v = typed asView \case
        Added def -> [create (Shared, []) ("create view " <> viewSql v def)]
        Changed _ new ->
            [ create
                (Shared, [MayFail "fails when a column is removed, renamed or retyped; then drop and recreate it with its dependents"])
                ("create or replace view " <> viewSql v new)
            ]
        Dropped _ -> [remove (Shared, []) ("drop view " <> qualified v)]

    materializedView v = typed asView \case
        -- Created again by a revert, it gets back its indexes, grants and comments.
        Added def -> create (Shared, []) ("create materialized view " <> viewSql v def) : restoreOnView v <> restoreViewComments v
        Changed _ new ->
            [ remove (Shared, [MayFail "fails when other objects depend on it"]) ("drop materialized view " <> qualified v)
            , create (Shared, [LongLock]) ("create materialized view " <> viewSql v new)
            ]
                <> restoreOnView v
                <> restoreViewComments v
        Dropped _ -> [remove (Shared, []) ("drop materialized view " <> qualified v)]

    restoreViewComments v = restoreComments (MaterializedView v : [o | o@(OtherColumn OfMaterializedView t _) <- Map.keys ctx.keptComments, t == v])
    restoreOnView v = case Map.lookup v ctx.recreatedViews of
        Nothing -> []
        Just (indexes, grants) ->
            [create (Shared, [IndexBuild]) def | (_, def) <- indexes]
                <> restoreComments [Index q | (q, _) <- indexes]
                <> [createAt privilegesRank (Shared, []) (revokeGranted (qualified v)) | isJust grants]
                <> maybe [] (\(k, def) -> privileges k k.object (Added def)) grants

    trigger t n = case textual of
        Added def -> create (Shared, []) def : restoreComments [Trigger t n]
        Changed old new
            | namesAlteredColumn t old -> [dropTrigger DropPhase, create (Shared, []) new] <> restoreComments [Trigger t n]
            | Just rest <- T.stripPrefix "CREATE TRIGGER " new -> [create (Shared, []) ("CREATE OR REPLACE TRIGGER " <> rest)]
            | otherwise -> [dropTrigger CreatePhase, create (Shared, []) new] <> restoreComments [Trigger t n]
        Dropped _ -> [dropTrigger DropPhase]
      where
        dropTrigger phase =
            Step phase (if phase == DropPhase then dropRank object else createRank object) object ("drop trigger " <> quoted n <> " on " <> qualified t) Shared [] key change UndoObject

    policy t n = case textual of
        Added def -> createPolicy def : restoreComments [Policy t n]
        Changed old new
            | namesAlteredColumn t old -> [remove (Shared, []) dropPolicy, createPolicy new] <> restoreComments [Policy t n]
            | otherwise -> [create (Shared, []) dropPolicy, createPolicy new] <> restoreComments [Policy t n]
        Dropped _ -> [remove (Shared, []) dropPolicy]
      where
        dropPolicy = "drop policy " <> quoted n <> " on " <> qualified t
        createPolicy def = create (Shared, []) ("create policy " <> quoted n <> " on " <> qualified t <> "\n" <> def)


privileges :: ObjectKey -> ObjectId -> Change Definition -> [Step]
privileges key object change = case (target, traverse asAcl change) of
    (Nothing, _) -> []
    (Just onObject, Nothing) -> [step [Manual "the catalog described the privileges without their grants"] ("-- privileges on " <> onObject)]
    (_, Just (Dropped _)) -> []
    (Just onObject, Just (Added new)) -> reconcile onObject DefaultAcl new
    (Just onObject, Just (Changed old new)) -> reconcile onObject old new
  where
    step hazards sql = Step CreatePhase privilegesRank object sql Shared hazards key change UndoObject
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
            byOthersFirst = L.sortOn (\((grantor, _, _), _) -> grantor == ByOwner)
            revokes =
                byOthersFirst
                    $ [ (k, "revoke " <> onTarget privilege <> " from " <> granteeSql grantee)
                      | (k@(_, grantee, privilege), _) <- Map.toList (before `Map.difference` now)
                      ]
                    <> [ (k, "revoke grant option for " <> onTarget privilege <> " from " <> granteeSql grantee)
                       | (k@(_, grantee, privilege), (True, False)) <- Map.toList (Map.intersectionWith (,) before now)
                       ]
            grants =
                L.sortOn (\((grantor, _, _), _) -> grantor /= ByOwner)
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
    step sql = Step CreatePhase commentRank key.object ("comment on " <> sql) Shared [] key change UndoObject


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
        <> ["collate " <> quoted co | Just co <- [c.collation]]
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
