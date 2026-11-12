{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | One fixed recipe per kind of object and kind of change.
module Kronor.SchemaDiff.Recipe (
    Phase (..),
    Txn (..),
    Hazard (..),
    hazardText,
    referencedTable,
    revokeGranted,
    Step (..),
    Context (..),
    ColumnDef (..),
    parseColumn,
    recipe,
) where

import Data.ByteString qualified as B
import Data.Char (isAlphaNum)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Kronor.SchemaDiff.Body (BodyCheck (..))
import Kronor.SchemaDiff.Catalog (Address (..), Aspect (..), ObjectKey (..))
import Kronor.SchemaDiff.Diff (Change (..))
import Kronor.SchemaDiff.Object
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set


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
    [i|do $$ declare g text; begin for g in select distinct case when x.grantee = 0 then 'public' else quote_ident(pg_get_userbyid(x.grantee)) end from pg_class c cross join aclexplode(c.relacl) x where c.oid = #{literal relation}::regclass and x.grantee <> c.relowner loop execute format('revoke all on table %s from %s', #{literal relation}, g); end loop; end $$|]


-- | The table a foreign key constraint's definition references.
referencedTable :: Text -> Maybe Text
referencedTable def = do
    guard ("FOREIGN KEY " `T.isPrefixOf` def)
    let (_, rest) = T.breakOn " REFERENCES " def
    T.takeWhile (/= '(') <$> T.stripPrefix " REFERENCES " rest


data Step = Step
    { phase :: Phase
    , rank :: Int
    , address :: Address
    , object :: Object
    , sql :: Text
    , txn :: Txn
    , hazards :: [Hazard]
    , key :: ObjectKey
    , change :: Change
    }
    deriving stock (Eq, Show)


data Context = Context
    { newTables :: Set QName
    , droppedTables :: Set QName
    , partitionedTables :: Set QName
    , droppedColumns :: Set (QName, Text)
    , retypedColumns :: Set (QName, Text)
    , newTableColumns :: Map QName [(Text, ColumnDef)]
    , bodyChecks :: Map ObjectKey (Text, BodyCheck)
    -- ^ Body checks, with the definition each was made for.
    , withDependents :: Set Object
    -- ^ Objects that something else depends on, in the database being changed.
    , recreatedViews :: Map QName ([Text], Maybe (ObjectKey, Text))
    -- ^ Materialized views dropped and created again, with the index definitions and
    -- privileges to restore on them.
    , viewsAround :: Map (QName, Text) ([Text], [Text])
    -- ^ For a column whose type changes, the statements that drop the views reading it
    -- and the ones that create them again, with their grants, indexes, triggers and comments.
    , droppedObjects :: Set Address
    }


recipe :: Context -> ObjectKey -> Change -> [Step]
recipe ctx key change = case key.aspect of
    Privileges -> privileges key object change
    Comment -> comment ctx key change
    Definition -> definition ctx key object change
  where
    object = fromAddress key.address


-- Ranks order steps within a phase; dependency order breaks ties.
createRank :: Object -> Int
createRank = \case
    Schema _ -> 0
    Extension _ -> 1
    Type _ -> 2
    Sequence _ -> 3
    Table _ -> 4
    Column _ _ -> 5
    Routine{} -> 6
    View _ -> 7
    MaterializedView _ -> 8
    TableConstraint _ _ -> 10
    DomainConstraint _ _ -> 10
    Index _ -> 11
    Trigger _ _ -> 12
    Policy _ _ -> 13
    Unknown _ -> 20


dropRank :: Object -> Int
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
    Unknown _ -> 20


sequenceOwnerRank, tableSettingsRank, privilegesRank :: Int
sequenceOwnerRank = 9
tableSettingsRank = 14
privilegesRank = 15


definition :: Context -> ObjectKey -> Object -> Change -> [Step]
definition ctx key object change = case object of
    Schema s -> case change of
        Added _ -> [create (Shared, []) ("create schema " <> quoteIdent s)]
        Dropped _ -> [remove (Shared, []) ("drop schema " <> quoteIdent s)]
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
    Unknown _ -> [create (Shared, [Manual "no recipe for this kind of object"]) ("-- " <> T.pack (show change))]
  where
    addr = key.address
    create (txn, hazards) sql = Step CreatePhase (createRank object) addr object sql txn hazards key change
    createAt rank (txn, hazards) sql = Step CreatePhase rank addr object sql txn hazards key change
    remove (txn, hazards) sql = Step DropPhase (dropRank object) addr object sql txn hazards key change
    manual why = [create (Shared, [Manual why]) ("-- " <> why)]
    onEveryPartition s
        | any (`elem` [DeletesData, MayRewriteTable, LongLock]) s.hazards = s{hazards = s.hazards <> [EveryPartition]}
        | otherwise = s
    -- A definition that names a column the edit drops or retypes has to go before it.
    namesAlteredColumn t = namesAlteredColumnOf (== t)
    namesAlteredColumnOf onTable def =
        let names = Set.fromList (T.split (\ch -> not (isAlphaNum ch || ch == '_')) def)
         in any (\(t', c) -> onTable t' && c `Set.member` names) (ctx.droppedColumns <> ctx.retypedColumns)

    extension e = case change of
        Added def ->
            let (version, schema) = extensionParts def
             in [create (Shared, []) ("create extension if not exists " <> quoteIdent e <> " with schema " <> schema <> " version " <> literal version)]
        Dropped _ -> [remove (Shared, []) ("drop extension " <> quoteIdent e)]
        Changed old new
            | snd (extensionParts old) /= snd (extensionParts new) -> manual "extension moved to another schema"
            | otherwise -> [create (Own, []) ("alter extension " <> quoteIdent e <> " update to " <> literal (fst (extensionParts new)))]

    table t = case change of
        Added def -> createTable t (T.lines def)
        Dropped _ -> [remove (Shared, [DeletesData]) ("drop table " <> qualified t)]
        Changed old new -> alterTable t (T.lines old) (T.lines new)

    createTable t lines'
        | Just parentAndBound <- firstWithPrefix "partition of " lines' =
            create (Shared, []) ("create table " <> qualified t <> " partition of " <> parentAndBound) : settings
        | otherwise =
            create
                (Shared, [])
                ( (if "unlogged" `elem` lines' then "create unlogged table " else "create table ")
                    <> qualified t
                    <> " ("
                    <> columnList
                    <> ")"
                    <> maybe "" (" partition by " <>) (firstWithPrefix "partition by " lines')
                    <> maybe "" (" with " <>) (firstWithPrefix "with " lines')
                )
                : settings
      where
        columnList = case Map.findWithDefault [] t ctx.newTableColumns of
            [] -> ""
            cols -> [i|\n#{T.intercalate ",\n" (map columnLine cols)}\n|]
        columnLine (c, def) = [i|    #{quoteIdent c} #{columnSql def}|]
        settings =
            [createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} enable row level security|] | "row level security" `elem` lines']
                <> [createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} force row level security|] | "force row level security" `elem` lines']

    alterTable t old new
        | partitioning old /= partitioning new = manual "partitioning changed"
        | otherwise =
            toggle "unlogged" ("set unlogged", "set logged") [MayRewriteTable, LongLock]
                <> toggle "row level security" ("enable row level security", "disable row level security") []
                <> toggle "force row level security" ("force row level security", "no force row level security") []
                <> storageOptions
      where
        partitioning = filter (\l -> "partition " `T.isPrefixOf` l)
        toggle line (enable, disable) hazards
            | (line `elem` old) == (line `elem` new) = []
            | line `elem` new = [createAt tableSettingsRank (Shared, hazards) ("alter table " <> qualified t <> " " <> enable)]
            | otherwise = [createAt tableSettingsRank (Shared, hazards) ("alter table " <> qualified t <> " " <> disable)]
        options ls = maybe [] (T.splitOn ", " . T.dropEnd 1 . T.drop 1) (firstWithPrefix "with " ls)
        oldOptions = options old
        newOptions = options new
        removedKeys = [k | o <- oldOptions, let k = T.takeWhile (/= '=') o, k `notElem` map (T.takeWhile (/= '=')) newOptions]
        storageOptions =
            [ createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} set (#{T.intercalate ", " newOptions})|]
            | not (null newOptions)
            , L.sort newOptions /= L.sort oldOptions
            ]
                <> [ createAt tableSettingsRank (Shared, []) [i|alter table #{qualified t} reset (#{T.intercalate ", " removedKeys})|]
                   | not (null removedKeys)
                   ]

    column t c = case change of
        Added def
            | t `Set.member` ctx.newTables -> []
            | otherwise ->
                let parsed = parseColumn def
                    failsOnRows = parsed.notNull && isNothing parsed.defaultExpr && isNothing parsed.identity && isNothing parsed.generated
                    rewrites = parsed.rewritesOnAdd || isJust parsed.identity || fmap fst parsed.generated == Just "stored"
                    hazards =
                        [MayFail "a not null column without a default fails on a table with rows" | failsOnRows]
                            <> [h | rewrites, h <- [MayRewriteTable, LongLock]]
                 in [create (Shared, hazards) (alter <> "add column " <> quoteIdent c <> " " <> columnSql parsed)]
        Dropped _ -> [remove (Shared, [DeletesData]) (alter <> "drop column " <> quoteIdent c)]
        Changed old new -> alterColumn (parseColumn old) (parseColumn new)
      where
        alter = [i|alter table #{qualified t} |]
        col = [i|#{alter}alter column #{quoteIdent c} |]
        alterColumn old new =
            let typeChanged = old.colType /= new.colType || old.collation /= new.collation
                defaultChanged = old.defaultExpr /= new.defaultExpr
                serialToIdentity = isNothing old.identity && isJust new.identity && isJust (serialSequence old) && isNothing new.defaultExpr
                identityToSerial = isJust old.identity && isNothing new.identity && isJust (serialSequence new) && isNothing old.defaultExpr
                sequenceMoves = serialToIdentity || identityToSerial
             in [create (Shared, []) (col <> "drop default") | typeChanged, isJust old.defaultExpr]
                    <> [ create
                            ( Shared
                            , [MayRewriteTable, LongLock, MayFail "fails when an existing value does not convert to the new type"]
                                <> [MayFail "recreating a view fails when its query relies on the old type; then edit the view" | Map.member (t, c) ctx.viewsAround]
                            )
                            (aroundViews (col <> "type " <> new.colType <> maybe "" (" collate " <>) new.collation <> " using " <> quoteIdent c <> "::" <> new.colType))
                       | typeChanged
                       ]
                    <> case new.defaultExpr of
                        _ | sequenceMoves -> []
                        Just e | defaultChanged || (typeChanged && isJust old.defaultExpr) -> [create (Shared, []) (col <> "set default " <> e)]
                        Nothing | defaultChanged && not typeChanged -> [create (Shared, []) (col <> "drop default")]
                        _ -> []
                    <> notNullSteps old new
                    <> case (old.identity, new.identity) of
                        _ | serialToIdentity, Just k <- new.identity -> [create (Shared, []) (toIdentity k)]
                        _ | identityToSerial -> [create (Shared, []) (toSerial new.colType)]
                        (Nothing, Just k) -> [create (Shared, []) [i|#{col}add generated #{k} as identity|]]
                        (Just _, Nothing) -> [create (Shared, []) (col <> "drop identity")]
                        (Just a, Just b) | a /= b -> [create (Shared, []) (col <> "set generated " <> b)]
                        _ -> []
                    <> case (old.generated, new.generated) of
                        (Just (k, _), Just (k', e))
                            | k == k' && old.generated /= new.generated ->
                                [create (Shared, [h | k == "stored", h <- [MayRewriteTable, LongLock]]) [i|#{col}set expression as (#{e})|]]
                        (a, b) | a /= b -> manual "generated column added or removed"
                        _ -> []
        aroundViews statement = case Map.lookup (t, c) ctx.viewsAround of
            Just (drops, creates) | not (null drops) -> T.intercalate ";\n" (drops <> [statement] <> creates)
            _ -> statement
        serialSequence d = d.defaultExpr >>= T.stripPrefix "nextval('" >>= T.stripSuffix "'::regclass)"
        tableLit = literal (qualified t)
        columnLit = literal c
        serialName = let tableName = t.name in qualified (QName t.schema [i|#{tableName}_#{c}_seq|])
        toIdentity k =
            [i|do $$
declare seq text := pg_get_serial_sequence(#{tableLit}, #{columnLit}); last bigint; called boolean; acl aclitem[];
begin
    execute format('select last_value, is_called from %s', seq) into last, called;
    select relacl into acl from pg_class where oid = seq::regclass;
    #{col}drop default;
    execute format('drop sequence %s', seq);
    #{col}add generated #{k} as identity;
    perform setval(pg_get_serial_sequence(#{tableLit}, #{columnLit}), last, called);
    #{grantAcl serialOf}
end $$|]
        toSerial colType =
            [i|do $$
declare last bigint; called boolean; acl aclitem[];
begin
    execute format('select last_value, is_called from %s', pg_get_serial_sequence(#{tableLit}, #{columnLit})) into last, called;
    select relacl into acl from pg_class where oid = pg_get_serial_sequence(#{tableLit}, #{columnLit})::regclass;
    #{col}drop identity;
    create sequence #{serialName} as #{colType} owned by #{qualified t}.#{quoteIdent c};
    perform setval(#{literal serialName}, last, called);
    #{col}set default nextval(#{literal serialName}::regclass);
    #{grantAcl (literal serialName)}
end $$|]
        serialOf = [i|pg_get_serial_sequence(#{tableLit}, #{columnLit})|] :: Text
        -- Gives the new sequence the grants the old one had, apart from its owner's.
        grantAcl :: Text -> Text
        grantAcl target =
            [i|execute coalesce((select string_agg(format('grant %s on sequence %s to %s', x.privilege_type, #{target}, case when x.grantee = 0 then 'public' else quote_ident(pg_get_userbyid(x.grantee)) end), '; ') from aclexplode(acl) x where x.grantee <> x.grantor), 'select');|]
        notNullSteps old new = case (old.notNull, new.notNull) of
            (False, True) ->
                create (Shared, []) [i|#{alter}add #{named new}not null #{quoteIdent c} not valid|]
                    : [create (Own, []) (onNotNull new.notNullName "validate constraint") | new.notNullValid]
            (True, False) -> [create (Shared, []) (col <> "drop not null")]
            (True, True) ->
                [ create (Shared, []) (onNotNull old.notNullName "rename constraint" <> " to " <> quoteIdent n)
                | old.notNullName /= new.notNullName
                , let n = fromMaybe (T.concat [t.name, "_", c, "_not_null"]) new.notNullName
                ]
                    <> case (old.notNullValid, new.notNullValid) of
                        (False, True) -> [create (Own, []) (onNotNull new.notNullName "validate constraint")]
                        (True, False) -> manual "a validated not null constraint cannot be made NOT VALID again"
                        _ -> []
            (False, False) -> []
        named d = maybe "" (\n -> [i|constraint #{quoteIdent n} |] :: Text) d.notNullName
        onNotNull name action = case name of
            Just n -> alter <> action <> " " <> quoteIdent n
            Nothing ->
                let command = literal [i|#{T.replace "%" "%%" (alter <> action)} %I|]
                    tableLiteral = literal (qualified t)
                 in [i|do $$ begin execute format(#{command}, (select conname from pg_constraint where conrelid = #{tableLiteral}::regclass and contype = 'n' and conkey = array[(select attnum from pg_attribute where attrelid = #{tableLiteral}::regclass and attname = #{literal c})])); end $$|]

    tableConstraint t c = case change of
        Added def -> addConstraint def
        Dropped old -> [dropConstraint old]
        Changed old new -> dropConstraint old : addConstraint new
      where
        alterT = [i|alter table #{qualified t} |]
        dropConstraint old =
            remove
                (Shared, [MayFail "other objects depend on it, such as foreign keys; drop them first" | dependedOn] <> locksOther old)
                (alterT <> "drop constraint " <> quoteIdent c)
        dependedOn = any (`Set.member` ctx.withDependents) [TableConstraint t c, Index (QName t.schema c)]
        locksOther def = [LocksOther r | Just r <- [referencedTable def], r /= qualified t]
        plain hazards def = create (Shared, hazards <> locksOther def) (alterT <> "add constraint " <> quoteIdent c <> " " <> def)
        addConstraint def
            | t `Set.member` ctx.newTables = [plain [] def]
            | validatable def && not (" NOT VALID" `T.isSuffixOf` def) =
                [ plain [] (def <> " NOT VALID")
                , create (Own, []) (alterT <> "validate constraint " <> quoteIdent c)
                ]
            | Just (kind, cols) <- simpleKey def
            , not (t `Set.member` ctx.partitionedTables) =
                [ create (None, [IndexBuild]) (concurrentBuild (QName t.schema c) [i|create unique index concurrently #{quoteIdent c} on #{qualified t} (#{cols})|])
                , create (Shared, []) (alterT <> "add constraint " <> quoteIdent c <> " " <> kind <> " using index " <> quoteIdent c)
                ]
            | otherwise = [plain [LongLock, IndexBuild] def]
        validatable def = any (`T.isPrefixOf` def) ["CHECK ", "FOREIGN KEY "]
        simpleKey def =
            listToMaybe
                [ (kind, cols)
                | kind <- ["PRIMARY KEY", "UNIQUE"]
                , Just rest <- [T.stripPrefix (kind <> " (") def]
                , Just cols <- [T.stripSuffix ")" rest]
                , not (T.any (`elem` ("()" :: String)) cols)
                ]

    domainConstraint d c = case change of
        Added def -> add def
        Dropped _ -> [remove (Shared, []) (alterD <> "drop constraint " <> quoteIdent c)]
        Changed _ new -> remove (Shared, []) (alterD <> "drop constraint " <> quoteIdent c) : add new
      where
        alterD = [i|alter domain #{d} |]
        add def =
            [ create (Shared, []) [i|#{alterD}add constraint #{quoteIdent c} #{def} NOT VALID|]
            , create (Own, []) (alterD <> "validate constraint " <> quoteIdent c)
            ]

    indexRecipe ix = case change of
        Added def -> addIndex def
        Dropped def
            | indexTable def `elem` map (Just . qualified) (Set.toList ctx.droppedTables) -> []
            | onOnly def -> [remove (Shared, [LongLock]) ("drop index " <> qualified ix)]
            | otherwise -> [remove (None, []) ("drop index concurrently " <> qualified ix)]
        Changed old new
            | onOnly new || onOnly old -> remove (Shared, [LongLock]) ("drop index " <> qualified ix) : addIndex new
            | namesAlteredColumnOf (\t -> indexTable old == Just (qualified t)) old ->
                remove (None, []) ("drop index concurrently if exists " <> qualified ix) : addIndex new
            | otherwise ->
                let tmp = takeBytes 59 ix.name <> "_new"
                    oldName = quoteIdent ix.name
                 in [ create
                        (None, [IndexBuild])
                        (concurrentBuild (QName ix.schema tmp) (T.replace [i| INDEX #{oldName} ON |] [i| INDEX CONCURRENTLY #{quoteIdent tmp} ON |] new))
                    , create (None, []) ("drop index concurrently if exists " <> qualified ix)
                    , create (Shared, []) ("alter index " <> qualified (QName ix.schema tmp) <> " rename to " <> oldName)
                    ]
      where
        addIndex def
            | onOnly def = [create (Shared, [WriteLock, EveryPartition, IndexBuild]) (T.replace " ON ONLY " " ON " def)]
            | indexTable def `elem` map (Just . qualified) (Set.toList ctx.newTables) = [create (Shared, []) def]
            | otherwise = [create (None, [IndexBuild]) (concurrentBuild ix (T.replace " INDEX " " INDEX CONCURRENTLY " def))]
        onOnly def = " ON ONLY " `T.isInfixOf` def
        indexTable def =
            let afterOn = T.drop 4 (snd (T.breakOn " ON " def))
             in Just (fst (T.breakOn " USING " (fromMaybe afterOn (T.stripPrefix "ONLY " afterOn))))

    sequenceRecipe s = case change of
        Added def ->
            let (base, owner) = sequenceParts def
             in create (Shared, []) ("create sequence " <> qualified s <> " " <> base)
                    : [createAt sequenceOwnerRank (Shared, []) ("alter sequence " <> qualified s <> " owned by " <> o) | Just o <- [owner]]
        Dropped def
            | Just o <- snd (sequenceParts def), ownedByDropped o -> []
            | otherwise -> [remove (Shared, []) ("drop sequence " <> qualified s)]
        Changed _ new ->
            let (base, owner) = sequenceParts new
             in [createAt sequenceOwnerRank (Shared, []) ("alter sequence " <> qualified s <> " " <> base <> " owned by " <> fromMaybe "none" owner)]
      where
        ownedByDropped o =
            any (\t -> (qualified t <> ".") `T.isPrefixOf` o) ctx.droppedTables
                || any (\(t, c) -> o == qualified t <> "." <> quoteIdent c) ctx.droppedColumns

    type' t = case change of
        Added def
            | Just rest <- T.stripPrefix "domain " def -> [create (Shared, []) ("create domain " <> t <> " as " <> rest)]
            | Just rest <- T.stripPrefix "composite " def -> [create (Shared, []) ("create type " <> t <> " as " <> rest)]
            | otherwise -> [create (Shared, []) ("create type " <> t <> " as " <> def)]
        Dropped def
            | "domain " `T.isPrefixOf` def -> [remove (Shared, []) ("drop domain " <> t)]
            | otherwise -> [remove (Shared, []) ("drop type " <> t)]
        Changed old new
            | Just oldLabels <- enumLabels old
            , Just newLabels <- enumLabels new
            , oldLabels `L.isSubsequenceOf` newLabels ->
                [ create (Own, []) ("alter type " <> t <> " add value " <> label <> placement)
                | (label, position) <- zip newLabels [0 :: Int ..]
                , label `notElem` oldLabels
                , let placement = case drop' position newLabels of
                        Just previous -> " after " <> previous
                        Nothing -> maybe "" (" before " <>) (L.headMaybe (filter (`elem` oldLabels) newLabels))
                ]
            | otherwise -> manual "only adding enum values is automated"
      where
        drop' position labels = if position == 0 then Nothing else L.headMaybe (L.drop (position - 1) labels)

    routine kind q args = case change of
        Added def -> replaceRoutine def []
        Changed old new
            | take 2 (T.lines old) == take 2 (T.lines new) -> replaceRoutine new []
            | otherwise -> replaceRoutine new [MayFail "fails when the return type or argument names change; then drop and create it"]
        Dropped _ -> [remove (Shared, []) [i|drop #{keyword} #{qualified q}(#{T.intercalate ", " args})|]]
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

    viewRecipe v = case change of
        Added def -> [create (Shared, []) ("create view " <> viewHead v def <> viewBody def)]
        Changed _ new ->
            [ create
                (Shared, [MayFail "fails when a column is removed, renamed or retyped; then drop and recreate it with its dependents"])
                ("create or replace view " <> viewHead v new <> viewBody new)
            ]
        Dropped _ -> [remove (Shared, []) ("drop view " <> qualified v)]

    materializedView v = case change of
        Added def -> [create (Shared, []) ("create materialized view " <> viewHead v def <> viewBody def)]
        Changed _ new ->
            [ remove (Shared, [MayFail "fails when other objects depend on it"]) ("drop materialized view " <> qualified v)
            , create (Shared, [LongLock]) ("create materialized view " <> viewHead v new <> viewBody new)
            ]
                <> restoreOnView v
        Dropped _ -> [remove (Shared, []) ("drop materialized view " <> qualified v)]

    restoreOnView v = case Map.lookup v ctx.recreatedViews of
        Nothing -> []
        Just (indexes, grants) ->
            [create (Shared, [IndexBuild]) def | def <- indexes]
                <> [createAt privilegesRank (Shared, []) (revokeGranted (qualified v)) | isJust grants]
                <> maybe [] (\(k, def) -> privileges k (fromAddress k.address) (Added def)) grants

    trigger t n = case change of
        Added def -> [create (Shared, []) def]
        Changed old new
            | namesAlteredColumn t old -> [dropTrigger DropPhase, create (Shared, []) new]
            | Just rest <- T.stripPrefix "CREATE TRIGGER " new -> [create (Shared, []) ("CREATE OR REPLACE TRIGGER " <> rest)]
            | otherwise -> [dropTrigger CreatePhase, create (Shared, []) new]
        Dropped _ -> [dropTrigger DropPhase]
      where
        dropTrigger phase =
            Step phase (if phase == DropPhase then dropRank object else createRank object) addr object ("drop trigger " <> quoteIdent n <> " on " <> qualified t) Shared [] key change

    policy t n = case change of
        Added def -> [createPolicy def]
        Changed old new
            | namesAlteredColumn t old -> [remove (Shared, []) dropPolicy, createPolicy new]
            | otherwise -> [create (Shared, []) dropPolicy, createPolicy new]
        Dropped _ -> [remove (Shared, []) dropPolicy]
      where
        dropPolicy = "drop policy " <> quoteIdent n <> " on " <> qualified t
        createPolicy def = create (Shared, []) ("create policy " <> quoteIdent n <> " on " <> qualified t <> "\n" <> def)


privileges :: ObjectKey -> Object -> Change -> [Step]
privileges key object change = case (target, change) of
    (Nothing, _) -> []
    (_, Dropped _) -> []
    (Just onObject, Added new) -> grants onObject Set.empty (aclLines new)
    (Just onObject, Changed old new)
        | new == "default" -> [step [Manual "privileges reset to the defaults"] ("-- reset privileges on " <> onObject)]
        | otherwise -> grants onObject (aclLines old) (aclLines new)
  where
    step hazards sql = Step CreatePhase privilegesRank key.address object sql Shared hazards key change
    target = case object of
        Table t -> Just ("table " <> qualified t)
        View t -> Just ("table " <> qualified t)
        MaterializedView t -> Just ("table " <> qualified t)
        Sequence s -> Just ("sequence " <> qualified s)
        Routine Function q args -> Just [i|function #{qualified q}(#{T.intercalate ", " args})|]
        Routine Procedure q args -> Just [i|procedure #{qualified q}(#{T.intercalate ", " args})|]
        Schema s -> Just ("schema " <> quoteIdent s)
        _ -> Nothing
    aclLines def = if def == "default" then Set.empty else Set.fromList (T.lines def)
    grants onObject old new =
        [step [] ("revoke " <> privilege <> " on " <> onObject <> " from " <> grantee) | line <- Set.toList (old Set.\\ new), let (grantee, privilege, _) = aclParts line]
            <> [ step [] ("grant " <> privilege <> " on " <> onObject <> " to " <> grantee <> (if grantable then " with grant option" else ""))
               | line <- Set.toList (new Set.\\ old)
               , let (grantee, privilege, grantable) = aclParts line
               ]
    aclParts line =
        let (role, rest) = T.breakOn " " line
            (privilege, option) = T.breakOn " with grant option" (T.drop 1 rest)
         in (if role == "public" then role else quoteIdent role, privilege, not (T.null option))


comment :: Context -> ObjectKey -> Change -> [Step]
comment ctx key change = case (target, change) of
    (Nothing, _) -> []
    (Just _, Dropped _) | key.address `Set.member` ctx.droppedObjects -> []
    (Just subject, Dropped _) -> [step (subject <> " is null")]
    (Just subject, Added new) -> [step (subject <> " is " <> literal new)]
    (Just subject, Changed _ new) -> [step (subject <> " is " <> literal new)]
  where
    addr = key.address
    step sql = Step CreatePhase commentRank addr object ("comment on " <> sql) Shared [] key change
    object = fromAddress addr
    target = case (object, addr.objectType, addr.objectNames) of
        (Table t, _, _) -> Just ("table " <> qualified t)
        (View t, _, _) -> Just ("view " <> qualified t)
        (MaterializedView t, _, _) -> Just ("materialized view " <> qualified t)
        (Column t c, _, _) -> Just ("column " <> qualified t <> "." <> quoteIdent c)
        (_, kind, [s, t, c]) | kind `elem` ["view column", "materialized view column"] -> Just ("column " <> qualified (QName s t) <> "." <> quoteIdent c)
        (Index ix, _, _) -> Just ("index " <> qualified ix)
        (Sequence s, _, _) -> Just ("sequence " <> qualified s)
        (TableConstraint t c, _, _) -> Just ("constraint " <> quoteIdent c <> " on " <> qualified t)
        (DomainConstraint d c, _, _) -> Just ("constraint " <> quoteIdent c <> " on domain " <> d)
        (Trigger t n, _, _) -> Just ("trigger " <> quoteIdent n <> " on " <> qualified t)
        (Policy t n, _, _) -> Just ("policy " <> quoteIdent n <> " on " <> qualified t)
        (Routine Function q args, _, _) -> Just [i|function #{qualified q}(#{T.intercalate ", " args})|]
        (Routine Procedure q args, _, _) -> Just [i|procedure #{qualified q}(#{T.intercalate ", " args})|]
        (Routine Aggregate q args, _, _) -> Just [i|aggregate #{qualified q}(#{T.intercalate ", " args})|]
        (Type t, _, _) -> Just ("type " <> t)
        (Schema s, _, _) -> Just ("schema " <> quoteIdent s)
        (Extension e, _, _) -> Just ("extension " <> quoteIdent e)
        _ -> Nothing


commentRank :: Int
commentRank = 16


data ColumnDef = ColumnDef
    { colType :: Text
    , collation :: Maybe Text
    , notNull :: Bool
    , notNullValid :: Bool
    , notNullName :: Maybe Text
    , identity :: Maybe Text
    , generated :: Maybe (Text, Text)
    , defaultExpr :: Maybe Text
    , rewritesOnAdd :: Bool
    }
    deriving stock (Eq, Show)


parseColumn :: Text -> ColumnDef
parseColumn = foldl' add (ColumnDef "" Nothing False False Nothing Nothing Nothing Nothing False) . T.lines
  where
    add c line
        | Just t <- T.stripPrefix "type " line = c{colType = t}
        | Just x <- T.stripPrefix "collate " line = c{collation = Just x}
        | Just x <- T.stripPrefix "not null" line =
            let (valid, named) = maybe (True, x) (False,) (T.stripPrefix " not valid" x)
             in c{notNull = True, notNullValid = valid, notNullName = T.stripPrefix " constraint " named}
        | line == "rewrites the table when added" = c{rewritesOnAdd = True}
        | Just x <- T.stripPrefix "identity " line = c{identity = Just x}
        | Just x <- T.stripPrefix "generated " line = let (kind, expr) = T.breakOn " " x in c{generated = Just (kind, T.drop 1 expr)}
        | Just x <- T.stripPrefix "default " line = c{defaultExpr = Just x}
        | otherwise = c


columnSql :: ColumnDef -> Text
columnSql c =
    T.unwords
        $ catMaybes
            [ Just c.colType
            , ("collate " <>) <$> c.collation
            , (\(kind, e) -> "generated always as (" <> e <> ") " <> kind) <$> c.generated
            , (\kind -> [i|generated #{kind} as identity|]) <$> c.identity
            , if c.notNull then Just (maybe "" (\n -> [i|constraint #{quoteIdent n} |]) c.notNullName <> "not null") else Nothing
            , ("default " <>) <$> c.defaultExpr
            ]


extensionParts :: Text -> (Text, Text)
extensionParts def =
    let (version, schema) = T.breakOn " schema " (fromMaybe def (T.stripPrefix "version " def))
     in (version, T.drop (T.length " schema ") schema)


sequenceParts :: Text -> (Text, Maybe Text)
sequenceParts def =
    let (base, owner) = T.breakOn " owned by " def
     in (base, T.stripPrefix " owned by " owner)


-- | The quoted labels of @enum ('a', 'b')@, as written.
enumLabels :: Text -> Maybe [Text]
enumLabels def = do
    inner <- T.stripPrefix "enum (" def >>= T.stripSuffix ")"
    pure (go inner)
  where
    go t
        | T.null t = []
        | otherwise =
            let (label, rest) = literalPrefix t
             in label : go (fromMaybe rest (T.stripPrefix ", " rest))
    literalPrefix t = case T.uncons t of
        Just ('\'', rest) -> let (body, after) = closing rest in ("'" <> body, after)
        _ -> T.breakOn ", " t
    closing t = case T.breakOn "'" t of
        (body, after)
            | "''" `T.isPrefixOf` after -> let (more, rest) = closing (T.drop 2 after) in (body <> "''" <> more, rest)
            | otherwise -> (body <> "'", T.drop 1 after)


-- | The name and @with (...)@ options of a described view, up to its query.
viewHead :: QName -> Text -> Text
viewHead v def = qualified v <> maybe "" (" " <>) (fst (viewParts def)) <> " as\n"


viewBody :: Text -> Text
viewBody = T.dropWhileEnd (\ch -> ch == ';' || ch == ' ' || ch == '\n') . T.stripStart . snd . viewParts


viewParts :: Text -> (Maybe Text, Text)
viewParts def = case T.breakOn "\n" def of
    (options, rest) | "with (" `T.isPrefixOf` options -> (Just options, T.drop 1 rest)
    _ -> (Nothing, def)


-- | A concurrent index build that first fails when an invalid index of that name is left
-- from an earlier failed build.
concurrentBuild :: QName -> Text -> Text
concurrentBuild index build =
    "do $$ begin if exists (select from pg_index where indexrelid = to_regclass("
        <> literal (qualified index)
        <> ") and not indisvalid) then raise exception 'index "
        <> T.replace "'" "''" (qualified index)
        <> " is invalid from an earlier failed build; drop it first'; end if; end $$;\n"
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


firstWithPrefix :: Text -> [Text] -> Maybe Text
firstWithPrefix prefix = listToMaybe . mapMaybe (T.stripPrefix prefix)


literal :: Text -> Text
literal t = [i|'#{T.replace "'" "''" t}'|]
