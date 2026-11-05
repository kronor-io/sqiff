{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | One fixed recipe per kind of object and kind of change.
module Kronor.SchemaDiff.Recipe (
    Phase (..),
    Txn (..),
    Hazard (..),
    Step (..),
    Context (..),
    ColumnDef (..),
    parseColumn,
    recipe,
) where

import Data.Text qualified as T
import Kronor.SchemaDiff.Body (BodyCheck (..))
import Kronor.SchemaDiff.Catalog (Address, Aspect (..), ObjectKey (..))
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
    | LongLock
    | IndexBuild
    | MayFail Text
    | Manual Text
    deriving stock (Eq, Ord, Show)


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
    , newTableColumns :: Map QName [(Text, ColumnDef)]
    , bodyChecks :: Map ObjectKey (Text, BodyCheck)
    -- ^ Body checks, with the definition each was made for.
    }


recipe :: Context -> ObjectKey -> Change -> [Step]
recipe ctx key change = case key.aspect of
    Privileges -> privileges key object change
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
        | otherwise -> column t c
    TableConstraint t c
        | t `Set.member` ctx.droppedTables -> []
        | otherwise -> tableConstraint t c
    DomainConstraint d c -> domainConstraint d c
    Index i -> indexRecipe i
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
            cols -> "\n" <> T.intercalate ",\n" ["    " <> quoteIdent c <> " " <> columnSql def | (c, def) <- cols] <> "\n"
        settings =
            [createAt tableSettingsRank (Shared, []) ("alter table " <> qualified t <> " enable row level security") | "row level security" `elem` lines']
                <> [createAt tableSettingsRank (Shared, []) ("alter table " <> qualified t <> " force row level security") | "force row level security" `elem` lines']

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
            [ createAt tableSettingsRank (Shared, []) ("alter table " <> qualified t <> " set (" <> T.intercalate ", " newOptions <> ")")
            | not (null newOptions)
            , L.sort newOptions /= L.sort oldOptions
            ]
                <> [ createAt tableSettingsRank (Shared, []) ("alter table " <> qualified t <> " reset (" <> T.intercalate ", " removedKeys <> ")")
                   | not (null removedKeys)
                   ]

    column t c = case change of
        Added def
            | t `Set.member` ctx.newTables -> []
            | otherwise ->
                let parsed = parseColumn def
                    failsOnRows = parsed.notNull && isNothing parsed.defaultExpr && isNothing parsed.identity && isNothing parsed.generated
                    hazards =
                        [MayFail "a not null column without a default fails on a table with rows" | failsOnRows]
                            <> [h | isJust parsed.identity || fmap fst parsed.generated == Just "stored", h <- [MayRewriteTable, LongLock]]
                 in [create (Shared, hazards) (alter <> "add column " <> quoteIdent c <> " " <> columnSql parsed)]
        Dropped _ -> [remove (Shared, [DeletesData]) (alter <> "drop column " <> quoteIdent c)]
        Changed old new -> alterColumn (parseColumn old) (parseColumn new)
      where
        alter = "alter table " <> qualified t <> " "
        col = alter <> "alter column " <> quoteIdent c <> " "
        alterColumn old new =
            let typeChanged = old.colType /= new.colType || old.collation /= new.collation
                defaultChanged = old.defaultExpr /= new.defaultExpr
                checkName = T.take 50 c <> "_not_null_tmp"
             in [create (Shared, []) (col <> "drop default") | typeChanged, isJust old.defaultExpr]
                    <> [ create (Shared, [MayRewriteTable, LongLock]) (col <> "type " <> new.colType <> maybe "" (" collate " <>) new.collation)
                       | typeChanged
                       ]
                    <> case new.defaultExpr of
                        Just e | defaultChanged || (typeChanged && isJust old.defaultExpr) -> [create (Shared, []) (col <> "set default " <> e)]
                        Nothing | defaultChanged && not typeChanged -> [create (Shared, []) (col <> "drop default")]
                        _ -> []
                    <> case (old.notNull, new.notNull) of
                        (False, True) ->
                            [ create (Shared, []) (alter <> "add constraint " <> quoteIdent checkName <> " check (" <> quoteIdent c <> " is not null) not valid")
                            , create (Own, []) (alter <> "validate constraint " <> quoteIdent checkName)
                            , create (Own, []) (col <> "set not null")
                            , create (Shared, []) (alter <> "drop constraint " <> quoteIdent checkName)
                            ]
                        (True, False) -> [create (Shared, []) (col <> "drop not null")]
                        _ -> []
                    <> case (old.identity, new.identity) of
                        (Nothing, Just k) -> [create (Shared, []) (col <> "add generated " <> k <> " as identity")]
                        (Just _, Nothing) -> [create (Shared, []) (col <> "drop identity")]
                        (Just a, Just b) | a /= b -> [create (Shared, []) (col <> "set generated " <> b)]
                        _ -> []
                    <> case (old.generated, new.generated) of
                        (Just (k, _), Just (k', e)) | k == k' && old.generated /= new.generated -> [create (Shared, [MayRewriteTable, LongLock]) (col <> "set expression as (" <> e <> ")")]
                        (a, b) | a /= b -> manual "generated column added or removed"
                        _ -> []

    tableConstraint t c = case change of
        Added def -> addConstraint def
        Dropped _ -> [remove (Shared, []) (alterT <> "drop constraint " <> quoteIdent c)]
        Changed _ new -> remove (Shared, []) (alterT <> "drop constraint " <> quoteIdent c) : addConstraint new
      where
        alterT = "alter table " <> qualified t <> " "
        plain hazards def = create (Shared, hazards) (alterT <> "add constraint " <> quoteIdent c <> " " <> def)
        addConstraint def
            | t `Set.member` ctx.newTables = [plain [] def]
            | validatable def && not (" NOT VALID" `T.isSuffixOf` def) =
                [ plain [] (def <> " NOT VALID")
                , create (Own, []) (alterT <> "validate constraint " <> quoteIdent c)
                ]
            | Just (kind, cols) <- simpleKey def
            , not (t `Set.member` ctx.partitionedTables) =
                [ create (None, [IndexBuild]) ("create unique index concurrently " <> quoteIdent c <> " on " <> qualified t <> " (" <> cols <> ")")
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
        alterD = "alter domain " <> d <> " "
        add def =
            [ create (Shared, []) (alterD <> "add constraint " <> quoteIdent c <> " " <> def <> " NOT VALID")
            , create (Own, []) (alterD <> "validate constraint " <> quoteIdent c)
            ]

    indexRecipe i = case change of
        Added def -> addIndex def
        Dropped def
            | indexTable def `elem` map (Just . qualified) (Set.toList ctx.droppedTables) -> []
            | onOnly def -> [remove (Shared, [LongLock]) ("drop index " <> qualified i)]
            | otherwise -> [remove (None, []) ("drop index concurrently " <> qualified i)]
        Changed old new
            | onOnly new || onOnly old -> remove (Shared, [LongLock]) ("drop index " <> qualified i) : addIndex new
            | otherwise ->
                let tmp = T.take 59 i.name <> "_new"
                 in [ create (None, [IndexBuild]) (T.replace (" INDEX " <> quoteIdent i.name <> " ON ") (" INDEX CONCURRENTLY " <> quoteIdent tmp <> " ON ") new)
                    , create (None, []) ("drop index concurrently " <> qualified i)
                    , create (Shared, []) ("alter index " <> qualified (QName i.schema tmp) <> " rename to " <> quoteIdent i.name)
                    ]
      where
        addIndex def
            | onOnly def = [create (Shared, [LongLock, IndexBuild]) (T.replace " ON ONLY " " ON " def)]
            | indexTable def `elem` map (Just . qualified) (Set.toList ctx.newTables) = [create (Shared, []) def]
            | otherwise = [create (None, [IndexBuild]) (T.replace " INDEX " " INDEX CONCURRENTLY " def)]
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
        Dropped _ -> [remove (Shared, []) ("drop " <> keyword <> " " <> qualified q <> "(" <> T.intercalate ", " args <> ")")]
      where
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
        Added def -> [create (Shared, []) ("create view " <> qualified v <> " as\n" <> viewBody def)]
        Changed _ new ->
            [ create
                (Shared, [MayFail "fails when a column is removed, renamed or retyped; then drop and recreate it with its dependents"])
                ("create or replace view " <> qualified v <> " as\n" <> viewBody new)
            ]
        Dropped _ -> [remove (Shared, []) ("drop view " <> qualified v)]

    materializedView v = case change of
        Added def -> [create (Shared, [LongLock]) ("create materialized view " <> qualified v <> " as\n" <> viewBody def)]
        Changed _ new ->
            [ remove (Shared, [MayFail "fails when other objects depend on it"]) ("drop materialized view " <> qualified v)
            , create (Shared, [LongLock]) ("create materialized view " <> qualified v <> " as\n" <> viewBody new)
            ]
        Dropped _ -> [remove (Shared, []) ("drop materialized view " <> qualified v)]

    trigger t n = case change of
        Added def -> [create (Shared, []) def]
        Changed _ new
            | Just rest <- T.stripPrefix "CREATE TRIGGER " new -> [create (Shared, []) ("CREATE OR REPLACE TRIGGER " <> rest)]
            | otherwise -> [dropTrigger CreatePhase, create (Shared, []) new]
        Dropped _ -> [dropTrigger DropPhase]
      where
        dropTrigger phase =
            Step phase (if phase == DropPhase then dropRank object else createRank object) addr object ("drop trigger " <> quoteIdent n <> " on " <> qualified t) Shared [] key change

    policy t n = case change of
        Added def -> [createPolicy def]
        Changed _ new -> [create (Shared, []) dropPolicy, createPolicy new]
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
        Routine Function q args -> Just ("function " <> qualified q <> "(" <> T.intercalate ", " args <> ")")
        Routine Procedure q args -> Just ("procedure " <> qualified q <> "(" <> T.intercalate ", " args <> ")")
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


data ColumnDef = ColumnDef
    { colType :: Text
    , collation :: Maybe Text
    , notNull :: Bool
    , identity :: Maybe Text
    , generated :: Maybe (Text, Text)
    , defaultExpr :: Maybe Text
    }
    deriving stock (Eq, Show)


parseColumn :: Text -> ColumnDef
parseColumn = foldl' add (ColumnDef "" Nothing False Nothing Nothing Nothing) . T.lines
  where
    add c line
        | Just t <- T.stripPrefix "type " line = c{colType = t}
        | Just x <- T.stripPrefix "collate " line = c{collation = Just x}
        | line == "not null" = c{notNull = True}
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
            , (\kind -> "generated " <> kind <> " as identity") <$> c.identity
            , if c.notNull then Just "not null" else Nothing
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


viewBody :: Text -> Text
viewBody = T.dropWhileEnd (\ch -> ch == ';' || ch == ' ' || ch == '\n') . T.stripStart


firstWithPrefix :: Text -> [Text] -> Maybe Text
firstWithPrefix prefix = listToMaybe . mapMaybe (T.stripPrefix prefix)


literal :: Text -> Text
literal t = "'" <> T.replace "'" "''" t <> "'"
