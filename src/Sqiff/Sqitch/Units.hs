{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Splits a plan into sqitch changes, and renders their deploy, revert and verify scripts.
module Sqiff.Sqitch.Units (
    UnitKind (..),
    Placement (..),
    Unit (..),
    units,
    slug,
    Shape (..),
    shapeOf,
    TemplateUse (..),
    templateOf,
    indexPrefix,
    renderScript,
    verifyScript,
    tidyConstraint,
    tidyIndex,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.String.Interpolate qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Config (Names (..), fill)
import Sqiff.Definition
import Sqiff.Diff (Change (..), Changes, after, before)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (invert)
import Sqiff.Recipe
import Sqiff.Sqitch.Plan (SqitchPlan (..))
import Sqiff.Sql (doBlock, literal, lowerCode, replaceAllInCode, terminator)


data UnitKind = AddChange | ReworkChange
    deriving stock (Eq, Show)


-- | Where a change lives: a time-stamped migration, or kronor's path for its object.
data Placement = Migration | Conventional
    deriving stock (Eq, Show)


-- | One sqitch change. A rework keeps the revert sqitch gives it: the previous version.
data Unit = Unit
    { kind :: UnitKind
    , changeName :: Text
    , note :: Text
    , placement :: Placement
    , deploy :: [Step]
    , revert :: [Step]
    }


-- | Groups ordered steps into changes: conventional paths for steps that pass
-- @conventional@, reworks of changes the plan has, and migrations for the rest. Each
-- gets its revert, and each migration its time-stamped name.
units :: Names -> (Step -> Bool) -> Context -> Changes -> SqitchPlan -> UTCTime -> [Step] -> [Unit]
units names conventional inverseCtx forward sqitchPlan now =
    nameMigrations names now
        . claimReverts inverseCtx forward
        . groupSteps names conventional sqitchPlan


-- | One change per step, merged with the next where they can share one: migration
-- steps of the same table, phase and transaction, and overloads of a routine.
groupSteps :: Names -> (Step -> Bool) -> SqitchPlan -> [Step] -> [Unit]
groupSteps names conventional sqitchPlan = foldr (merge . single) []
  where
    existing = Set.fromList sqitchPlan.changes
    single s = case if conventional s then conventionalName names s else Nothing of
        Just n
            | n `Set.member` existing -> unitOf ReworkChange n (stepNote "update" s) Conventional s
            | isAdded s.change -> unitOf AddChange n (createNote s) Conventional s
        _ -> unitOf AddChange (groupKey s) (stepNote (verb s) s) Migration s
    unitOf kind changeName note placement s = Unit{kind, changeName, note, placement, deploy = [s], revert = []}
    merge u (next : rest)
        | u.placement == Migration && next.placement == Migration && u.changeName == next.changeName && all sharable both && samePhase both =
            u{deploy = both} : rest
        -- Overloads of a routine share their conventional change.
        | u.placement == Conventional && next.placement == Conventional && u.changeName == next.changeName && u.kind == next.kind =
            u{deploy = both} : rest
      where
        both = u.deploy <> next.deploy
    merge u rest = u : rest
    sharable s = s.txn == Shared
    samePhase ss = length (nubOrd (map (.phase) ss)) <= 1


createNote :: Step -> Text
createNote s = case (s.object, s.change) of
    (Index ix, Added (IndexDefinition def))
        | Just t <- plainTable def ->
            let table = dotted (QName ix.schema t)
                name = nameText ix.name
             in [S.i|add_index: add index #{table}.#{name}|]
    _ -> stepNote "create" s


stepNote :: Text -> Step -> Text
stepNote v s = let what = render s.object in [S.i|sqiff: #{v} #{what}|]


-- | The migration a step goes in: its table's, or its own.
groupKey :: Step -> Text
groupKey s = case owningTable s.object of
    Just t -> dotted t
    Nothing -> T.intercalate "." (toAddress s.object).objectNames


verb :: Step -> Text
verb s
    | s.key.aspect == Privileges = "grant"
    | s.key.aspect == Comment = "comment"
    | s.role == RenamesColumn = "rename"
    | s.phase == DropPhase = "drop"
    | isAdded s.change = "add"
    | otherwise = "alter"


isAdded :: Change a -> Bool
isAdded = \case
    Added _ -> True
    _ -> False


-- | Where the steps of each object change sit among the migrations, numbered in order,
-- for deciding which migration reverts which part of it.
data RevertIndex = RevertIndex
    { withSteps :: Set ObjectKey
    -- ^ The object changes that have steps of their own.
    , objectUnits :: Map ObjectKey (Set Int)
    -- ^ The migrations holding steps of each object change that its inverse undoes.
    , mainUnits :: Map ObjectKey Int
    -- ^ The first migration with a step that creates, replaces or drops the object itself.
    , dropThenCreate :: Map ObjectKey Bool
    -- ^ Whether an object's change drops the old object in the drop phase and creates
    -- the new one in the create phase, so each part has its own inverse.
    }


revertIndex :: [Unit] -> RevertIndex
revertIndex us =
    RevertIndex
        { withSteps = Set.fromList [s.key | (_, s) <- numbered]
        , objectUnits = Map.fromListWith (<>) [(s.key, Set.singleton n) | (n, s) <- undoneByObject]
        , mainUnits = Map.fromListWith min [(s.key, n) | (n, s) <- undoneByObject, defines s]
        , dropThenCreate = Map.map (== Set.fromList [DropPhase, CreatePhase]) $ Map.fromListWith (<>) [(s.key, Set.singleton s.phase) | (_, s) <- undoneByObject]
        }
  where
    numbered = [(n, s) | (n, u) <- zip [0 :: Int ..] us, s <- u.deploy]
    undoneByObject = [(n, s) | (n, s) <- numbered, s.undo == UndoObject]


-- | Each migration reverts its own steps, so reverting it returns to the state after
-- the one before. A rework keeps the revert sqitch gives it: the previous version.
claimReverts :: Context -> Changes -> [Unit] -> [Unit]
claimReverts inverseCtx forward us =
    [ u{revert = if u.kind == ReworkChange then [] else map refill (revertOf index ctx n u <> withTables index ctx forward u)}
    | (n, u) <- zip [0 :: Int ..] us
    , let ctx = unitContext inverseCtx u
    ]
  where
    index = revertIndex us


-- | A migration's revert creates or drops only its own tables; another migration's
-- revert sees to the rest.
unitContext :: Context -> Unit -> Context
unitContext inverseCtx u =
    inverseCtx{droppedTables = Set.intersection inverseCtx.droppedTables own, newTables = Set.intersection inverseCtx.newTables own}
  where
    own = Set.fromList [t | s <- u.deploy, s.key.aspect == Definition, Table t <- [s.object]]


-- | What went with a table the migration drops, which has no steps of its own: its
-- constraints, indexes, triggers, policies, comments and grants.
withTables :: RevertIndex -> Context -> Changes -> Unit -> [Step]
withTables index ctx forward u =
    L.sortOn (.rank)
        $ concat
            [ recipe ctx k (Added d)
            | not (Set.null dropped)
            , (k, Dropped d) <- Map.toList forward
            , k `Set.notMember` index.withSteps
            , belongs k d
            ]
  where
    dropped = Set.fromList [t | s <- u.deploy, s.key.aspect == Definition, Table t <- [s.object], Dropped _ <- [s.change]]
    belongs k d = case k.object of
        Table t -> k.aspect /= Definition && t `Set.member` dropped
        Index _ | Just ix <- asIndex d -> ix.table `Set.member` dropped
        o -> maybe False (`Set.member` dropped) (owningTable o) && not (isColumn o)
    isColumn = \case
        Column{} -> True
        _ -> False


-- | The revert of migration @n@'s own steps, object change by object change, last first.
revertOf :: RevertIndex -> Context -> Int -> Unit -> [Step]
revertOf index ctx n u =
    concat [undoKey k (filter ((== k) . (.key)) u.deploy) | k <- reverse (nubOrd (map (.key) u.deploy))]
  where
    -- The key's steps here, undone last to first; a run of steps the object's inverse
    -- undoes gets it once.
    undoKey k here = go False (reverse here)
      where
        go _ [] = []
        go done (s : rest) = case s.undo of
            UndoBy undoing -> undoing <> go done rest
            UndoObject
                | done -> go done rest
                | otherwise -> objectPart index ctx n k s.change [x | x <- here, x.undo == UndoObject] <> go True rest


-- | The part of an object change's inverse that migration @n@ reverts, given its own
-- steps of it (@mine@).
objectPart :: RevertIndex -> Context -> Int -> ObjectKey -> Change Definition -> [Step] -> [Step]
objectPart index ctx n k change mine
    | Map.lookup k index.objectUnits == Just (Set.singleton n) = recipe ctx k (invert change)
    -- The migration with the object's own drop, or create, reverts it; one that only sets
    -- its grants, indexes or comments can be deployed again as it is.
    | dropsThenCreates
    , Changed old _ <- change
    , all ((== DropPhase) . (.phase)) mine =
        if any defines mine then recipe ctx k (Added old) else []
    | dropsThenCreates
    , Changed _ new <- change
    , all ((== CreatePhase) . (.phase)) mine =
        if any defines mine then recipe ctx k (Dropped new) else []
    -- Otherwise the migration with its main step reverts it all, or the last with a part
    -- of it when none has one.
    | Just n == (Map.lookup k index.mainUnits <|> (Map.lookup k index.objectUnits >>= Set.lookupMax)) = recipe ctx k (invert change)
    | otherwise = []
  where
    dropsThenCreates = Map.findWithDefault False k index.dropThenCreate


-- | A column that fails on a table with rows comes back nullable: its values were
-- dropped with it.
refill :: Step -> Step
refill s
    | Column t c <- s.object
    , Added (ColumnDefinition p) <- s.change
    , failsOnRows p =
        let table = qualified t
            column = quoted c
            definition = columnSql p{nullability = Nullable}
         in s
                { sql = [S.i|alter table #{table} add column #{column} #{definition}|]
                , hazards = [MayFail "the column comes back nullable and empty, since its values were dropped"]
                }
    | otherwise = s


-- | Names each migration by its stamp, one second apart from the one before, and its
-- description.
nameMigrations :: Names -> UTCTime -> [Unit] -> [Unit]
nameMigrations names now = go 0
  where
    go :: Int -> [Unit] -> [Unit]
    go _ [] = []
    go i (u : rest)
        | u.placement == Migration =
            let stamp = T.pack (formatTime defaultTimeLocale (T.unpack names.migrationStamp) (addUTCTime (fromIntegral i) now))
                description = describe u
                template = maybe "migration" (.name) (templateOf u)
             in u{changeName = fill names.migration [("stamp", stamp), ("slug", slug description)], note = [S.i|#{template}: #{description}|]} : go (i + 1) rest
        | otherwise = u : go i rest


describe :: Unit -> Text
describe u = case shapeOf u of
    AddColumnShape t c _ -> let table = dotted t; column = nameText c in [S.i|add column #{table}.#{column}|]
    ValidateShape t c -> let table = dotted t; constraint = nameText c in [S.i|validate constraint #{table}.#{constraint}|]
    _ ->
        let objects = nubOrd [fromMaybe (render s.object) (L.lastMaybe (toAddress s.object).objectNames) | s <- u.deploy]
            joined = T.intercalate "-" objects
            suffix = if objects == maybeToList (L.lastMaybe (T.split (== '.') u.changeName)) then "" else [S.i|.#{joined}|] :: Text
            v = maybe "alter" verb (listToMaybe u.deploy)
            name = u.changeName
         in [S.i|#{v} #{name}#{suffix}|] :: Text


-- | Where the project keeps a new object of this kind, for the steps that create or replace one.
conventionalName :: Names -> Step -> Maybe Text
conventionalName names s
    | s.key.aspect /= Definition = Nothing
    | otherwise = case (s.object, s.change) of
        (Table t, Added _) | s.role == Creates -> Just (path names.table t)
        (Routine Aggregate _ _, _) -> Nothing
        (Routine _ q _, Added _) -> Just (path names.function q)
        (Routine _ q _, Changed _ _) -> Just (path names.function q)
        (View v, Added _) -> Just (path names.view v)
        (View v, Changed _ _) -> Just (path names.view v)
        (MaterializedView v, Added _) | s.phase == CreatePhase -> Just (path names.view v)
        (Trigger t n, Added _) -> Just (triggerPath t n)
        (Trigger t n, Changed _ _) | s.role == Replaces -> Just (triggerPath t n)
        (Index ix, Added (IndexDefinition def)) | Just t <- plainTable def -> Just (fill names.index [("schema", nameText ix.schema), ("table", nameText t), ("name", nameText ix.name)])
        (Type t, Added _) | [schema, name] <- T.splitOn "." (typeText t) -> Just (fill names.type_ [("schema", schema), ("name", name)])
        _ -> Nothing
  where
    path pattern q = fill pattern [("schema", nameText q.schema), ("name", nameText q.name)]
    triggerPath t n = fill names.trigger [("schema", nameText t.schema), ("table", nameText t.name), ("name", nameText n)]


-- | The name of the table an index is on, when SQL writes it without quotes.
plainTable :: IndexDef -> Maybe Identifier
plainTable def = do
    let t = def.table.name
    guard (quoted t == nameText t)
    pure t


-- | The changes create-migration's templates cover.
data Shape
    = -- | The table, the column, and the column's type and options.
      AddColumnShape QName Identifier Text
    | ValidateShape QName Identifier
    | AddIndexShape QName Identifier
    | OtherShape


shapeOf :: Unit -> Shape
shapeOf u = case u.deploy of
    [s]
        | u.placement == Conventional
        , Index ix <- s.object
        , Added (IndexDefinition def) <- s.change
        , Just t <- plainTable def ->
            AddIndexShape (QName ix.schema t) ix.name
        | u.placement == Migration
        , Column t c <- s.object
        , Added _ <- s.change
        , plain t
        , quoted c == nameText c
        , -- [allow:SqiffStepRole] reads a template variable back out of the SQL; steps do not carry it yet
          Just options <- T.stripPrefix [S.i|alter table #{qualified t} add column #{quoted c} |] s.sql
        , not (any (`elem` [MayRewriteTable, RewritesTable, LongLock]) s.hazards) ->
            AddColumnShape t c options
        | u.placement == Migration
        , Just t <- owningTable s.object
        , plain t
        , -- [allow:SqiffStepRole] reads a template variable back out of the SQL; steps do not carry it yet
          Just c <- T.stripPrefix [S.i|alter table #{qualified t} validate constraint |] s.sql
        , quoteIdent c == c ->
            ValidateShape t (identifier c)
    _ -> OtherShape
  where
    plain t = qualified t == dotted t


-- | The add_index template names an index idx_<table>_<index_name>.
indexPrefix :: QName -> Text
indexPrefix t = let name = nameText t.name in [S.i|idx_#{name}_|]


-- | A create-migration template and the variables it is filled with.
data TemplateUse = TemplateUse
    { name :: Text
    , variables :: [(Text, Text)]
    }


-- | The template and its variables, as create-migration would pass them.
templateOf :: Unit -> Maybe TemplateUse
templateOf u = case shapeOf u of
    AddColumnShape t c _ -> Just (TemplateUse "add_column" [("schema", nameText t.schema), ("table_name", nameText t.name), ("column_name", nameText c)])
    ValidateShape t c -> Just (TemplateUse "validate_constraint" [("schema", nameText t.schema), ("table_name", nameText t.name), ("constraint_name", nameText c)])
    AddIndexShape t ix -> Just (TemplateUse "add_index" [("schema", nameText t.schema), ("table_name", nameText t.name), ("index_name", fromMaybe (nameText ix) (T.stripPrefix (indexPrefix t) (nameText ix)))])
    OtherShape | u.placement == Migration -> Just (TemplateUse "migration" [])
    OtherShape -> Nothing


slug :: Text -> Text
slug = T.take 80 . T.intercalate "-" . filter (not . T.null) . T.split (not . allowed) . T.toLower
  where
    allowed c = isAsciiLower c || isDigit c || c == '_' || c == '.'


-- | A script: shared steps in one transaction, others in their own or none.
renderScript :: Text -> Placement -> [Step] -> Text
renderScript header placement steps =
    T.intercalate "\n\n" (header : ["set lock_timeout to '50ms';" | placement == Conventional] <> map block (L.groupBy together steps)) <> "\n"
  where
    together a b = a.txn == Shared && b.txn == Shared
    block group' = case group' of
        [s] | s.txn == None -> statement (idempotent s.sql) s
        _ ->
            T.intercalate "\n\n"
                $ ["begin;"]
                <> ["  -- Do not remove this line\n  select set_config('lock_timeout', '50ms', false);" | placement == Migration]
                <> map (\s -> if safeToIndent s.sql then indent (statement s.sql s) else statement s.sql s) group'
                <> ["commit;"]
    statement sql s =
        T.intercalate "\n" (map ("-- " <>) (hazardNotes s) <> [lowerCode (tidy s sql) <> terminator sql] <> stopUntilWritten s)
    -- A step sqiff cannot write stops the deploy until someone writes it, rather than
    -- deploying as a change that does nothing.
    stopUntilWritten s =
        [ doBlock [S.i| begin raise exception using message = #{literal message}; end |] <> ";"
        | placeholder s
        , why <- take 1 [w | Manual w <- s.hazards]
        , let what = render s.object
              message = [S.i|sqiff could not write this step for #{what}: #{why}; write it here and remove this block|] :: Text
        ]
    -- A step left to a person: its SQL is a "--" note saying what to write.
    -- [allow:SqiffStepRole] such a step has no role of its own yet
    placeholder s = "--" `T.isPrefixOf` s.sql
    indent = T.intercalate "\n" . map (\l -> if T.null l then l else "  " <> l) . T.lines
    safeToIndent sql = not ("$" `T.isInfixOf` sql) && all (even . T.count "'") (T.lines sql)
    tidy s sql = case T.lines sql of
        [l]
            | Just rest <- T.stripPrefix "alter table " l
            , (name, more) <- T.breakOn " " rest
            , not ("\"" `T.isInfixOf` name)
            , not (T.null more) ->
                let clause = tidyConstraint (T.stripStart more) in [S.i|alter table #{name}\n    #{clause}|]
        ls
            | indexLike s.object -> T.intercalate "\n" (map tidyIndex ls)
            | otherwise -> sql
    indexLike = \case Index _ -> True; TableConstraint _ _ -> True; _ -> False
    idempotent sql =
        replaceAllInCode
            [ ("drop index concurrently if exists ", "drop index concurrently ")
            , ("drop index concurrently ", "drop index concurrently if exists ")
            , ("create unique index concurrently ", "create unique index concurrently if not exists ")
            , ("CREATE INDEX CONCURRENTLY ", "CREATE INDEX CONCURRENTLY IF NOT EXISTS ")
            , ("CREATE UNIQUE INDEX CONCURRENTLY ", "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS ")
            ]
            sql
    hazardNotes s = map hazardText s.hazards


-- | pg_get_constraintdef's keywords in lower case, leaving a check's expression alone.
tidyConstraint :: Text -> Text
tidyConstraint clause = case T.stripPrefix "add constraint " clause of
    Just rest ->
        let (name, def) = T.breakOn " " rest
            lowered
                | "FOREIGN KEY " `T.isPrefixOf` T.stripStart def = replaceAllInCode foreignKey def
                | otherwise = fromMaybe def (asum [(new <>) <$> T.stripPrefix old def | (old, new) <- leading])
            rest' = replaceSuffixes lowered
         in [S.i|add constraint #{name}#{rest'}|]
    Nothing -> clause
  where
    leading = [(" CHECK ", " check "), (" UNIQUE ", " unique "), (" PRIMARY KEY ", " primary key "), (" EXCLUDE USING ", " exclude using "), (" NOT NULL ", " not null ")]
    foreignKey =
        [ (" FOREIGN KEY ", " foreign key ")
        , (" REFERENCES ", " references ")
        , (" ON DELETE ", " on delete ")
        , (" ON UPDATE ", " on update ")
        , (" CASCADE", " cascade")
        , (" SET NULL", " set null")
        , (" SET DEFAULT", " set default")
        , (" RESTRICT", " restrict")
        , (" NO ACTION", " no action")
        , (" MATCH FULL", " match full")
        , (" DEFERRABLE", " deferrable")
        , (" INITIALLY DEFERRED", " initially deferred")
        , (" INITIALLY IMMEDIATE", " initially immediate")
        ]
    replaceSuffixes t = foldl' (\acc (old, new) -> maybe acc (<> new) (T.stripSuffix old acc)) t [(" NO INHERIT", " no inherit"), (" NOT VALID", " not valid")]


-- | pg_get_indexdef's keywords in lower case, without the default @USING btree@.
tidyIndex :: Text -> Text
tidyIndex line
    | "CREATE " `T.isPrefixOf` line && " INDEX " `T.isInfixOf` line =
        let (front, back) = T.breakOn " USING " line
            method = T.drop 7 back
            (name, columns) = T.breakOn " " method
            using = if name == "btree" then "" else [S.i| using #{name}|]
         in replaceAllInCode keywords front <> using <> replaceAllInCode tail' columns
    | otherwise = line
  where
    keywords =
        [ ("CREATE UNIQUE INDEX ", "create unique index ")
        , ("CREATE INDEX ", "create index ")
        , ("CONCURRENTLY ", "concurrently ")
        , ("IF NOT EXISTS ", "if not exists ")
        , (" ON ONLY ", " on only ")
        , (" ON ", " on ")
        ]
    tail' = [(" INCLUDE (", " include ("), (" NULLS NOT DISTINCT", " nulls not distinct"), (" WITH (", " with ("), (" WHERE ", " where ")]


-- | A verify script that checks in the catalog what a migration's statements leave
-- behind. It runs with an empty @search_path@, as sqiff reads the catalog, so names and
-- types print as the plan has them.
verifyScript :: Text -> [Step] -> Text
verifyScript header steps =
    T.intercalate "\n\n" ([header, "begin;", "    set local search_path = '';"] <> map ("    " <>) selects <> assertBlock <> ["rollback;"]) <> "\n"
  where
    final = Map.elems (Map.fromList [(s.object, s) | s <- steps, s.key.aspect == Definition])
    gone :: Step -> Bool
    gone s = case s.change of
        Dropped _ -> True
        _ -> s.role == Drops
    selects :: [Text]
    selects =
        [ [S.i|select #{columns} from #{table} where false;|]
        | (t, cs) <- Map.toList (Map.fromListWith (flip (<>)) [(t, [c]) | s <- final, not (gone s), Column t c <- [s.object]])
        , let columns = T.intercalate ", " (map quoted cs)
              table = qualified t
        ]
    asserts = concatMap check final
    check :: Step -> [Text]
    check s
        -- Dropped to be created again by a later change: sqitch verify runs every
        -- change's script on the final database, where it exists again.
        | gone s, Changed _ _ <- s.change = []
        | otherwise = checkObject s
    checkObject :: Step -> [Text]
    checkObject s = case s.object of
        Column t col
            | gone s -> [notExists [S.i|select from pg_attribute where attrelid = #{reg t} and attname = #{literal c} and not attisdropped|] [S.i|column #{c} is still on #{qualified t}|]]
            | Just new <- asColumn =<< after s.change ->
                let old = asColumn =<< before s.change
                    differs f = fmap f old /= Just (f new)
                    ty = typeText new.colType
                    attribute :: Text -> Text
                    attribute what = [S.i|(select #{what} from pg_attribute where attrelid = #{reg t} and attname = #{literal c})|]
                 in [ [S.i|assert #{attribute "format_type(atttypid, atttypmod)"} = #{literal ty}, #{literal (c <> " is not " <> ty)};|]
                    | differs (.colType)
                    ]
                        <> [ let notNull = if isNotNull new then "true" else "false" :: Text
                                 message = c <> (if isNotNull new then " is nullable" else " is not null")
                              in [S.i|assert #{attribute "attnotnull"} = #{notNull}, #{literal message};|]
                           | differs isNotNull
                           ]
                        <> [ let name = collationSql co
                              in [S.i|assert #{attribute "attcollation"} = #{literal name}::regcollation, #{literal (c <> " is not collated " <> name)};|]
                           | differs (.collation)
                           , Just co <- [new.collation]
                           ]
          where
            c = nameText col
        TableConstraint t con | s.role == BuildsIndex -> [relation s (QName t.schema con)]
        TableConstraint t con -> let c = nameText con in [presence s [S.i|select from pg_constraint where conrelid = #{reg t} and conname = #{literal c}|] [S.i|constraint #{c} on #{qualified t}|]]
        Policy t name -> let n = nameText name in [presence s [S.i|select from pg_policy where polrelid = #{reg t} and polname = #{literal n}|] [S.i|policy #{n} on #{qualified t}|]]
        Trigger t name -> let n = nameText name in [presence s [S.i|select from pg_trigger where tgrelid = #{reg t} and tgname = #{literal n} and not tgisinternal|] [S.i|trigger #{n} on #{qualified t}|]]
        DomainConstraint d con ->
            let c = nameText con
                domain = literal (typeText d)
             in [presence s [S.i|select from pg_constraint where contypid = to_regtype(#{domain}) and conname = #{literal c}|] [S.i|constraint #{c} on domain #{typeText d}|]]
        Index q ->
            relation s q
                : [ [S.i|assert (select indisvalid from pg_index where indexrelid = #{reg q}), #{literal message};|]
                  | not (gone s)
                  , Just (IndexDefinition def) <- [after s.change]
                  , def.partitioned
                  , let message = [S.i|#{qualified q} is invalid: a partition has no index attached to it|] :: Text
                  ]
        Table q -> [relation s q]
        View q -> [relation s q]
        MaterializedView q -> [relation s q]
        Sequence q -> [relation s q]
        Routine _ q args ->
            let signature = [S.i|#{qualified q}(#{T.intercalate ", " (map typeText args)})|]
             in [lookupBy s "to_regprocedure" signature]
        Type t ->
            lookupBy s "to_regtype" (typeText t)
                : [ [S.i|assert exists (select from pg_enum where enumtypid = to_regtype(#{literal (typeText t)}) and enumlabel = #{literal label}), #{literal ("label " <> label <> " is missing from " <> typeText t)};|]
                  | not (gone s)
                  , Just (TypeDefinition (EnumType labels)) <- [after s.change]
                  , label <- labels
                  ]
        Schema n -> let name = nameText n in [presence s [S.i|select from pg_namespace where nspname = #{literal name}|] [S.i|schema #{name}|]]
        Extension e -> let name = nameText e in [presence s [S.i|select from pg_extension where extname = #{literal name}|] [S.i|extension #{name}|]]
        _ -> []
    -- Whether a regclass-like lookup finds the object, as it should.
    lookupBy :: Step -> Text -> Text -> Text
    lookupBy s function name =
        let stillThere = literal [S.i|#{name} still exists|]
            missing = literal [S.i|#{name} is missing|]
         in if gone s
                then [S.i|assert #{function}(#{literal name}) is null, #{stillThere};|]
                else [S.i|assert #{function}(#{literal name}) is not null, #{missing};|]
    relation :: Step -> QName -> Text
    relation s q = lookupBy s "to_regclass" (qualified q)
    presence :: Step -> Text -> Text -> Text
    presence s query what =
        let missing = literal [S.i|#{what} is missing|]
         in if gone s then notExists query [S.i|#{what} still exists|] else [S.i|assert exists (#{query}), #{missing};|]
    notExists :: Text -> Text -> Text
    notExists query message = [S.i|assert not exists (#{query}), #{literal message};|]
    reg :: QName -> Text
    reg t = let name = literal (qualified t) in [S.i|#{name}::regclass|]
    assertBlock
        | null asserts = []
        | otherwise =
            let block = doBlock (T.intercalate "\n" ([""] <> ["    begin"] <> map ("        " <>) asserts <> ["    end", "    "]))
             in [[S.i|    #{block};|]]
