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
    Shape (..),
    shapeOf,
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
import Sqiff.Diff (Change (..))
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (invert)
import Sqiff.Recipe
import Sqiff.Sqitch.Plan (SqitchPlan (..))
import Sqiff.Sql (doBlock, literal, replaceAllInCode)


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
-- @conventional@, reworks of changes the plan has, and migrations for the rest.
units :: Names -> (Step -> Bool) -> Context -> SqitchPlan -> UTCTime -> [Step] -> [Unit]
units names conventional inverseCtx sqitchPlan now steps = nameMigrations (0 :: Int) (claimReverts Set.empty (foldr (merge . single) [] steps))
  where
    existing = Set.fromList sqitchPlan.changes

    single s = case if conventional s then conventionalName names s else Nothing of
        Just n
            | n `Set.member` existing -> Unit ReworkChange n (note "update" s) Conventional [s] []
            | isAdded s.change -> Unit AddChange n (createNote s) Conventional [s] []
        _ -> Unit AddChange (groupKey s) (note (verb s) s) Migration [s] []

    createNote s = case (s.object, s.change) of
        (Index ix, Added (IndexDefinition def)) | Just t <- plainTable def -> let table = dotted (QName ix.schema t); name = nameText ix.name in [S.i|add_index: add index #{table}.#{name}|]
        _ -> note "create" s

    merge u (next : rest)
        | u.placement == Migration && next.placement == Migration && u.changeName == next.changeName && all sharable (u.deploy <> next.deploy) && samePhase (u.deploy <> next.deploy) =
            u{deploy = u.deploy <> next.deploy, note = u.note} : rest
    merge u rest = u : rest

    sharable s = s.txn == Shared
    samePhase ss = length (nubOrd (map (.phase) ss)) <= 1

    claimReverts _ [] = []
    claimReverts claimed (u : rest) =
        let keys = nubOrd [s.key | s <- u.deploy, s.key `Set.notMember` claimed]
            inverse = map refill (concat [recipe inverseCtx k (invert c) | k <- reverse keys, Just c <- [lookup k [(s.key, s.change) | s <- u.deploy]]])
         in u{revert = if u.kind == ReworkChange then [] else inverse} : claimReverts (claimed <> Set.fromList keys) rest

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

    nameMigrations _ [] = []
    nameMigrations i (u : rest)
        | u.placement == Migration =
            let stamp = T.pack (formatTime defaultTimeLocale (T.unpack names.migrationStamp) (addUTCTime (fromIntegral i) now))
                description = describe u
                template = maybe "migration" fst (templateOf u)
             in u{changeName = fill names.migration [("stamp", stamp), ("slug", slug description)], note = [S.i|#{template}: #{description}|]} : nameMigrations (i + 1) rest
        | otherwise = u : nameMigrations i rest

    describe u = case shapeOf u of
        AddColumnShape t c _ -> let table = dotted t; column = nameText c in [S.i|add column #{table}.#{column}|]
        ValidateShape t c -> let table = dotted t; constraint = nameText c in [S.i|validate constraint #{table}.#{constraint}|]
        _ ->
            let objects = nubOrd [fromMaybe (render s.object) (L.lastMaybe (toAddress s.object).objectNames) | s <- u.deploy]
                joined = T.intercalate "-" objects
                suffix = if objects == maybeToList (L.lastMaybe (T.split (== '.') u.changeName)) then "" else [S.i|.#{joined}|] :: Text
                v = verbOf u
                name = u.changeName
             in [S.i|#{v} #{name}#{suffix}|] :: Text

    verbOf :: Unit -> Text
    verbOf u = maybe "alter" verb (listToMaybe u.deploy)

    verb :: Step -> Text
    verb s
        | s.key.aspect == Privileges = "grant"
        | s.key.aspect == Comment = "comment"
        | " rename column " `T.isInfixOf` s.sql = "rename"
        | s.phase == DropPhase = "drop"
        | isAdded s.change = "add"
        | otherwise = "alter"

    note :: Text -> Step -> Text
    note v s = let what = render s.object in [S.i|sqiff: #{v} #{what}|]

    isAdded = \case Added _ -> True; _ -> False

    groupKey s = case owningTable s.object of
        Just t -> dotted t
        Nothing -> T.intercalate "." (toAddress s.object).objectNames


-- | Where the project keeps a new object of this kind, for the steps that create or replace one.
conventionalName :: Names -> Step -> Maybe Text
conventionalName names s
    | s.key.aspect /= Definition = Nothing
    | otherwise = case (s.object, s.change) of
        (Table t, Added _) | s.rank == 4 -> Just (path names.table t)
        (Routine Aggregate _ _, _) -> Nothing
        (Routine _ q _, Added _) -> Just (path names.function q)
        (Routine _ q _, Changed _ _) -> Just (path names.function q)
        (View v, Added _) -> Just (path names.view v)
        (View v, Changed _ _) -> Just (path names.view v)
        (MaterializedView v, Added _) | s.phase == CreatePhase -> Just (path names.view v)
        (Trigger t n, Added _) -> Just (triggerPath t n)
        (Trigger t n, Changed _ _) | "CREATE OR REPLACE TRIGGER" `T.isPrefixOf` s.sql -> Just (triggerPath t n)
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
        , Just options <- T.stripPrefix [S.i|alter table #{qualified t} add column #{quoted c} |] s.sql
        , not (any (`elem` [MayRewriteTable, RewritesTable, LongLock]) s.hazards) ->
            AddColumnShape t c options
        | u.placement == Migration
        , Just t <- owningTable s.object
        , plain t
        , Just c <- T.stripPrefix [S.i|alter table #{qualified t} validate constraint |] s.sql
        , quoteIdent c == c ->
            ValidateShape t (identifier c)
    _ -> OtherShape
  where
    plain t = qualified t == dotted t


-- | The add_index template names an index idx_<table>_<index_name>.
indexPrefix :: QName -> Text
indexPrefix t = let name = nameText t.name in [S.i|idx_#{name}_|]


-- | The template and its variables, as create-migration would pass them.
templateOf :: Unit -> Maybe (Text, [(Text, Text)])
templateOf u = case shapeOf u of
    AddColumnShape t c _ -> Just ("add_column", [("schema", nameText t.schema), ("table_name", nameText t.name), ("column_name", nameText c)])
    ValidateShape t c -> Just ("validate_constraint", [("schema", nameText t.schema), ("table_name", nameText t.name), ("constraint_name", nameText c)])
    AddIndexShape t ix -> Just ("add_index", [("schema", nameText t.schema), ("table_name", nameText t.name), ("index_name", fromMaybe (nameText ix) (T.stripPrefix (indexPrefix t) (nameText ix)))])
    OtherShape | u.placement == Migration -> Just ("migration", [])
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
                $ ["BEGIN;"]
                <> ["  -- Do not remove this line\n  select set_config('lock_timeout', '50ms', false);" | placement == Migration]
                <> map (\s -> if safeToIndent s.sql then indent (statement s.sql s) else statement s.sql s) group'
                <> ["COMMIT;"]
    statement sql s =
        T.intercalate "\n" (map ("-- " <>) (hazardNotes s) <> [tidy s sql <> if "--" `T.isPrefixOf` sql then "" else ";"])
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
-- behind.
verifyScript :: Text -> [Step] -> Text
verifyScript header steps =
    T.intercalate "\n\n" ([header, "BEGIN;"] <> map ("    " <>) selects <> assertBlock <> ["ROLLBACK;"]) <> "\n"
  where
    final = Map.elems (Map.fromList [(s.object, s) | s <- steps, s.key.aspect == Definition])
    gone :: Step -> Bool
    gone s = case s.change of
        Dropped _ -> True
        _
            | " rename " `T.isInfixOf` s.sql -> False
            | s.phase == DropPhase -> True
            | Index _ <- s.object -> "drop index " `T.isPrefixOf` s.sql
            | otherwise -> False
    selects :: [Text]
    selects =
        [ [S.i|select #{columns} from #{table} where false;|]
        | (t, cs) <- Map.toList (Map.fromListWith (flip (<>)) [(t, [c]) | s <- final, not (gone s), Column t c <- [s.object]])
        , let columns = T.intercalate ", " (map quoted cs)
              table = qualified t
        ]
    asserts = concatMap check final
    check :: Step -> [Text]
    check s = case s.object of
        Column t col
            | gone s -> [notExists [S.i|select from pg_attribute where attrelid = #{reg t} and attname = #{literal c} and not attisdropped|] [S.i|column #{c} is still on #{qualified t}|]]
            | Changed (ColumnDefinition old) (ColumnDefinition new) <- s.change
            , let ty = typeText new.colType
            , old.colType /= new.colType ->
                [ let message = literal [S.i|#{c} is not #{ty}|]
                   in [S.i|assert (select atttypid from pg_attribute where attrelid = #{reg t} and attname = #{literal c}) = #{literal ty}::regtype, #{message};|]
                ]
          where
            c = nameText col
        TableConstraint t con | "create unique index " `T.isInfixOf` T.toLower s.sql -> [relation s (QName t.schema con)]
        TableConstraint t con -> let c = nameText con in [presence s [S.i|select from pg_constraint where conrelid = #{reg t} and conname = #{literal c}|] [S.i|constraint #{c} on #{qualified t}|]]
        Policy t name -> let n = nameText name in [presence s [S.i|select from pg_policy where polrelid = #{reg t} and polname = #{literal n}|] [S.i|policy #{n} on #{qualified t}|]]
        Trigger t name -> let n = nameText name in [presence s [S.i|select from pg_trigger where tgrelid = #{reg t} and tgname = #{literal n} and not tgisinternal|] [S.i|trigger #{n} on #{qualified t}|]]
        Index q -> [relation s q]
        View q -> [relation s q]
        MaterializedView q -> [relation s q]
        Sequence q -> [relation s q]
        _ -> []
    relation :: Step -> QName -> Text
    relation s q =
        let name = qualified q
            stillThere = literal [S.i|#{name} still exists|]
            missing = literal [S.i|#{name} is missing|]
         in if gone s
                then [S.i|assert to_regclass(#{literal name}) is null, #{stillThere};|]
                else [S.i|assert to_regclass(#{literal name}) is not null, #{missing};|]
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
        | otherwise = ["    " <> doBlock (T.intercalate "\n" ([""] <> ["    begin"] <> map ("        " <>) asserts <> ["    end", "    "])) <> ";"]
