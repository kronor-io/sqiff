{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Splits a plan into sqitch changes and writes them with sqitch's own commands.
module Sqiff.Sqitch (
    UnitKind (..),
    Placement (..),
    PreviousFile (..),
    Unit (..),
    SqitchPlan (..),
    parsePlan,
    plannedChanges,
    deployedChanges,
    readPlan,
    requireSqitch,
    reworkEdited,
    moveToEnd,
    units,
    writeUnits,
    renderScript,
    tidyConstraint,
    tidyIndex,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.String.Interpolate qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Hasql.TH (vectorStatement)
import RIO
import RIO.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import RIO.FilePath ((<.>), (</>))
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Catalog (errorText, failWith)
import Sqiff.Config (Names (..), fill)
import Sqiff.Definition
import Sqiff.Diff (Change (..))
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (invert)
import Sqiff.Recipe
import Sqiff.Timing (timed)
import System.Process.Typed (proc, runProcess, setWorkingDir)


data UnitKind = AddChange | ReworkChange
    deriving stock (Eq, Show)


-- | Where a change lives: a time-stamped migration, or kronor's path for its object.
data Placement = Migration | Conventional
    deriving stock (Eq, Show)


-- | Whether the file a rework replaces can still run on the edited table, as its revert.
data PreviousFile = PreviousFileRuns | PreviousFileFails
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


data SqitchPlan = SqitchPlan
    { project :: Text
    , changes :: [Text]
    , tagged :: Set Text
    -- ^ Changes with a tag somewhere after their last entry.
    }


readPlan :: FilePath -> IO SqitchPlan
readPlan path = do
    exists <- doesFileExist path
    unless exists $ failWith [S.i|#{path} does not exist; run from the project root or pass --project and --plan-file|]
    parsePlan <$> readFileUtf8 path


parsePlan :: Text -> SqitchPlan
parsePlan text =
    let lines' = T.lines text
        entries = [l | l <- lines', Just (c, _) <- [T.uncons l], c `notElem` ("%#" :: String)]
        isTag = ("@" `T.isPrefixOf`)
        entryName = T.takeWhile (\c -> c /= ' ' && c /= '[')
        fromEnd _ _ [] = []
        fromEnd seenTag visited (l : ls)
            | isTag l = fromEnd True visited ls
            | entryName l `Set.member` visited = fromEnd seenTag visited ls
            | otherwise = (entryName l, seenTag) : fromEnd seenTag (Set.insert (entryName l) visited) ls
     in SqitchPlan
            { project = fromMaybe "" (listToMaybe (mapMaybe (T.stripPrefix "%project=") lines'))
            , changes = [entryName l | l <- entries, not (isTag l)]
            , tagged = Set.fromList [n | (n, True) <- fromEnd False Set.empty (reverse entries)]
            }


-- | The plan's changes in order, each with its planned time as the plan writes it.
plannedChanges :: Text -> [(Text, Text)]
plannedChanges text =
    [ (T.takeWhile (\ch -> ch /= ' ' && ch /= '[') l, planned)
    | l <- T.lines text
    , Just (c, _) <- [T.uncons l]
    , c `notElem` ("%#@" :: String)
    , Just planned <- [L.find timestamp (T.words l)]
    ]
  where
    timestamp w = T.length w == 20 && "Z" `T.isSuffixOf` w && T.take 1 (T.drop 10 w) == "T"


-- | The changes a database's sqitch registry has deployed for @project@, as 'plannedChanges' names them.
deployedChanges :: Connection.Connection -> Text -> IO (Set (Text, Text))
deployedChanges conn project =
    Connection.use conn (Session.statement project query)
        >>= either (\e -> failWith [S.i|could not read the sqitch registry: #{errorText e}|]) (pure . Set.fromList . toList)
  where
    query =
        [vectorStatement|
            select change::text, to_char(planned_at at time zone 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"')::text
            from sqitch.changes
            where project = $1::text
        |]


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

    createNote s = case (s.object, printed <$> s.change) of
        (Index ix, Added def) | Just t <- indexTableName def -> let table = dotted (QName ix.schema (identifier t)); name = nameText ix.name in [S.i|add_index: add index #{table}.#{name}|]
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
    | otherwise = case (s.object, printed <$> s.change) of
        (Table t, Added _) | s.rank == 4 -> Just (path names.table t)
        (Routine Aggregate _ _, _) -> Nothing
        (Routine _ q _, Added _) -> Just (path names.function q)
        (Routine _ q _, Changed _ _) -> Just (path names.function q)
        (View v, Added _) -> Just (path names.view v)
        (View v, Changed _ _) -> Just (path names.view v)
        (MaterializedView v, Added _) | s.phase == CreatePhase -> Just (path names.view v)
        (Trigger t n, Added _) -> Just (triggerPath t n)
        (Trigger t n, Changed _ _) | "CREATE OR REPLACE TRIGGER" `T.isPrefixOf` s.sql -> Just (triggerPath t n)
        (Index ix, Added def) | Just t <- indexTableName def -> Just (fill names.index [("schema", nameText ix.schema), ("table", t), ("name", nameText ix.name)])
        (Type t, Added _) | [schema, name] <- T.splitOn "." (typeText t) -> Just (fill names.type_ [("schema", schema), ("name", name)])
        _ -> Nothing
  where
    path pattern q = fill pattern [("schema", nameText q.schema), ("name", nameText q.name)]
    triggerPath t n = fill names.trigger [("schema", nameText t.schema), ("table", nameText t.name), ("name", nameText n)]


-- | The unquoted name of the table an index definition is on.
indexTableName :: Text -> Maybe Text
indexTableName def =
    let afterOn = T.drop 4 (snd (T.breakOn " ON " def))
        qualifiedTable = fst (T.breakOn " USING " (fromMaybe afterOn (T.stripPrefix "ONLY " afterOn)))
     in case T.splitOn "." qualifiedTable of
            [_, t] | not ("\"" `T.isInfixOf` t) -> Just t
            _ -> Nothing


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
        , Added def <- printed <$> s.change
        , Just t <- indexTableName def ->
            AddIndexShape (QName ix.schema (identifier t)) ix.name
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
        T.replace "CREATE UNIQUE INDEX CONCURRENTLY " "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS "
            . T.replace "CREATE INDEX CONCURRENTLY " "CREATE INDEX CONCURRENTLY IF NOT EXISTS "
            . T.replace "create unique index concurrently " "create unique index concurrently if not exists "
            . T.replace "drop index concurrently " "drop index concurrently if exists "
            . T.replace "drop index concurrently if exists " "drop index concurrently "
            $ sql
    hazardNotes s = map hazardText s.hazards


-- | pg_get_constraintdef's keywords in lower case, leaving a check's expression alone.
tidyConstraint :: Text -> Text
tidyConstraint clause = case T.stripPrefix "add constraint " clause of
    Just rest ->
        let (name, def) = T.breakOn " " rest
            lowered
                | "FOREIGN KEY " `T.isPrefixOf` T.stripStart def = replaceAll foreignKey def
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


replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll pairs t = foldl' (\acc (old, new) -> T.replace old new acc) t pairs


-- | pg_get_indexdef's keywords in lower case, without the default @USING btree@.
tidyIndex :: Text -> Text
tidyIndex line
    | "CREATE " `T.isPrefixOf` line && " INDEX " `T.isInfixOf` line =
        let (front, back) = T.breakOn " USING " line
            method = T.drop 7 back
            (name, columns) = T.breakOn " " method
            using = if name == "btree" then "" else [S.i| using #{name}|]
         in replaceAll keywords front <> using <> replaceAll tail' columns
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
    T.intercalate "\n\n" ([header, "BEGIN;"] <> map ("    " <>) selects <> doBlock <> ["ROLLBACK;"]) <> "\n"
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
            | gone s -> [notExists [S.i|select from pg_attribute where attrelid = #{reg t} and attname = #{sqlLiteral c} and not attisdropped|] [S.i|column #{c} is still on #{qualified t}|]]
            | Changed (ColumnDefinition old) (ColumnDefinition new) <- s.change
            , let ty = typeText new.colType
            , old.colType /= new.colType ->
                [ let message = sqlLiteral [S.i|#{c} is not #{ty}|]
                   in [S.i|assert (select atttypid from pg_attribute where attrelid = #{reg t} and attname = #{sqlLiteral c}) = #{sqlLiteral ty}::regtype, #{message};|]
                ]
          where
            c = nameText col
        TableConstraint t con | "create unique index " `T.isInfixOf` T.toLower s.sql -> [relation s (QName t.schema con)]
        TableConstraint t con -> let c = nameText con in [presence s [S.i|select from pg_constraint where conrelid = #{reg t} and conname = #{sqlLiteral c}|] [S.i|constraint #{c} on #{qualified t}|]]
        Policy t name -> let n = nameText name in [presence s [S.i|select from pg_policy where polrelid = #{reg t} and polname = #{sqlLiteral n}|] [S.i|policy #{n} on #{qualified t}|]]
        Trigger t name -> let n = nameText name in [presence s [S.i|select from pg_trigger where tgrelid = #{reg t} and tgname = #{sqlLiteral n} and not tgisinternal|] [S.i|trigger #{n} on #{qualified t}|]]
        Index q -> [relation s q]
        View q -> [relation s q]
        MaterializedView q -> [relation s q]
        Sequence q -> [relation s q]
        _ -> []
    relation :: Step -> QName -> Text
    relation s q =
        let name = qualified q
            stillThere = sqlLiteral [S.i|#{name} still exists|]
            missing = sqlLiteral [S.i|#{name} is missing|]
         in if gone s
                then [S.i|assert to_regclass(#{sqlLiteral name}) is null, #{stillThere};|]
                else [S.i|assert to_regclass(#{sqlLiteral name}) is not null, #{missing};|]
    presence :: Step -> Text -> Text -> Text
    presence s query what =
        let missing = sqlLiteral [S.i|#{what} is missing|]
         in if gone s then notExists query [S.i|#{what} still exists|] else [S.i|assert exists (#{query}), #{missing};|]
    notExists :: Text -> Text -> Text
    notExists query message = [S.i|assert not exists (#{query}), #{sqlLiteral message};|]
    reg :: QName -> Text
    reg t = let name = sqlLiteral (qualified t) in [S.i|#{name}::regclass|]
    doBlock
        | null asserts = []
        | otherwise = [T.intercalate "\n" (["    do $$", "    begin"] <> map ("        " <>) asserts <> ["    end", "    $$;"])]


sqlLiteral :: Text -> Text
sqlLiteral t = let escaped = T.replace "'" "''" t in [S.i|'#{escaped}'|]


-- | Moves the last plan entry for @name@, one the branch added, to the end of the plan.
moveToEnd :: FilePath -> Text -> IO ()
moveToEnd path name = do
    text <- readFileUtf8 path
    let ls = reverse (dropWhile (T.null . T.strip) (reverse (T.lines text)))
        isEntry l = T.takeWhile (\c -> c /= ' ' && c /= '[') l == name
    case L.lastMaybe [k | (k, l) <- zip [0 :: Int ..] ls, isEntry l] of
        Just k | k < length ls - 1 -> writeFileUtf8 path (T.unlines (take k ls <> drop (k + 1) ls <> take 1 (drop k ls)))
        _ -> pure ()


requireSqitch :: IO ()
requireSqitch = do
    found <- findExecutable "sqitch"
    when (isNothing found) $ failWith "sqitch is not on PATH; load the nix environment (direnv) in this shell first"


-- | Writes each change with @sqitch add@ or @sqitch rework@, from create-migration's
-- templates in @templateDir@ when it is set, adding a tag first when a rework needs one.
writeUnits :: Maybe FilePath -> FilePath -> FilePath -> SqitchPlan -> Text -> [Unit] -> IO ()
writeUnits templateDir' root topDir sqitchPlan tag us = do
    requireSqitch
    templates <- maybe (pure False) doesDirectoryExist templateDir
    when (any (\u -> u.kind == ReworkChange && u.changeName `Set.notMember` sqitchPlan.tagged) us)
        $ sqitch ["tag", tag, "-n", "sqiff: before the changes it writes"]
    for_ us \u -> do
        let template = if templates && u.kind == AddChange then templateOf u else Nothing
            project = sqitchPlan.project
            name = u.changeName
            deploy = script "deploy" [S.i|-- Deploy #{project}:#{name} to pg|] u.deploy u
            revert = script "revert" [S.i|-- Revert #{project}:#{name} from pg|] u.revert u
            verify = writeFileUtf8 (scriptPath "verify" u) (verifyScript [S.i|-- Verify #{project}:#{name} on pg|] (if u.placement == Migration then u.deploy else []))
        case u.kind of
            AddChange -> sqitch (["add", u.changeName, "-n", u.note] <> foldMap templateArgs template)
            ReworkChange -> sqitch ["rework", u.changeName, "-n", u.note]
        case (template, shapeOf u) of
            (Just _, ValidateShape _ _) -> pure ()
            (Just _, AddColumnShape _ _ options) -> do
                rendered <- readFileUtf8 (scriptPath "deploy" u)
                let hazards = T.unlines [[S.i|-- #{hazardText h}|] | s <- u.deploy, h <- s.hazards]
                writeFileUtf8 (scriptPath "deploy" u)
                    . T.replace "__your_type_here__ null" options
                    . T.replace "\nalter table " [S.i|\n#{hazards}alter table |]
                    $ rendered
            (Just (_, vars), AddIndexShape t index)
                | any (\s -> EveryPartition `elem` s.hazards) u.deploy -> do
                    deploy >> revert
                    writeFileUtf8 (scriptPath "verify" u) (verifyScript [S.i|-- Verify #{project}:#{name} on pg|] u.deploy)
                | otherwise -> do
                    deploy
                    let ix = nameText index
                        templated = indexPrefix t <> fromMaybe ix (lookup "index_name" vars)
                    for_ ["revert", "verify"] \dir -> do
                        rendered <- readFileUtf8 (scriptPath dir u)
                        writeFileUtf8 (scriptPath dir u) (T.replace templated ix rendered)
            (Just _, OtherShape) -> deploy >> revert >> verify
            (Nothing, _) -> do
                deploy
                when (u.kind == AddChange) (revert >> verify)
  where
    sqitch = runSqitch root
    templateDir = (root </>) <$> templateDir'
    templateArgs (template, vars) =
        ["--template", template] <> foldMap (\d -> ["--template-directory", T.pack d]) templateDir <> concat [["-s", [S.i|#{k}=#{v}|]] | (k, v) <- vars]
    scriptPath dir u = root </> topDir </> dir </> T.unpack u.changeName <.> "sql"
    script dir header steps u
        | null steps = writeFileUtf8 (scriptPath dir u) [S.i|#{header}\n\n-- Nothing to revert.\n|]
        | otherwise = writeFileUtf8 (scriptPath dir u) (renderScript header u.placement steps)


-- | Reworks a change whose deploy script was already edited in place, putting the edit
-- back whatever happens; with 'PreviousFileFails' its revert does nothing.
reworkEdited :: FilePath -> FilePath -> SqitchPlan -> Text -> Text -> Text -> Text -> PreviousFile -> IO ()
reworkEdited root topDir sqitchPlan tag changeName note original previousFile = do
    let path = root </> topDir </> "deploy" </> T.unpack changeName <.> "sql"
        revertPath = root </> topDir </> "revert" </> T.unpack changeName <.> "sql"
    edited <- readFileUtf8 path
    ( do
            writeFileUtf8 path original
            unless (changeName `Set.member` sqitchPlan.tagged)
                $ runSqitch root ["tag", tag, "-n", "sqiff: before the changes it writes"]
            runSqitch root ["rework", changeName, "-n", note]
        )
        `finally` writeFileUtf8 path edited
    let project = sqitchPlan.project
    when (previousFile == PreviousFileFails)
        $ writeFileUtf8
            revertPath
            [S.i|-- Revert #{project}:#{changeName} from pg

-- The migrations before this change revert the table; the previous file fails on the edited table.
|]


runSqitch :: FilePath -> [Text] -> IO ()
runSqitch root args = do
    let command = T.unwords (take 1 args)
    code <- timed [S.i|sqitch #{command}|] $ runProcess (setWorkingDir root (proc "sqitch" (map T.unpack args)))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [S.i|sqitch #{T.unwords args} failed with exit code #{n}; changes before it were written|]
