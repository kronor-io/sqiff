{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Splits a plan into sqitch changes and writes them with sqitch's own commands.
module Kronor.SchemaDiff.Sqitch (
    UnitKind (..),
    Placement (..),
    PreviousFile (..),
    Unit (..),
    SqitchPlan (..),
    parsePlan,
    readPlan,
    requireSqitch,
    reworkEdited,
    moveToEnd,
    units,
    writeUnits,
    renderScript,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.String.Interpolate qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Kronor.SchemaDiff.Catalog (Address (..), Aspect (..), ObjectKey (..), failWith, render)
import Kronor.SchemaDiff.Diff (Change (..))
import Kronor.SchemaDiff.Object
import Kronor.SchemaDiff.Plan (invert)
import Kronor.SchemaDiff.Recipe
import Kronor.SchemaDiff.Timing (timed)
import RIO
import RIO.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import RIO.FilePath ((<.>), (</>))
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
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
    unless exists $ failWith (T.pack path <> " does not exist; run from the project root or pass --project and --plan-file")
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
            { project = fromMaybe "kronor" (listToMaybe (mapMaybe (T.stripPrefix "%project=") lines'))
            , changes = [entryName l | l <- entries, not (isTag l)]
            , tagged = Set.fromList [n | (n, True) <- fromEnd False Set.empty (reverse entries)]
            }


-- | Groups ordered steps into changes: new objects at their conventional paths,
-- reworks of changes the plan already has, and time-stamped migrations for the rest.
-- Consecutive steps on the same table that can share a transaction share a change.
-- Only steps that pass @conventional@ get a conventional path; the rest go into migrations.
units :: (Step -> Bool) -> Context -> SqitchPlan -> UTCTime -> [Step] -> [Unit]
units conventional inverseCtx sqitchPlan now steps = nameMigrations (0 :: Int) (claimReverts Set.empty (foldr (merge . single) [] steps))
  where
    existing = Set.fromList sqitchPlan.changes

    single s = case if conventional s then conventionalName s else Nothing of
        Just n
            | n `Set.member` existing -> Unit ReworkChange n (note "update" s) Conventional [s] []
            | isAdded s.change -> Unit AddChange n (createNote s) Conventional [s] []
        _ -> Unit AddChange (groupKey s) (note (verb s) s) Migration [s] []

    createNote s = case (s.object, s.change) of
        (Index ix, Added def) | Just t <- indexTableName def -> "add_index: add index " <> dotted (QName ix.schema t) <> "." <> ix.name
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

    -- A dropped not null column without a default can only come back nullable: its values are gone.
    refill s
        | Column t c <- s.object
        , Added def <- s.change
        , let p = parseColumn def
        , p.notNull && isNothing p.defaultExpr && isNothing p.identity && isNothing p.generated =
            let table = qualified t
                column = quoteIdent c
                definition = columnSql p{notNull = False}
             in s
                    { sql = [S.i|alter table #{table} add column #{column} #{definition}|]
                    , hazards = [MayFail "the column comes back nullable and empty, since its values were dropped"]
                    }
        | otherwise = s

    nameMigrations _ [] = []
    nameMigrations i (u : rest)
        | u.placement == Migration =
            let stamp = formatTime defaultTimeLocale "%Y/%m/%Y-%m-%d-%H%M%S" (addUTCTime (fromIntegral i) now)
                description = describe u
                template = maybe "migration" fst (templateOf u)
             in u{changeName = "migrations/" <> T.pack stamp <> "-" <> slug description, note = template <> ": " <> description} : nameMigrations (i + 1) rest
        | otherwise = u : nameMigrations i rest

    -- The words create-migration uses for the same change.
    describe u = case shapeOf u of
        AddColumnShape t c _ -> "add column " <> dotted t <> "." <> c
        ValidateShape t c -> "validate constraint " <> dotted t <> "." <> c
        _ ->
            let names = nubOrd [fromMaybe (render s.address) (L.lastMaybe s.address.objectNames) | s <- u.deploy]
                suffix = if names == maybeToList (L.lastMaybe (T.split (== '.') u.changeName)) then "" else "." <> T.intercalate "-" names
             in verbOf u <> " " <> u.changeName <> suffix

    verbOf u = maybe "alter" verb (listToMaybe u.deploy)

    verb s
        | s.key.aspect == Privileges = "grant"
        | s.key.aspect == Comment = "comment"
        | " rename column " `T.isInfixOf` s.sql = "rename"
        | s.phase == DropPhase = "drop"
        | isAdded s.change = "add"
        | otherwise = "alter"

    note v s = "schema-diff: " <> v <> " " <> render s.address

    isAdded = \case Added _ -> True; _ -> False

    groupKey s = case owningTable s.object of
        Just t -> t.schema <> "." <> t.name
        Nothing -> T.intercalate "." s.address.objectNames


-- | Where kronor keeps a new object of this kind, for the steps that create or replace one.
conventionalName :: Step -> Maybe Text
conventionalName s
    | s.key.aspect /= Definition = Nothing
    | otherwise = case (s.object, s.change) of
        (Table t, Added _) | s.rank == 4 -> Just (path t "table")
        (Routine Aggregate _ _, _) -> Nothing
        (Routine _ q _, Added _) -> Just (path q "function")
        (Routine _ q _, Changed _ _) -> Just (path q "function")
        (View v, Added _) -> Just (path v "view")
        (View v, Changed _ _) -> Just (path v "view")
        (MaterializedView v, Added _) | s.phase == CreatePhase -> Just (path v "view")
        (Trigger t n, Added _) -> Just (t.schema <> "/trigger/" <> t.name <> "/" <> n)
        (Trigger t n, Changed _ _) | "CREATE OR REPLACE TRIGGER" `T.isPrefixOf` s.sql -> Just (t.schema <> "/trigger/" <> t.name <> "/" <> n)
        (Index i, Added def) | Just t <- indexTableName def -> Just (i.schema <> "/table/" <> t <> "/index/" <> i.name)
        (Type t, Added _) | [schema, name'] <- T.splitOn "." t -> Just (schema <> "/type/" <> name')
        _ -> Nothing
  where
    path q kind' = q.schema <> "/" <> kind' <> "/" <> q.name


-- | The unquoted name of the table an index definition is on.
indexTableName :: Text -> Maybe Text
indexTableName def =
    let afterOn = T.drop 4 (snd (T.breakOn " ON " def))
        qualifiedTable = fst (T.breakOn " USING " (fromMaybe afterOn (T.stripPrefix "ONLY " afterOn)))
     in case T.splitOn "." qualifiedTable of
            [_, t] | not ("\"" `T.isInfixOf` t) -> Just t
            _ -> Nothing


-- | The changes kronor's own templates in @_dev/sqitch/templates@ cover.
data Shape
    = -- | The table, the column, and the column's type and options.
      AddColumnShape QName Text Text
    | ValidateShape QName Text
    | AddIndexShape QName Text
    | OtherShape


shapeOf :: Unit -> Shape
shapeOf u = case u.deploy of
    [s]
        | u.placement == Conventional
        , Index ix <- s.object
        , Added def <- s.change
        , Just t <- indexTableName def ->
            AddIndexShape (QName ix.schema t) ix.name
        | u.placement == Migration
        , Column t c <- s.object
        , Added _ <- s.change
        , plain t
        , quoteIdent c == c
        , Just options <- T.stripPrefix [S.i|alter table #{qualified t} add column #{c} |] s.sql
        , not (any (`elem` [MayRewriteTable, RewritesTable, LongLock]) s.hazards) ->
            AddColumnShape t c options
        | u.placement == Migration
        , Just t <- owningTable s.object
        , plain t
        , Just c <- T.stripPrefix [S.i|alter table #{qualified t} validate constraint |] s.sql
        , quoteIdent c == c ->
            ValidateShape t c
    _ -> OtherShape
  where
    plain t = qualified t == t.schema <> "." <> t.name


dotted :: QName -> Text
dotted t = t.schema <> "." <> t.name


-- | The add_index template names an index idx_<table>_<index_name>.
indexPrefix :: QName -> Text
indexPrefix t = let name = t.name in [S.i|idx_#{name}_|]


-- | The template and its variables, as create-migration would pass them.
templateOf :: Unit -> Maybe (Text, [(Text, Text)])
templateOf u = case shapeOf u of
    AddColumnShape t c _ -> Just ("add_column", [("schema", t.schema), ("table_name", t.name), ("column_name", c)])
    ValidateShape t c -> Just ("validate_constraint", [("schema", t.schema), ("table_name", t.name), ("constraint_name", c)])
    AddIndexShape t ix -> Just ("add_index", [("schema", t.schema), ("table_name", t.name), ("index_name", fromMaybe ix (T.stripPrefix (indexPrefix t) ix))])
    OtherShape | u.placement == Migration -> Just ("migration", [])
    OtherShape -> Nothing


slug :: Text -> Text
slug = T.take 80 . T.intercalate "-" . filter (not . T.null) . T.split (not . allowed) . T.toLower
  where
    allowed c = isAsciiLower c || isDigit c || c == '_' || c == '.'


-- | A script: shared steps in one transaction, others in their own or none.
renderScript :: Text -> Placement -> [Step] -> Text
renderScript header placement steps =
    T.intercalate "\n\n" (header : map block (L.groupBy together steps)) <> "\n"
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
    -- Indenting inside a string or a dollar-quoted body would change its text.
    safeToIndent sql = not ("$" `T.isInfixOf` sql) && all (even . T.count "'") (T.lines sql)
    tidy s sql = case T.lines sql of
        [l]
            | Just rest <- T.stripPrefix "alter table " l
            , (name, more) <- T.breakOn " " rest
            , not ("\"" `T.isInfixOf` name)
            , not (T.null more) ->
                "alter table " <> name <> "\n    " <> tidyConstraint (T.stripStart more)
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
         in "add constraint " <> name <> replaceSuffixes lowered
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
            using = if name == "btree" then "" else " using " <> name
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


-- | A verify script that checks what a migration's statements leave behind: columns,
-- constraints, indexes, policies, triggers and views exist or are gone, and retyped
-- columns have their new type. It reads only the catalog.
verifyScript :: Text -> [Step] -> Text
verifyScript header steps =
    T.intercalate "\n\n" ([header, "BEGIN;"] <> map ("    " <>) selects <> doBlock <> ["ROLLBACK;"]) <> "\n"
  where
    final = Map.elems (Map.fromList [(s.object, s) | s <- steps, s.key.aspect == Definition])
    -- A changed object's drop phase removes the old version; its create phase brings the new one.
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
        , let columns = T.intercalate ", " (map quoteIdent cs)
              table = qualified t
        ]
    asserts = concatMap check final
    check :: Step -> [Text]
    check s = case s.object of
        Column t c
            | gone s -> [notExists [S.i|select from pg_attribute where attrelid = #{reg t} and attname = #{sqlLiteral c} and not attisdropped|] [S.i|column #{c} is still on #{qualified t}|]]
            | Changed old new <- s.change
            , let ty = (parseColumn new).colType
            , (parseColumn old).colType /= ty ->
                [ [S.i|assert (select atttypid from pg_attribute where attrelid = #{reg t} and attname = #{sqlLiteral c}) = #{sqlLiteral ty}::regtype, #{sqlLiteral (c <> " is not " <> ty)};|]
                ]
        -- A new key's index is built in a change before the one that attaches it.
        TableConstraint t c | "create unique index " `T.isInfixOf` T.toLower s.sql -> [relation s (QName t.schema c)]
        TableConstraint t c -> [presence s [S.i|select from pg_constraint where conrelid = #{reg t} and conname = #{sqlLiteral c}|] [S.i|constraint #{c} on #{qualified t}|]]
        Policy t n -> [presence s [S.i|select from pg_policy where polrelid = #{reg t} and polname = #{sqlLiteral n}|] [S.i|policy #{n} on #{qualified t}|]]
        Trigger t n -> [presence s [S.i|select from pg_trigger where tgrelid = #{reg t} and tgname = #{sqlLiteral n} and not tgisinternal|] [S.i|trigger #{n} on #{qualified t}|]]
        Index q -> [relation s q]
        View q -> [relation s q]
        MaterializedView q -> [relation s q]
        Sequence q -> [relation s q]
        _ -> []
    relation :: Step -> QName -> Text
    relation s q =
        let name = qualified q
         in if gone s
                then [S.i|assert to_regclass(#{sqlLiteral name}) is null, #{sqlLiteral (name <> " still exists")};|]
                else [S.i|assert to_regclass(#{sqlLiteral name}) is not null, #{sqlLiteral (name <> " is missing")};|]
    presence :: Step -> Text -> Text -> Text
    presence s query what =
        if gone s then notExists query (what <> " still exists") else [S.i|assert exists (#{query}), #{sqlLiteral (what <> " is missing")};|]
    notExists :: Text -> Text -> Text
    notExists query message = [S.i|assert not exists (#{query}), #{sqlLiteral message};|]
    reg :: QName -> Text
    reg t = sqlLiteral (qualified t) <> "::regclass"
    doBlock
        | null asserts = []
        | otherwise = [T.intercalate "\n" (["    do $$", "    begin"] <> map ("        " <>) asserts <> ["    end", "    $$;"])]


sqlLiteral :: Text -> Text
sqlLiteral t = let escaped = T.replace "'" "''" t in [S.i|'#{escaped}'|]


-- | Moves the last plan entry for @name@ to the end of the plan, after the changes added
-- since. Only for an entry the branch added, since moving a line changes the ids below it.
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


-- | Adds a tag first when a rework needs one, then each change with @sqitch add@ or
-- @sqitch rework@, and fills in the scripts sqitch created. When the project has
-- create-migration's templates, a new change is added from the template create-migration
-- would use, and keeps the scripts that template writes in full.
writeUnits :: FilePath -> FilePath -> SqitchPlan -> Text -> [Unit] -> IO ()
writeUnits root topDir sqitchPlan tag us = do
    requireSqitch
    templates <- doesDirectoryExist templateDir
    when (any (\u -> u.kind == ReworkChange && u.changeName `Set.notMember` sqitchPlan.tagged) us)
        $ sqitch ["tag", tag, "-n", "schema-diff: before the changes it writes"]
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
                let hazards = T.unlines ["-- " <> hazardText h | s <- u.deploy, h <- s.hazards]
                writeFileUtf8 (scriptPath "deploy" u)
                    . T.replace "__your_type_here__ null" options
                    . T.replace "\nalter table " [S.i|\n#{hazards}alter table |]
                    $ rendered
            (Just (_, vars), AddIndexShape t ix)
                -- A partitioned index has no row in pg_stat_user_indexes and can't be
                -- dropped concurrently, so the template's revert and verify don't fit it.
                | any (\s -> EveryPartition `elem` s.hazards) u.deploy -> do
                    deploy >> revert
                    writeFileUtf8 (scriptPath "verify" u) (verifyScript [S.i|-- Verify #{project}:#{name} on pg|] u.deploy)
                | otherwise -> do
                    deploy
                    let templated = indexPrefix t <> fromMaybe ix (lookup "index_name" vars)
                    for_ ["revert", "verify"] \dir -> do
                        rendered <- readFileUtf8 (scriptPath dir u)
                        writeFileUtf8 (scriptPath dir u) (T.replace templated ix rendered)
            (Just _, OtherShape) -> deploy >> revert >> verify
            (Nothing, _) -> do
                deploy
                when (u.kind == AddChange) (revert >> verify)
  where
    sqitch = runSqitch root
    templateDir = root </> "_dev" </> "sqitch" </> "templates"
    templateArgs (template, vars) =
        ["--template", template, "--template-directory", T.pack templateDir] <> concat [["-s", k <> "=" <> v] | (k, v) <- vars]
    scriptPath dir u = root </> topDir </> dir </> T.unpack u.changeName <.> "sql"
    script dir header steps u
        | null steps = writeFileUtf8 (scriptPath dir u) (header <> "\n\n-- Nothing to revert.\n")
        | otherwise = writeFileUtf8 (scriptPath dir u) (renderScript header u.placement steps)


-- | Reworks a change whose deploy script was already edited in place: puts back @original@
-- for sqitch to snapshot, reworks, then restores the edit whatever happens. With
-- 'PreviousFileFails' the reworked change's revert does nothing.
reworkEdited :: FilePath -> FilePath -> SqitchPlan -> Text -> Text -> Text -> Text -> PreviousFile -> IO ()
reworkEdited root topDir sqitchPlan tag changeName note original previousFile = do
    let path = root </> topDir </> "deploy" </> T.unpack changeName <.> "sql"
        revertPath = root </> topDir </> "revert" </> T.unpack changeName <.> "sql"
    edited <- readFileUtf8 path
    ( do
            writeFileUtf8 path original
            unless (changeName `Set.member` sqitchPlan.tagged)
                $ runSqitch root ["tag", tag, "-n", "schema-diff: before the changes it writes"]
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
    code <- timed ("sqitch " <> T.unwords (take 1 args)) $ runProcess (setWorkingDir root (proc "sqitch" (map T.unpack args)))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [S.i|sqitch #{T.unwords args} failed with exit code #{n}; changes before it were written|]
