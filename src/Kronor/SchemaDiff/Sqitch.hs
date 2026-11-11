{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Splits a plan into sqitch changes and writes them with sqitch's own commands.
module Kronor.SchemaDiff.Sqitch (
    UnitKind (..),
    Unit (..),
    SqitchPlan (..),
    parsePlan,
    readPlan,
    requireSqitch,
    reworkEdited,
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
import RIO
import RIO.Directory (doesFileExist, findExecutable)
import RIO.FilePath ((<.>), (</>))
import RIO.List qualified as L
import RIO.Set qualified as Set
import System.Process.Typed (proc, runProcess, setWorkingDir)


data UnitKind = AddChange | ReworkChange
    deriving stock (Eq, Show)


-- | One sqitch change. A rework keeps the revert sqitch gives it: the previous version.
data Unit = Unit
    { kind :: UnitKind
    , changeName :: Text
    , note :: Text
    , migration :: Bool
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
-- With @conventional@ off, every step goes into a migration.
units :: Bool -> Context -> SqitchPlan -> UTCTime -> [Step] -> [Unit]
units conventional inverseCtx sqitchPlan now steps = nameMigrations (0 :: Int) (claimReverts Set.empty (foldr (merge . single) [] steps))
  where
    existing = Set.fromList sqitchPlan.changes

    single s = case if conventional then conventionalName s else Nothing of
        Just n
            | n `Set.member` existing -> Unit ReworkChange n (note "update" s) False [s] []
            | isAdded s.change -> Unit AddChange n (note "create" s) False [s] []
        _ -> Unit AddChange (groupKey s) (note (verb s) s) True [s] []

    merge u (next : rest)
        | u.migration && next.migration && u.changeName == next.changeName && all sharable (u.deploy <> next.deploy) && samePhase (u.deploy <> next.deploy) =
            u{deploy = u.deploy <> next.deploy, note = u.note} : rest
    merge u rest = u : rest

    sharable s = s.txn == Shared
    samePhase ss = length (nubOrd (map (.phase) ss)) <= 1

    claimReverts _ [] = []
    claimReverts claimed (u : rest) =
        let keys = nubOrd [s.key | s <- u.deploy, s.key `Set.notMember` claimed]
            inverse = concat [recipe inverseCtx k (invert c) | k <- reverse keys, Just c <- [lookup k [(s.key, s.change) | s <- u.deploy]]]
         in u{revert = if u.kind == ReworkChange then [] else inverse} : claimReverts (claimed <> Set.fromList keys) rest

    nameMigrations _ [] = []
    nameMigrations i (u : rest)
        | u.migration =
            let stamp = formatTime defaultTimeLocale "%Y/%m/%Y-%m-%d-%H%M%S" (addUTCTime (fromIntegral i) now)
             in u{changeName = "migrations/" <> T.pack stamp <> "-" <> slug (verbOf u <> "-" <> u.changeName)} : nameMigrations (i + 1) rest
        | otherwise = u : nameMigrations i rest

    verbOf u = maybe "alter" verb (listToMaybe u.deploy)

    verb s
        | s.key.aspect == Privileges = "grant"
        | s.key.aspect == Comment = "comment"
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
        (Index i, Added def) | Just t <- indexTable def -> Just (i.schema <> "/table/" <> t <> "/index/" <> i.name)
        (Type t, Added _) | [schema, name'] <- T.splitOn "." t -> Just (schema <> "/type/" <> name')
        _ -> Nothing
  where
    path q kind' = q.schema <> "/" <> kind' <> "/" <> q.name
    indexTable def =
        let afterOn = T.drop 4 (snd (T.breakOn " ON " def))
            qualifiedTable = fst (T.breakOn " USING " (fromMaybe afterOn (T.stripPrefix "ONLY " afterOn)))
         in case T.splitOn "." qualifiedTable of
                [_, t] | not ("\"" `T.isInfixOf` t) -> Just t
                _ -> Nothing


slug :: Text -> Text
slug = T.take 80 . T.intercalate "-" . filter (not . T.null) . T.split (not . allowed) . T.toLower
  where
    allowed c = isAsciiLower c || isDigit c || c == '_' || c == '.'


-- | A script: shared steps in one transaction, others in their own or none.
renderScript :: Text -> Bool -> [Step] -> Text
renderScript header migration steps =
    T.intercalate "\n" (header : "" : map block (L.groupBy together steps)) <> "\n"
  where
    together a b = a.txn == Shared && b.txn == Shared
    block group' = case group' of
        [s] | s.txn == None -> statement (idempotent s.sql) s <> "\n"
        _ ->
            T.unlines
                $ ["BEGIN;", ""]
                <> ["select set_config('lock_timeout', '50ms', false);\n" | migration]
                <> map (\s -> statement s.sql s) group'
                <> ["COMMIT;"]
    statement sql s =
        T.unlines (map ("-- " <>) (hazardNotes s)) <> sql <> if "--" `T.isPrefixOf` sql then "" else ";"
    idempotent sql =
        T.replace "CREATE UNIQUE INDEX CONCURRENTLY " "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS "
            . T.replace "CREATE INDEX CONCURRENTLY " "CREATE INDEX CONCURRENTLY IF NOT EXISTS "
            . T.replace "create unique index concurrently " "create unique index concurrently if not exists "
            . T.replace "drop index concurrently " "drop index concurrently if exists "
            $ sql
    hazardNotes s =
        map
            ( \case
                DeletesData -> "hazard: deletes data"
                MayRewriteTable -> "hazard: may rewrite the table"
                LongLock -> "hazard: holds a lock that blocks writes while it runs"
                IndexBuild -> "hazard: builds an index"
                MayFail why -> "hazard: " <> why
                Manual why -> "manual: " <> why
            )
            s.hazards


requireSqitch :: IO ()
requireSqitch = do
    found <- findExecutable "sqitch"
    when (isNothing found) $ failWith "sqitch is not on PATH; load the nix environment (direnv) in this shell first"


-- | Adds a tag first when a rework needs one, then each change with @sqitch add@ or
-- @sqitch rework@, and fills in the scripts sqitch created.
writeUnits :: FilePath -> FilePath -> SqitchPlan -> Text -> [Unit] -> IO ()
writeUnits root topDir sqitchPlan tag us = do
    requireSqitch
    when (any (\u -> u.kind == ReworkChange && u.changeName `Set.notMember` sqitchPlan.tagged) us)
        $ sqitch ["tag", tag, "-n", "schema-diff: before the changes it writes"]
    for_ us \u -> do
        case u.kind of
            AddChange -> sqitch ["add", u.changeName, "-n", u.note]
            ReworkChange -> sqitch ["rework", u.changeName, "-n", u.note]
        let project = sqitchPlan.project
            name = u.changeName
        script "deploy" [S.i|-- Deploy #{project}:#{name} to pg|] u.deploy u
        when (u.kind == AddChange) do
            script "revert" [S.i|-- Revert #{project}:#{name} from pg|] u.revert u
            writeFileUtf8 (scriptPath "verify" u) [S.i|-- Verify #{project}:#{name} on pg\n\nBEGIN;\n\nROLLBACK;\n|]
  where
    sqitch = runSqitch root
    scriptPath dir u = root </> topDir </> dir </> T.unpack u.changeName <.> "sql"
    script dir header steps u = writeFileUtf8 (scriptPath dir u) (renderScript header u.migration steps)


-- | Reworks a change whose deploy script was already edited in place: puts back @original@
-- for sqitch to snapshot, reworks, then restores the edit whatever happens.
reworkEdited :: FilePath -> FilePath -> SqitchPlan -> Text -> Text -> Text -> Text -> IO ()
reworkEdited root topDir sqitchPlan tag changeName note original = do
    let path = root </> topDir </> "deploy" </> T.unpack changeName <.> "sql"
    edited <- readFileUtf8 path
    ( do
            writeFileUtf8 path original
            unless (changeName `Set.member` sqitchPlan.tagged)
                $ runSqitch root ["tag", tag, "-n", "schema-diff: before the changes it writes"]
            runSqitch root ["rework", changeName, "-n", note]
        )
        `finally` writeFileUtf8 path edited


runSqitch :: FilePath -> [Text] -> IO ()
runSqitch root args = do
    code <- runProcess (setWorkingDir root (proc "sqitch" (map T.unpack args)))
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> failWith [S.i|sqitch #{T.unwords args} failed with exit code #{n}; changes before it were written|]
