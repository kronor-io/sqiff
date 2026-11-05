{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SchemaDiff (main) where

import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Hasql.Connection qualified as Connection
import Kronor.SchemaDiff.Body (BodyCheck, checkBodies, sameDefinition)
import Kronor.SchemaDiff.Catalog (Address (..), Aspect (..), ObjectKey (..), UserError (..), connect, dependencies, describe, failWith, render)
import Kronor.SchemaDiff.Diff (Change (..), affected, diff)
import Kronor.SchemaDiff.Folder (writeFolder)
import Kronor.SchemaDiff.Object (QName (..))
import Kronor.SchemaDiff.Plan (context, invert, plan, renderStep)
import Kronor.SchemaDiff.Script (splitStatements, withoutTransactionControl)
import Kronor.SchemaDiff.Sqitch (SqitchPlan (..), Unit (..), UnitKind (..), parsePlan, readPlan, requireSqitch, reworkEdited, units, writeUnits)
import Kronor.SchemaDiff.TableEdit (Baseline (..), TableEdit (..), compareTable)
import Options.Applicative qualified as O
import RIO
import RIO.Directory (makeAbsolute)
import RIO.FilePath (makeRelative, (</>))
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Text qualified as T
import System.Process.Typed (proc, readProcess, setWorkingDir)


data Scope = Scope
    { excluded :: [Text]
    , includePartitions :: Bool
    , checkFunctionBodies :: Bool
    }


bodyChecks :: Scope -> Connection.Connection -> Map ObjectKey Text -> IO (Map ObjectKey BodyCheck)
bodyChecks scope conn schema
    | scope.checkFunctionBodies = checkBodies conn schema
    | otherwise = pure Map.empty


data Command
    = Describe Text (Maybe FilePath) Bool Scope
    | Diff Text Text Scope
    | Plan Text Text Scope
    | Sqitch Text Text Scope SqitchOptions
    | TableFile FilePath Text TableOptions


data TableOptions = TableOptions
    { write :: Bool
    , againstDatabase :: Bool
    , allowDrops :: Bool
    , base :: Text
    , project :: FilePath
    , top :: FilePath
    , planPath :: FilePath
    , tableTag :: Maybe Text
    }


data SqitchOptions = SqitchOptions
    { projectDir :: FilePath
    , topDir :: FilePath
    , planFile :: FilePath
    , tag :: Maybe Text
    , dryRun :: Bool
    }


main :: IO ()
main =
    handle (\(UserError message) -> TIO.hPutStrLn stderr ("schema-diff: " <> message) >> exitWith (ExitFailure 2))
        $ O.execParser (O.info (commandParser O.<**> O.helper) O.fullDesc)
        >>= \case
            Describe db out replace scope -> do
                conn <- connect db
                schema <- describe conn scope.excluded scope.includePartitions
                case out of
                    Just dir -> writeFolder replace dir schema
                    Nothing ->
                        for_ (Map.toList (Map.fromListWith (+) [(k.address.objectType, 1 :: Int) | k <- Map.keys schema])) \(kind, n) ->
                            TIO.putStrLn [i|#{n}\t#{kind}|]
            Diff fromDb toDb scope -> do
                fromConn <- connect fromDb
                toConn <- connect toDb
                fromSchema <- describe fromConn scope.excluded scope.includePartitions
                toSchema <- describe toConn scope.excluded scope.includePartitions
                edges <- dependencies fromConn scope.excluded
                checks <- bodyChecks scope toConn toSchema
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    known = Set.fromList [k.address | k <- Map.keys fromSchema, k.aspect == Definition]
                    affectedBy = affected known edges changes
                    dependentsOf key = case key.aspect of
                        Definition -> Map.findWithDefault Set.empty key.address affectedBy
                        Privileges -> Set.empty
                for_ (Map.toList changes) \(key, change) -> report key change (dependentsOf key)
                TIO.putStrLn (summary changes)
                unless (Map.null changes) $ exitWith (ExitFailure 1)
            Plan fromDb toDb scope -> do
                fromConn <- connect fromDb
                toConn <- connect toDb
                fromSchema <- describe fromConn scope.excluded scope.includePartitions
                toSchema <- describe toConn scope.excluded scope.includePartitions
                fromEdges <- dependencies fromConn scope.excluded
                toEdges <- dependencies toConn scope.excluded
                checks <- bodyChecks scope toConn toSchema
                let steps = plan checks fromSchema toSchema fromEdges toEdges (diff (sameDefinition checks) fromSchema toSchema)
                for_ steps (TIO.putStrLn . renderStep)
            Sqitch fromDb toDb scope opts -> do
                unless opts.dryRun requireSqitch
                sqitchPlan <- readPlan (opts.projectDir </> opts.planFile)
                fromConn <- connect fromDb
                toConn <- connect toDb
                fromSchema <- describe fromConn scope.excluded scope.includePartitions
                toSchema <- describe toConn scope.excluded scope.includePartitions
                fromEdges <- dependencies fromConn scope.excluded
                toEdges <- dependencies toConn scope.excluded
                now <- getCurrentTime
                checks <- bodyChecks scope toConn toSchema
                let changes = diff (sameDefinition checks) fromSchema toSchema
                    steps = plan checks fromSchema toSchema fromEdges toEdges changes
                    inverseCtx = context Map.empty toSchema fromSchema (Map.map invert changes)
                    written = units True inverseCtx sqitchPlan now steps
                    tagName = fromMaybe (T.pack (formatTime defaultTimeLocale "%Y-%m-%d" now) <> "_schema_diff") opts.tag
                if opts.dryRun
                    then for_ written \u -> do
                        let kind = case u.kind of AddChange -> "add" :: Text; ReworkChange -> "rework"
                            count = length u.deploy
                            changeName = u.changeName
                            note = u.note
                        TIO.putStrLn [i|#{kind}\t#{changeName}\t#{count} statements\t#{note}|]
                    else writeUnits opts.projectDir opts.topDir sqitchPlan tagName written
            TableFile file db opts -> do
                when opts.write requireSqitch
                root <- makeAbsolute opts.project
                absFile <- makeAbsolute file
                let deployDir = root </> opts.top </> "deploy"
                    relative = T.pack (makeRelative deployDir absFile)
                changeName <- maybe (failWith (T.pack file <> " is not under " <> T.pack deployDir)) pure (T.stripSuffix ".sql" relative)
                table <- case T.split (== '/') changeName of
                    [s, "table", t] | not ("@" `T.isInfixOf` t) -> pure (QName s t)
                    _ -> failWith (T.pack file <> " is not a table's own deploy file (<schema>/table/<name>.sql)")
                let parse label = either (\e -> failWith (T.pack file <> label <> ": " <> e)) (pure . withoutTransactionControl) . splitStatements
                statements <- parse "" =<< readFileUtf8 absFile
                original <- jjShow root opts.base (makeRelative root absFile)
                baseline <-
                    if opts.againstDatabase
                        then pure (LiveTable opts.allowDrops)
                        else BaseFile <$> parse (" at " <> opts.base) original
                conn <- connect db
                edit <- compareTable conn baseline table statements
                let steps = plan Map.empty edit.current edit.desired [] [] edit.changes
                    same = if opts.againstDatabase then "the file matches the table" else "the file has no schema changes since " <> opts.base
                for_ edit.ignored \(k, reason) -> do
                    let name = render k.address <> if k.aspect == Privileges then " (privileges)" else ""
                    TIO.putStrLn [i|-- left alone: #{name}: #{reason}|]
                if not opts.write
                    then
                        if null steps
                            then TIO.putStrLn ("-- " <> same)
                            else for_ steps (TIO.putStrLn . renderStep)
                    else do
                        when (null steps) $ failWith ("nothing to write: " <> same)
                        sqitchPlan <- readPlan (root </> opts.planPath)
                        basePlan <- parsePlan <$> jjShow root opts.base opts.planPath
                        now <- getCurrentTime
                        let inverseCtx = context Map.empty edit.desired edit.current (Map.map invert edit.changes)
                            written = units False inverseCtx sqitchPlan now steps
                            tagName = fromMaybe (T.pack (formatTime defaultTimeLocale "%Y-%m-%d" now) <> "_schema_diff") opts.tableTag
                            entries p = length (filter (== changeName) p.changes)
                            alreadyReworked = entries sqitchPlan > entries basePlan
                        writeUnits root opts.top sqitchPlan tagName written
                        unless alreadyReworked
                            $ reworkEdited root opts.top sqitchPlan tagName changeName ("schema-diff: " <> changeName <> " as edited") original
                        for_ written \u -> TIO.putStrLn ("added " <> u.changeName)
                        TIO.putStrLn (if alreadyReworked then "already reworked on this branch: " <> changeName else "reworked " <> changeName)


-- | A file as it is at a jj revision.
jjShow :: FilePath -> Text -> FilePath -> IO Text
jjShow root rev path = do
    (code, out, err) <- readProcess (setWorkingDir root (proc "jj" ["file", "show", "-r", T.unpack rev, path]))
    case code of
        ExitSuccess -> pure (decodeUtf8Lenient (toStrictBytes out))
        ExitFailure _ -> failWith ("jj file show -r " <> rev <> " " <> T.pack path <> ": " <> decodeUtf8Lenient (toStrictBytes err))


commandParser :: O.Parser Command
commandParser =
    O.hsubparser
        $ O.command "describe" (O.info describeParser (O.progDesc "Read one database's schema; with --out, write one file per object"))
        <> O.command "diff" (O.info diffParser (O.progDesc "Show what turns --from into --to; exits 1 when they differ"))
        <> O.command "plan" (O.info (twoDatabases Plan) (O.progDesc "Print the SQL that turns --from into --to"))
        <> O.command "sqitch" (O.info (twoDatabases Sqitch <*> sqitchOptions) (O.progDesc "Write the plan as sqitch changes"))
        <> O.command
            "table"
            ( O.info
                tableParser
                (O.progDesc "Print the migration that turns a table into what its edited deploy file creates; with --write, add it and rework the file")
            )
  where
    tableParser =
        TableFile
            <$> O.strArgument (O.metavar "FILE" <> O.help "The table's deploy file, for example database/deploy/runtime/table/payment.sql")
            <*> O.strOption (O.long "db" <> O.metavar "CONNINFO" <> O.help "A database with the table as it is now; changes are rolled back")
            <*> ( TableOptions
                    <$> O.switch (O.long "write" <> O.help "Add the migration with sqitch and rework the table's change")
                    <*> O.switch (O.long "against-database" <> O.help "Compare with the live table instead of the file at --base; shows drift too")
                    <*> O.switch (O.long "allow-drops" <> O.help "With --against-database, drop columns and constraints that are not in the file")
                    <*> O.strOption (O.long "base" <> O.metavar "REV" <> O.value "master" <> O.help "Revision with the file before your edit (jj)")
                    <*> O.strOption (O.long "project" <> O.metavar "DIR" <> O.value ".")
                    <*> O.strOption (O.long "top-dir" <> O.metavar "DIR" <> O.value "database")
                    <*> O.strOption (O.long "plan-file" <> O.metavar "FILE" <> O.value "database/sqitch.plan")
                    <*> O.optional (O.strOption (O.long "tag" <> O.metavar "NAME" <> O.help "Tag to add when the rework needs one"))
                )
    sqitchOptions =
        SqitchOptions
            <$> O.strOption (O.long "project" <> O.metavar "DIR" <> O.value "." <> O.help "Where sqitch.conf is")
            <*> O.strOption (O.long "top-dir" <> O.metavar "DIR" <> O.value "database")
            <*> O.strOption (O.long "plan-file" <> O.metavar "FILE" <> O.value "database/sqitch.plan")
            <*> O.optional (O.strOption (O.long "tag" <> O.metavar "NAME" <> O.help "Tag to add when a rework needs one"))
            <*> O.switch (O.long "dry-run" <> O.help "List the changes without writing them")
    twoDatabases constructor =
        constructor
            <$> O.strOption (O.long "from" <> O.metavar "CONNINFO" <> O.help "The database to change")
            <*> O.strOption (O.long "to" <> O.metavar "CONNINFO" <> O.help "The database with the desired schema")
            <*> scopeParser
    describeParser =
        Describe
            <$> O.strOption (O.long "db" <> O.metavar "CONNINFO")
            <*> O.optional (O.strOption (O.long "out" <> O.metavar "DIR"))
            <*> O.switch (O.long "replace" <> O.help "Clear --out first, if schema-diff wrote it")
            <*> scopeParser
    diffParser = twoDatabases Diff
    scopeParser =
        Scope
            <$> O.many (O.strOption (O.long "exclude-schema" <> O.metavar "SCHEMA"))
            <*> O.switch (O.long "include-partitions" <> O.help "Also compare partitions and their columns")
            <*> fmap
                not
                ( O.switch
                    ( O.long "no-body-check"
                        <> O.help "Skip recreating SQL-standard function bodies in rolled-back transactions on --to"
                    )
                )


report :: ObjectKey -> Change -> Set Address -> IO ()
report key change dependents = do
    let name = render key.address
    TIO.putStrLn [i|#{verb} #{name}#{suffix}|]
    case change of
        Changed old new ->
            for_ (getDiff (T.lines old) (T.lines new)) \case
                First line -> TIO.putStrLn ("  - " <> line)
                Second line -> TIO.putStrLn ("  + " <> line)
                Both _ _ -> pure ()
        _ -> pure ()
    unless (Set.null dependents) do
        TIO.putStrLn "  depended on by:"
        for_ (Set.toList dependents) \addr -> TIO.putStrLn ("    " <> render addr)
  where
    verb :: Text
    verb = case change of
        Added _ -> "added  "
        Dropped _ -> "dropped"
        Changed _ _ -> "changed"
    suffix :: Text
    suffix = case key.aspect of
        Definition -> ""
        Privileges -> " (privileges)"


summary :: Map ObjectKey Change -> Text
summary changes = [i|#{count isAdded} added, #{count isDropped} dropped, #{count isChanged} changed|]
  where
    count p = L.length (filter p (Map.elems changes))
    isAdded = \case Added _ -> True; _ -> False
    isDropped = \case Dropped _ -> True; _ -> False
    isChanged = \case Changed _ _ -> True; _ -> False
