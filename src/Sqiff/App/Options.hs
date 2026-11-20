{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The command line, parsed into a 'Command' whose options all apply: flags that make no
-- sense together are refused before anything runs.
module Sqiff.App.Options (
    Command (..),
    DescribeOptions (..),
    SqitchOptions (..),
    TableOptions (..),
    TableMode (..),
    MigrateOptions (..),
    Comparison (..),
    renamesOf,
    Output (..),
    SameTypeSwap (..),
    TableFlags (..),
    tableOptions,
    parseCommand,
) where

import OptEnvConf hiding (Command, commandParser)
import Paths_sqiff (version)
import RIO hiding (many, optional)
import RIO.Text qualified as T
import Sqiff.App.Env (BodyChecks (..), Scope (..), Source, source)
import Sqiff.Catalog (Partitions (..))
import Sqiff.ConnectionString (ConnectionString, connectionString)
import Sqiff.Folder (Existing (..))
import Sqiff.Identifier (Identifier, identifier)
import Sqiff.TableEdit (Drops (..))


data Command
    = Describe DescribeOptions
    | Diff Source ConnectionString Scope
    | Plan Source ConnectionString Scope
    | Sqitch Source ConnectionString Scope SqitchOptions
    | TableFile FilePath ConnectionString TableOptions


data DescribeOptions = DescribeOptions
    { db :: ConnectionString
    , out :: Maybe FilePath
    , existing :: Existing
    , scope :: Scope
    }


data SqitchOptions = SqitchOptions
    { projectDir :: FilePath
    , topDir :: Maybe FilePath
    , planFile :: Maybe FilePath
    , tag :: Maybe Text
    , dryRun :: Bool
    }


-- | The table command's options.
data TableOptions = TableOptions
    { output :: Output
    , base :: Text
    -- ^ The git revision with the file before the edit.
    , project :: FilePath
    , top :: Maybe FilePath
    , planPath :: Maybe FilePath
    , tableTag :: Maybe Text
    , mode :: TableMode
    }


-- | Whether @table@ migrates the table to the file, or rewrites the file from the table.
data TableMode
    = MigrateToFile MigrateOptions
    | -- | From the table in this database, read-only, or in @--db@.
      RewriteFile (Maybe ConnectionString)


data MigrateOptions = MigrateOptions
    { comparison :: Comparison
    , sameTypeSwap :: SameTypeSwap
    }


-- | What the edited file is compared with: the file at the base revision, with the
-- columns these pairs rename, or the live table.
data Comparison
    = WithBaseFile [(Identifier, Identifier)]
    | WithLiveTable Drops


renamesOf :: Comparison -> [(Identifier, Identifier)]
renamesOf = \case
    WithBaseFile renames -> renames
    WithLiveTable _ -> []


-- | Whether @table@ prints the migration or writes it with sqitch.
data Output = PrintMigration | WriteMigration
    deriving stock (Eq)


-- | A dropped column and an added one of the same type: maybe a rename, or not one.
data SameTypeSwap = AskIfRename | DropAndAdd
    deriving stock (Eq)


-- | The table command's flags as given.
data TableFlags = TableFlags
    { output :: Output
    , againstDatabase :: Bool
    , allowDrops :: Bool
    , base :: Text
    , project :: FilePath
    , top :: Maybe FilePath
    , planPath :: Maybe FilePath
    , tableTag :: Maybe Text
    , renames :: [(Identifier, Identifier)]
    , dropAndAdd :: Bool
    , rewriteFile :: Bool
    , liveFrom :: Maybe ConnectionString
    }


-- | The table command's options, when its flags apply together.
tableOptions :: TableFlags -> Either Text TableOptions
tableOptions f = do
    mode <-
        if f.rewriteFile
            then do
                unless (null f.renames) $ Left "--rename works with the file comparison, not --rewrite-file"
                when f.againstDatabase $ Left "--against-database works with the file comparison, not --rewrite-file"
                when f.allowDrops $ Left "--allow-drops works with --against-database, not --rewrite-file"
                when f.dropAndAdd $ Left "--drop-and-add works with the file comparison, not --rewrite-file"
                pure (RewriteFile f.liveFrom)
            else do
                when (f.againstDatabase && not (null f.renames)) $ Left "--rename works with the file comparison, not --against-database"
                when (isJust f.liveFrom) $ Left "--live-from works with --rewrite-file"
                when (f.allowDrops && not f.againstDatabase) $ Left "--allow-drops works with --against-database"
                let comparison
                        | f.againstDatabase = WithLiveTable (if f.allowDrops then AllowDrops else ReportDrops)
                        | otherwise = WithBaseFile f.renames
                pure (MigrateToFile MigrateOptions{comparison, sameTypeSwap = if f.dropAndAdd then DropAndAdd else AskIfRename})
    pure TableOptions{output = f.output, base = f.base, project = f.project, top = f.top, planPath = f.planPath, tableTag = f.tableTag, mode}


-- | The command the arguments name. Flags that do not apply together are a parse error.
parseCommand :: IO Command
parseCommand = runParser version "Compare PostgreSQL schemas and write the migrations between them" commandParser


commandParser :: Parser Command
commandParser =
    commands
        [ command "describe" "Read one database's schema; with --out, write one file per object" (Describe <$> describeParser)
        , command "diff" "Show what turns --from into --to; exits 1 when they differ" (twoDatabases Diff)
        , command "plan" "Print the SQL that turns --from into --to" (twoDatabases Plan)
        , command "sqitch" "Write the plan as sqitch changes" (twoDatabases Sqitch <*> sqitchOptions)
        , command
            "table"
            "Print the migration that turns a table into what its edited deploy file creates; with --write, add it and rework the file"
            tableParser
        ]
  where
    tableParser =
        TableFile
            <$> setting [help "The table's deploy file, for example database/deploy/runtime/table/payment.sql", reader str, argument, metavar "FILE"]
            <*> dbOption "db" "A database with the table as it is now; changes are rolled back"
            <*> checkMapEither (first T.unpack . tableOptions) tableFlags
    tableFlags =
        TableFlags
            <$> (bool PrintMigration WriteMigration <$> flag "write" "Add the migration with sqitch and rework the table's change")
            <*> flag "against-database" "Compare with the live table instead of the file at --base; shows drift too"
            <*> flag "allow-drops" "With --against-database, drop columns and constraints that are not in the file"
            <*> setting [help "Git revision with the file before your edit", reader str, option, long "base", metavar "REV", value "master"]
            <*> projectOption
            <*> topDirOption
            <*> planFileOption
            <*> optional (setting [help "Tag to add when the rework needs one", reader str, option, long "tag", metavar "NAME"])
            <*> many (setting [help "Column OLD is renamed to NEW; repeat for more columns", reader (eitherReader renameArg), option, long "rename", metavar "OLD:NEW"])
            <*> flag "drop-and-add" "A column dropped and one added with the same type are not a rename"
            <*> flag "rewrite-file" "Print the file rewritten to create the table as --db has it; with --write, write it and rework the change"
            <*> optional (connectionString <$> setting [help "With --rewrite-file, read the table from this database, read-only, instead of --db; --db still builds the file", reader str, option, long "live-from", metavar "CONNINFO"])
    flag key description = setting [help description, switch True, long key, value False]
    renameArg s = case T.split (== ':') (T.pack s) of
        [old, new] | not (T.null old), not (T.null new) -> Right (identifier old, identifier new)
        _ -> Left "expected OLD:NEW"
    dbOption key description =
        connectionString
            <$> setting
                [ help (description <> "; left out, libpq's PGHOST, PGPORT, PGUSER, PGDATABASE or PGSERVICE name it")
                , reader str
                , option
                , long key
                , metavar "CONNINFO"
                , value ""
                ]
    projectOption = setting [help "Where sqitch.conf and sqiff.yaml are", reader str, option, long "project", metavar "DIR", value "."]
    topDirOption = optional (setting [help "sqitch's top directory; sqitch config's core.top_dir when left out", reader str, option, long "top-dir", metavar "DIR"])
    planFileOption = optional (setting [help "sqitch's plan; sqitch config's core.plan_file when left out", reader str, option, long "plan-file", metavar "FILE"])
    sqitchOptions =
        SqitchOptions
            <$> projectOption
            <*> topDirOption
            <*> planFileOption
            <*> optional (setting [help "Tag to add when a rework needs one", reader str, option, long "tag", metavar "NAME"])
            <*> flag "dry-run" "List the changes without writing them"
    twoDatabases constructor =
        constructor
            <$> ( source
                    <$> setting
                        [ help "The database to change; master or rev:REV builds or updates the baseline database at that revision and uses it"
                        , reader str
                        , option
                        , long "from"
                        , metavar "CONNINFO|master|rev:REV"
                        ]
                )
            <*> dbOption "to" "The database with the desired schema"
            <*> scopeParser
    describeParser =
        DescribeOptions
            <$> dbOption "db" "The database to read"
            <*> optional (setting [help "Write one file per object to this directory", reader str, option, long "out", metavar "DIR"])
            <*> (bool KeepExisting ReplaceOwn <$> flag "replace" "Clear --out first, if sqiff wrote it")
            <*> scopeParser
    scopeParser =
        Scope
            <$> many (setting [help "Leave this schema out, besides sqiff.yaml's exclude-schemas", reader str, option, long "exclude-schema", metavar "SCHEMA"])
            <*> (bool SkipPartitions IncludePartitions <$> flag "include-partitions" "Also compare partitions and their columns")
            <*> (bool CheckBodies SkipBodyChecks <$> flag "no-body-check" "Skip recreating SQL-standard function bodies in rolled-back transactions on --to")
