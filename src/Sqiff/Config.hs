{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The project's @sqiff.yaml@, and the sqitch layout from @sqitch config@.
module Sqiff.Config (
    Config (..),
    Names (..),
    BaselineConfig (..),
    Password (..),
    SqitchPlace (..),
    DeployStep (..),
    Layout (..),
    loadConfig,
    configFile,
    sqitchLayout,
    fill,
    match,
) where

import Data.Aeson (FromJSON (..), Object, withObject, withText, (.!=), (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import RIO hiding (view)
import RIO.Directory (doesFileExist)
import RIO.FilePath ((</>))
import RIO.Map qualified as Map
import Sqiff.Catalog (failWith)
import System.Process.Typed (nullStream, proc, readProcess, setStdin, setWorkingDir)


data Config = Config
    { excludeSchemas :: [Text]
    , renameHint :: Maybe Text
    , templates :: Maybe FilePath
    , names :: Names
    , baseline :: BaselineConfig
    }


-- | Change names by kind of object, with @{schema}@, @{name}@ and @{table}@ filled in;
-- a migration's has @{stamp}@ (formatted by 'migrationStamp') and @{slug}@.
data Names = Names
    { table :: Text
    , function :: Text
    , view :: Text
    , trigger :: Text
    , index :: Text
    , type_ :: Text
    , migration :: Text
    , migrationStamp :: Text
    }


data BaselineConfig = BaselineConfig
    { composeFile :: FilePath
    , service :: Text
    , portVariable :: Maybe Text
    , port :: Int
    , containerPort :: Int
    , environment :: Map Text Text
    , user :: Text
    , database :: Text
    , password :: Maybe Password
    , sqitch :: SqitchPlace
    , deploy :: [DeployStep]
    , status :: [Text]
    }


-- | The baseline's password, as written or from a variable in sqiff's environment.
data Password = PasswordText Text | PasswordVariable Text


-- | Where the baseline's sqitch runs: on this machine against the published port, or
-- inside the compose service.
data SqitchPlace = OnHost | InService
    deriving stock (Eq)


-- | One sqitch call that deploys the baseline; skipped when @ifExists@ is missing from
-- the checkout.
data DeployStep = DeployStep
    { args :: [Text]
    , ifExists :: Maybe FilePath
    }


-- | Where sqitch keeps the project's scripts and plan, relative to the project root.
data Layout = Layout
    { topDir :: FilePath
    , planFile :: FilePath
    }


configFile :: FilePath
configFile = "sqiff.yaml"


-- | @dir@'s @sqiff.yaml@, or the defaults when it has none.
loadConfig :: FilePath -> IO Config
loadConfig dir = do
    let path = dir </> configFile
    doesFileExist path >>= \case
        False -> either (\e -> failWith [i|the default settings: #{e}|]) pure (parseEither parseJSON (Yaml.Object mempty))
        True ->
            Yaml.decodeFileEither path >>= \case
                Left e -> failWith [i|#{path}: #{Yaml.prettyPrintParseException e}|]
                Right c -> pure c


instance FromJSON Config where
    parseJSON = withObject "sqiff.yaml" \o -> do
        known ["exclude-schemas", "rename-hint", "templates", "names", "baseline"] o
        excludeSchemas <- o .:? "exclude-schemas" .!= []
        renameHint <- o .:? "rename-hint"
        templates <- o .:? "templates"
        names <- o .:? "names" .!= Yaml.Object mempty >>= parseJSON
        baseline <- o .:? "baseline" .!= Yaml.Object mempty >>= parseJSON
        pure Config{excludeSchemas, renameHint, templates, names, baseline}


instance FromJSON Names where
    parseJSON = withObject "names" \o -> do
        known ["table", "function", "view", "trigger", "index", "type", "migration", "migration-stamp"] o
        table <- o .:? "table" .!= "{schema}/table/{name}"
        function <- o .:? "function" .!= "{schema}/function/{name}"
        view <- o .:? "view" .!= "{schema}/view/{name}"
        trigger <- o .:? "trigger" .!= "{schema}/trigger/{table}/{name}"
        index <- o .:? "index" .!= "{schema}/table/{table}/index/{name}"
        type_ <- o .:? "type" .!= "{schema}/type/{name}"
        migration <- o .:? "migration" .!= "migrations/{stamp}-{slug}"
        migrationStamp <- o .:? "migration-stamp" .!= "%Y/%m/%Y-%m-%d-%H%M%S"
        pure Names{table, function, view, trigger, index, type_, migration, migrationStamp}


instance FromJSON BaselineConfig where
    parseJSON = withObject "baseline" \o -> do
        known ["compose-file", "service", "port-variable", "port", "container-port", "environment", "user", "database", "password", "password-variable", "sqitch", "deploy", "status"] o
        composeFile <- o .:? "compose-file" .!= "docker-compose.yaml"
        service <- o .:? "service" .!= "postgres"
        portVariable <- o .:? "port-variable"
        port <- o .:? "port" .!= 0
        containerPort <- o .:? "container-port" .!= 5432
        environment <- o .:? "environment" .!= Map.empty
        user <- o .:? "user" .!= "postgres"
        database <- o .:? "database" .!= "postgres"
        password <-
            (,) <$> o .:? "password" <*> o .:? "password-variable" >>= \case
                (Just _, Just _) -> fail "set password or password-variable, not both"
                (written, variable) -> pure (PasswordText <$> written <|> PasswordVariable <$> variable)
        sqitch <- o .:? "sqitch" .!= OnHost
        deploy <- o .:? "deploy" .!= [DeployStep ["deploy"] Nothing]
        status <- o .:? "status" .!= ["status"]
        pure BaselineConfig{composeFile, service, portVariable, port, containerPort, environment, user, database, password, sqitch, deploy, status}


instance FromJSON SqitchPlace where
    parseJSON = withText "sqitch" \case
        "host" -> pure OnHost
        "service" -> pure InService
        other -> fail [i|sqitch is host or service, not #{other}|]


instance FromJSON DeployStep where
    parseJSON = withObject "deploy step" \o -> do
        known ["sqitch", "if-exists"] o
        DeployStep <$> o .: "sqitch" <*> o .:? "if-exists"


-- | Refuses keys this version does not read, so a misspelt setting is not ignored.
known :: [Text] -> Object -> Parser ()
known keys o = case [k | k <- map Key.toText (KeyMap.keys o), k `notElem` keys] of
    [] -> pure ()
    unknown -> let ks = T.intercalate ", " unknown in fail [i|unknown settings: #{ks}|]


-- | The top directory and plan file sqitch uses in @root@, from @sqitch config --list@.
sqitchLayout :: FilePath -> IO Layout
sqitchLayout root = do
    (code, out, err) <- readProcess (setStdin nullStream (setWorkingDir root (proc "sqitch" ["config", "--list"])))
    unless (code == ExitSuccess) $ failWith [i|sqitch config --list in #{root} failed: #{decode err}|]
    let settings = Map.fromList [(T.toLower k, T.drop 1 v) | l <- T.lines (decode out), let (k, v) = T.breakOn "=" l, not (T.null v)]
        topDir = maybe "." T.unpack (Map.lookup "core.top_dir" settings)
        planFile = maybe (topDir </> "sqitch.plan") T.unpack (Map.lookup "core.plan_file" settings)
    pure Layout{topDir, planFile}
  where
    decode = T.strip . decodeUtf8Lenient . toStrictBytes


-- | @pattern@ with each @{key}@ replaced by its value, in one pass, so a value is never
-- read as a placeholder itself; a key without a value stays as written.
fill :: Text -> [(Text, Text)] -> Text
fill pattern values = T.concat (map piece (templatePieces pattern))
  where
    piece = \case
        Left text -> text
        Right key -> fromMaybe ("{" <> key <> "}") (lookup key values)


-- | The values @pattern@'s placeholders take in @text@; a placeholder matches one or more
-- characters other than @/@.
match :: Text -> Text -> Maybe [(Text, Text)]
match pattern text = go (templatePieces pattern) text
  where
    go [] rest = if T.null rest then Just [] else Nothing
    go (Left literal : ps) rest = T.stripPrefix literal rest >>= go ps
    go (Right key : ps) rest =
        let candidates = case ps of
                Left literal : _ -> [(T.take n rest, T.drop n rest) | (before, _) <- breaks literal rest, let n = T.length before]
                _ -> [(rest, "")]
         in listToMaybe
                [ (key, value) : bound
                | (value, after) <- candidates
                , not (T.null value)
                , not ("/" `T.isInfixOf` value)
                , Just bound <- [go ps after]
                ]
    breaks literal rest = [(T.dropEnd (T.length r) rest, r) | r <- T.tails rest, literal `T.isPrefixOf` r]


-- | A template's literal text and its @{key}@ placeholders, in order.
templatePieces :: Text -> [Either Text Text]
templatePieces p = case T.breakOn "{" p of
    (literal, "") -> [Left literal | not (T.null literal)]
    (literal, rest) ->
        let (key, after) = T.breakOn "}" (T.drop 1 rest)
         in [Left literal | not (T.null literal)] <> [Right key] <> templatePieces (T.drop 1 after)
