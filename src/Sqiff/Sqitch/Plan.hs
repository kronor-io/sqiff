{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | sqitch's plan file, and the changes a database's sqitch registry has deployed.
module Sqiff.Sqitch.Plan (
    SqitchPlan (..),
    readPlan,
    parsePlan,
    plannedChanges,
    deployedChanges,
    entryName,
) where

import Data.String.Interpolate qualified as S
import Data.Text qualified as T
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Hasql.TH (vectorStatement)
import RIO
import RIO.Directory (doesFileExist)
import RIO.List qualified as L
import RIO.Set qualified as Set
import Sqiff.Connection (errorText, failWith)


data SqitchPlan = SqitchPlan
    { project :: Text
    , changes :: [Text]
    , tagged :: Set Text
    -- ^ Changes with a tag somewhere after their last entry.
    , tags :: Set Text
    -- ^ The plan's tags, without the @\@@.
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
        fromEnd _ _ [] = []
        fromEnd seenTag visited (l : ls)
            | isTag l = fromEnd True visited ls
            | entryName l `Set.member` visited = fromEnd seenTag visited ls
            | otherwise = (entryName l, seenTag) : fromEnd seenTag (Set.insert (entryName l) visited) ls
     in SqitchPlan
            { project = fromMaybe "" (listToMaybe (mapMaybe (T.stripPrefix "%project=") lines'))
            , changes = [entryName l | l <- entries, not (isTag l)]
            , tagged = Set.fromList [n | (n, True) <- fromEnd False Set.empty (reverse entries)]
            , tags = Set.fromList [entryName t | l <- entries, Just t <- [T.stripPrefix "@" l]]
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


-- | The name a plan line starts with, a change's or a tag's.
entryName :: Text -> Text
entryName = T.takeWhile (\c -> c /= ' ' && c /= '[')
