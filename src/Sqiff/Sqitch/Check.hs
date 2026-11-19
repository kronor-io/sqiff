{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What sqiff checks before it writes anything to a sqitch project: every change name is
-- one sqitch accepts and stays inside the script directories, no two changes share a
-- name, and the tag a rework needs is free.
module Sqiff.Sqitch.Check (
    changeNameProblem,
    tagNameProblem,
    batchProblems,
    Tagging (..),
    chooseTag,
) where

import Data.Char (isPunctuation, isSpace, isSymbol)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Sqitch.Plan (SqitchPlan (..))
import Sqiff.Sqitch.Units (Unit (..), UnitKind (..))


-- | Why sqitch, or the file system, would refuse this change name.
changeNameProblem :: Text -> Maybe Text
changeNameProblem name
    | T.null name = Just "a change name cannot be empty"
    | Just c <- T.find (\c -> isSpace c || c `elem` (":@#\\" :: String)) name = Just [i|#{shown} has #{describeChar c}, which sqitch does not allow in a change name|]
    | punct (T.head name) || punct (T.last name) = Just [i|#{shown} starts or ends with punctuation, which sqitch does not allow|]
    | endsInReference = Just [i|#{shown} ends in one of ~^/=% and digits, which sqitch reads as a reference|]
    | "/" `T.isPrefixOf` name = Just [i|#{shown} is an absolute path|]
    | any (`elem` ["", ".", ".."]) (T.splitOn "/" name) = Just [i|#{shown} has an empty, . or .. path segment, so its scripts would not stay in their directories|]
    | otherwise = Nothing
  where
    shown = tshow name
    punct c = c /= '_' && (isPunctuation c || isSymbol c)
    endsInReference =
        let digits = T.takeWhileEnd (`elem` ['0' .. '9']) name
            rest = T.dropWhileEnd (`elem` ['0' .. '9']) name
         in not (T.null digits) && maybe False (\(_, c) -> c `elem` ("~^/=%" :: String)) (T.unsnoc rest)
    describeChar c = if isSpace c then "a blank" else T.pack ['\'', c, '\'']


-- | Why sqitch would refuse this tag name: the change name rules, and no @/@.
tagNameProblem :: Text -> Maybe Text
tagNameProblem name
    | "/" `T.isInfixOf` name = Just [i|tag #{tshow name} has '/', which sqitch does not allow in a tag|]
    | otherwise = (\p -> "tag: " <> p) <$> changeNameProblem name


-- | Every reason the batch cannot be written to the plan as it is; none when it can.
batchProblems :: SqitchPlan -> [Unit] -> [Text]
batchProblems plan us =
    mapMaybe changeNameProblem (nubOrd names)
        <> [ [i|#{tshow n} would be written #{count} times in one run; write them as one change, or one at a time|]
           | (n, count) <- Map.toList (Map.fromListWith (+) [(n, 1 :: Int) | n <- names])
           , count > 1
           ]
        <> [[i|#{tshow n} is already in the plan, so it cannot be added again|] | u <- us, u.kind == AddChange, let n = u.changeName, n `Set.member` planned]
        <> [[i|#{tshow n} is not in the plan, so it cannot be reworked|] | u <- us, u.kind == ReworkChange, let n = u.changeName, n `Set.notMember` planned]
  where
    names = map (.changeName) us
    planned = Set.fromList plan.changes


-- | The tag a rework needs: the one the command line names, or the day's with @_sqiff@.
data Tagging = Tagging
    { given :: Maybe Text
    , now :: UTCTime
    }


-- | The tag to add to the plan: the given one, which must be new, or the first free one of
-- @<day>_sqiff@, @<day>_sqiff_2@, …
chooseTag :: SqitchPlan -> Tagging -> Either Text Text
chooseTag plan tagging = case tagging.given of
    Just t
        | Just problem <- tagNameProblem t -> Left problem
        | t `Set.member` plan.tags -> Left [i|the plan already has the tag @#{t}; pass a new one with --tag|]
        | otherwise -> Right t
    Nothing ->
        let day = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" tagging.now)
            candidates = [day <> "_sqiff"] <> [[i|#{day}_sqiff_#{n}|] | n <- [2 :: Int ..]]
         in maybe (Left "no free tag") Right (L.find (`Set.notMember` plan.tags) candidates)
