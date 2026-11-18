{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Splits a deploy script into statements, so it can run inside a transaction of ours.
module Sqiff.Script (splitStatements, withoutTransactionControl) where

import Data.Char (isAlphaNum, isAsciiUpper, isDigit, isSpace, toLower)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import Sqiff.Sql (codeOnly)


data Mode = Plain | Single | Escaped | Double | Dollar String | LineComment | BlockComment Int
    deriving stock (Eq)


-- | Statements without their terminating semicolon, skipping psql meta-commands that only
-- affect psql's output and refusing the rest.
splitStatements :: Text -> Either Text [Text]
splitStatements = go Plain "" False "" Nothing 0 [] . T.unpack
  where
    go :: Mode -> String -> Bool -> String -> Maybe String -> Int -> [Text] -> String -> Either Text [Text]
    go mode cur hasCode word prev depth acc input = case (mode, input) of
        (Plain, []) -> do
            let (_, depth') = finishWord word prev depth
            when (depth' > 0) $ Left "a BEGIN ATOMIC body has no END"
            Right (reverse (emit cur hasCode acc))
        (LineComment, []) -> go Plain cur hasCode word prev depth acc []
        (_, []) -> Left "a quote, dollar quote or comment is not closed"
        (Plain, c : rest)
            | isWordChar c || (c == '$' && not (null word)) -> go Plain (c : cur) True (c : word) prev depth acc rest
            | otherwise ->
                let (prev', depth') = finishWord word prev depth
                    prevAfter = if isSpace c then prev' else Nothing
                 in plain c rest prev' prevAfter depth'
        (Single, '\'' : '\'' : rest) -> go Single ('\'' : '\'' : cur) hasCode word prev depth acc rest
        (Single, '\'' : rest) -> go Plain ('\'' : cur) hasCode "" prev depth acc rest
        (Escaped, '\\' : x : rest) -> go Escaped (x : '\\' : cur) hasCode word prev depth acc rest
        (Escaped, '\'' : '\'' : rest) -> go Escaped ('\'' : '\'' : cur) hasCode word prev depth acc rest
        (Escaped, '\'' : rest) -> go Plain ('\'' : cur) hasCode "" prev depth acc rest
        (Double, '"' : '"' : rest) -> go Double ('"' : '"' : cur) hasCode word prev depth acc rest
        (Double, '"' : rest) -> go Plain ('"' : cur) hasCode "" prev depth acc rest
        (Dollar tag, rest)
            | Just after <- L.stripPrefix tag rest -> go Plain (reverse tag <> cur) hasCode "" prev depth acc after
        (LineComment, '\n' : rest) -> go Plain ('\n' : cur) hasCode "" prev depth acc rest
        (BlockComment n, '/' : '*' : rest) -> go (BlockComment (n + 1)) ('*' : '/' : cur) hasCode word prev depth acc rest
        (BlockComment n, '*' : '/' : rest) ->
            go (if n == 1 then Plain else BlockComment (n - 1)) ('/' : '*' : cur) hasCode "" prev depth acc rest
        (_, c : rest) -> go mode (c : cur) hasCode word prev depth acc rest
      where
        plain c rest prev' prevAfter depth' = case c : rest of
            '\'' : more
                | map toLower word == "e" -> go Escaped ('\'' : cur) True "" Nothing depth' acc more
                | otherwise -> go Single ('\'' : cur) True "" Nothing depth' acc more
            '"' : more -> go Double ('"' : cur) True "" Nothing depth' acc more
            '-' : '-' : more -> go LineComment ('-' : '-' : cur) hasCode "" prev' depth' acc more
            '/' : '*' : more -> go (BlockComment 1) ('*' : '/' : cur) hasCode "" prev' depth' acc more
            '$' : more
                | Just tag <- dollarTag more -> go (Dollar ('$' : tag)) (reverse ('$' : tag) <> cur) True "" Nothing depth' acc (drop (length tag) more)
            ';' : more
                | depth' == 0 -> go Plain "" False "" Nothing 0 (emit cur hasCode acc) more
            '\\' : more
                | not hasCode ->
                    let (line, afterLine) = break (== '\n') more
                        command = takeWhile isWordChar line
                        psqlVariable = case words (drop (length command) line) of
                            name : _ -> all (\x -> isAsciiUpper x || isDigit x || x == '_') name
                            [] -> True
                     in if command `elem` outputOnly || (command `elem` ["set", "unset"] && psqlVariable)
                            then go Plain cur hasCode "" Nothing depth' acc afterLine
                            else Left [i|psql's \\#{command} cannot run outside psql|]
            _ -> go Plain (c : cur) (hasCode || not (isSpace c)) "" prevAfter depth' acc rest

    isWordChar c = isAlphaNum c || c == '_'

    outputOnly = ["pset", "o", "out", "echo", "qecho", "timing", "x"]

    dollarTag more = case span isWordChar more of
        (tag, '$' : _) | maybe True (not . isDigit) (listToMaybe tag) -> Just (tag <> "$")
        _ -> Nothing

    finishWord word prev depth
        | null word = (prev, depth)
        | otherwise =
            let w = map toLower (reverse word)
                depth'
                    | prev == Just "begin" && w == "atomic" && depth == 0 = 1
                    | depth > 0 && w == "case" = depth + 1
                    | depth > 0 && w == "end" = depth - 1
                    | otherwise = depth
             in (Just w, depth')

    emit cur hasCode acc
        | hasCode = T.strip (T.pack (reverse cur)) : acc
        | otherwise = acc


-- | Drops @BEGIN@, @COMMIT@ and the like in all their forms, so the statements run in a
-- transaction of ours. Savepoints stay; two-phase commit commands are refused.
withoutTransactionControl :: [Text] -> Either Text [Text]
withoutTransactionControl = fmap catMaybes . traverse classify
  where
    classify s = case T.words (T.toLower (codeOnly s)) of
        "prepare" : "transaction" : _ -> Left "PREPARE TRANSACTION cannot run inside a transaction of ours"
        w : "prepared" : _ | w `elem` ["commit", "rollback"] -> Left (T.toUpper w <> " PREPARED cannot run inside a transaction of ours")
        -- ROLLBACK [WORK | TRANSACTION] TO [SAVEPOINT] name undoes to a savepoint and stays.
        "rollback" : rest | "to" : _ <- dropWhile (`elem` ["work", "transaction"]) rest -> Right (Just s)
        w : _ | w `elem` ["begin", "start", "commit", "end", "rollback", "abort"] -> Right Nothing
        _ -> Right (Just s)
