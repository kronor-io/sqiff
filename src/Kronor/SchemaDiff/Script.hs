{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Splits a deploy script into statements, so it can run inside a transaction of ours.
module Kronor.SchemaDiff.Script (splitStatements, withoutTransactionControl) where

import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Set qualified as Set


data Mode = Plain | Single | Double | Dollar String | LineComment | BlockComment Int
    deriving stock (Eq)


-- | Statements without their terminating semicolon. Semicolons inside quotes, dollar
-- quotes, comments and @BEGIN ATOMIC … END@ bodies do not split. Skips psql meta-commands
-- that only affect psql's output and refuses the rest.
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
            | isWordChar c -> go Plain (c : cur) True (c : word) prev depth acc rest
            | otherwise ->
                let (prev', depth') = finishWord word prev depth
                    prevAfter = if isSpace c then prev' else Nothing
                 in plain c rest prev' prevAfter depth'
        (Single, '\'' : '\'' : rest) -> go Single ('\'' : '\'' : cur) hasCode word prev depth acc rest
        (Single, '\'' : rest) -> go Plain ('\'' : cur) hasCode "" prev depth acc rest
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
            '\'' : more -> go Single ('\'' : cur) True "" Nothing depth' acc more
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
                     in if command `elem` outputOnly
                            then go Plain cur hasCode "" Nothing depth' acc afterLine
                            else Left ("psql's \\" <> T.pack command <> " cannot run outside psql")
            _ -> go Plain (c : cur) (hasCode || not (isSpace c)) "" prevAfter depth' acc rest

    isWordChar c = isAlphaNum c || c == '_'

    -- Meta-commands that only change psql's own output, safe to leave out.
    outputOnly = ["pset", "o", "out", "echo", "qecho", "timing", "x", "set", "unset"]

    -- The tag after the opening @$@, including the closing @$@.
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


-- | Drops @BEGIN@, @COMMIT@ and the like, so the statements run in a transaction of ours.
withoutTransactionControl :: [Text] -> [Text]
withoutTransactionControl = filter (\s -> normal s `Set.notMember` control)
  where
    normal = T.toLower . T.unwords . T.words . stripComments
    stripComments = T.unlines . filter (not . ("--" `T.isPrefixOf`) . T.stripStart) . T.lines
    control =
        Set.fromList
            [ "begin"
            , "begin work"
            , "begin transaction"
            , "start transaction"
            , "commit"
            , "commit work"
            , "commit transaction"
            , "end"
            , "end work"
            , "end transaction"
            , "rollback"
            , "abort"
            ]
