{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Splits a deploy script into statements, so it can run inside a transaction of ours.
module Sqiff.Script (splitStatements, withoutTransactionControl) where

import Data.Char (isAlphaNum, isAsciiUpper, isDigit, isSpace)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import Sqiff.Sql (Region (..), closed, codeOnly, regions)


-- | Statements without their terminating semicolon, skipping psql meta-commands that only
-- affect psql's output and refusing the rest. Each statement is a slice of the script.
splitStatements :: Text -> Either Text [Text]
splitStatements script = do
    split <- firstStatement script
    more <- maybe (Right []) splitStatements split.after
    Right (maybeToList split.statement <> more)


-- | A script's first statement and the script after it.
data FirstStatement = FirstStatement
    { statement :: Maybe Text
    -- ^ Nothing when there are only whitespace and comments before the split.
    , after :: Maybe Text
    -- ^ Nothing when the statement ends the script.
    }


-- | What 'firstStatement' looks at in a script: its words, the semicolons and backslashes
-- in its code, and what lies between them.
data Token
    = Word Text
    -- ^ A word of code, lowercased.
    | Semicolon
    | Backslash
    | Blank
    -- ^ Whitespace or a comment: the word before it stays the previous word.
    | Mark
    -- ^ Any other code, or a quoted character: it parts @BEGIN@ from @ATOMIC@.


-- | Each token of the script, with where it starts.
tokens :: Text -> [(Int, Token)]
tokens = go . zip [0 ..] . regions
  where
    go = \case
        [] -> []
        xs@((k, (c, Code)) : rest)
            | isWordChar c ->
                let (w, more) = span (\(_, (ch, r)) -> r == Code && (isWordChar ch || ch == '$')) xs
                 in (k, Word (T.toLower (T.pack (map (fst . snd) w)))) : go more
            | c == ';' -> (k, Semicolon) : go rest
            | c == '\\' -> (k, Backslash) : go rest
            | isSpace c -> (k, Blank) : go rest
            | otherwise -> (k, Mark) : go rest
        (k, (_, Remark)) : rest -> (k, Blank) : go rest
        (k, (_, Quoted)) : rest -> (k, Mark) : go rest


-- | The script's first statement, unless it has only whitespace and comments, and the
-- script after it, unless it ends the script. A semicolon ends a statement in code, outside
-- any @BEGIN ATOMIC@ body; a backslash before any code starts a psql meta-command, which
-- runs to the end of its line.
--
-- 'splitStatements' lexes the script after a split afresh. That reads it as the lexer
-- would have read it in place: both splits are in code, after a character that is no
-- part of a word, which is where 'regions' starts.
firstStatement :: Text -> Either Text FirstStatement
firstStatement script = go (0 :: Int) Nothing False (tokens script)
  where
    -- How many @BEGIN ATOMIC@ bodies and @CASE@ blocks in them are open, the last word if
    -- only blanks came since, and whether the statement has code yet.
    go depth previous hasCode = \case
        []
            | not (closed script) -> Left "a quote, dollar quote or comment is not closed"
            | depth > 0 -> Left "a BEGIN ATOMIC body has no END"
            | otherwise -> Right (FirstStatement (statement hasCode script) Nothing)
        (k, Semicolon) : _ | depth == 0 -> Right (FirstStatement (statement hasCode (T.take k script)) (Just (T.drop (k + 1) script)))
        (k, Backslash) : _ | not hasCode -> FirstStatement Nothing . Just <$> skipPsqlCommand (T.drop (k + 1) script)
        (_, Word w) : rest -> go (atomicDepthAfter previous w depth) (Just w) True rest
        (_, Blank) : rest -> go depth previous hasCode rest
        _ : rest -> go depth Nothing True rest
    statement hasCode t = if hasCode then Just (T.strip t) else Nothing


-- | The script after a psql meta-command's line, if the command only affects psql's
-- output; refuses the command otherwise.
skipPsqlCommand :: Text -> Either Text Text
skipPsqlCommand afterBackslash
    | command `elem` outputOnly || (command `elem` ["set", "unset"] && psqlVariable) = Right afterLine
    | otherwise = Left [i|psql's \\#{command} cannot run outside psql|]
  where
    (line, afterLine) = T.break (== '\n') afterBackslash
    command = T.takeWhile isWordChar line
    psqlVariable = case T.words (T.drop (T.length command) line) of
        name : _ -> T.all (\x -> isAsciiUpper x || isDigit x || x == '_') name
        [] -> True
    outputOnly = ["pset", "o", "out", "echo", "qecho", "timing", "x"]


-- | The @BEGIN ATOMIC@ depth after a lowercased word, given the word before it.
atomicDepthAfter :: Maybe Text -> Text -> Int -> Int
atomicDepthAfter previous word depth
    | previous == Just "begin" && word == "atomic" && depth == 0 = 1
    | depth > 0 && word == "case" = depth + 1
    | depth > 0 && word == "end" = depth - 1
    | otherwise = depth


isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'


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
