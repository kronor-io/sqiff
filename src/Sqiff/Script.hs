{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
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


-- | Where the scanner stands in a script. The @Rev@ fields are built by consing, so they
-- hold their text, or their statements, latest first.
data ScanState = ScanState
    { mode :: Mode
    , statementRev :: String
    -- ^ The statement so far, comments and all, reversed.
    , hasCode :: Bool
    -- ^ Whether the statement so far has more than whitespace and comments.
    , wordRev :: String
    -- ^ The word being read, reversed; empty between words and outside 'Plain'.
    , previousWord :: Maybe String
    -- ^ The last word read, lowercased, if only whitespace and comments came since.
    , atomicDepth :: Int
    -- ^ How many @BEGIN ATOMIC@ bodies and @CASE@ blocks in them are open; a semicolon
    -- inside one does not end the statement.
    , statementsRev :: [Text]
    -- ^ The finished statements, reversed.
    }


-- | Statements without their terminating semicolon, skipping psql meta-commands that only
-- affect psql's output and refusing the rest.
splitStatements :: Text -> Either Text [Text]
splitStatements = scan startOfScript . T.unpack


startOfScript :: ScanState
startOfScript =
    ScanState
        { mode = Plain
        , statementRev = ""
        , hasCode = False
        , wordRev = ""
        , previousWord = Nothing
        , atomicDepth = 0
        , statementsRev = []
        }


-- | Reads the rest of a script, a character or a delimiter at a time.
scan :: ScanState -> String -> Either Text [Text]
scan state input = case (state.mode, input) of
    (Plain, []) -> do
        let ended = finishWord state
        when (ended.atomicDepth > 0) $ Left "a BEGIN ATOMIC body has no END"
        Right (reverse (finishStatement ended).statementsRev)
    (LineComment, []) -> scan state{mode = Plain} []
    (_, []) -> Left "a quote, dollar quote or comment is not closed"
    (Plain, c : rest)
        | isWordChar c || (c == '$' && not (null state.wordRev)) ->
            scan (keep [c] state){hasCode = True, wordRev = c : state.wordRev} rest
        | otherwise -> scanPlain state c rest
    (Single, '\'' : '\'' : rest) -> scan (keep "''" state) rest
    (Single, '\'' : rest) -> scan (backToPlain "'" state) rest
    (Escaped, '\\' : x : rest) -> scan (keep ['\\', x] state) rest
    (Escaped, '\'' : '\'' : rest) -> scan (keep "''" state) rest
    (Escaped, '\'' : rest) -> scan (backToPlain "'" state) rest
    (Double, '"' : '"' : rest) -> scan (keep "\"\"" state) rest
    (Double, '"' : rest) -> scan (backToPlain "\"" state) rest
    (Dollar tag, rest)
        | Just after <- L.stripPrefix tag rest -> scan (backToPlain tag state) after
    (LineComment, '\n' : rest) -> scan (backToPlain "\n" state) rest
    (BlockComment n, '/' : '*' : rest) -> scan (keep "/*" state){mode = BlockComment (n + 1)} rest
    (BlockComment n, '*' : '/' : rest) ->
        scan (keep "*/" state){mode = if n == 1 then Plain else BlockComment (n - 1)} rest
    (_, c : rest) -> scan (keep [c] state) rest


-- | Reads plain SQL from a character that is no part of a word, so it ends the word
-- being read.
scanPlain :: ScanState -> Char -> String -> Either Text [Text]
scanPlain state c rest = case c : rest of
    '\'' : more
        | map toLower state.wordRev == "e" -> scan (openQuote Escaped "'" ended) more
        | otherwise -> scan (openQuote Single "'" ended) more
    '"' : more -> scan (openQuote Double "\"" ended) more
    '-' : '-' : more -> scan (keep "--" ended){mode = LineComment} more
    '/' : '*' : more -> scan (keep "/*" ended){mode = BlockComment 1} more
    '$' : more
        | Just tag <- dollarTag more ->
            scan (openQuote (Dollar ('$' : tag)) ('$' : tag) ended) (drop (length tag) more)
    ';' : more
        | ended.atomicDepth == 0 -> scan (finishStatement ended) more
    '\\' : more
        | not state.hasCode -> skipPsqlCommand ended more
    _ ->
        scan
            (keep [c] ended)
                { hasCode = ended.hasCode || not (isSpace c)
                , previousWord = if isSpace c then ended.previousWord else Nothing
                }
            rest
  where
    ended = finishWord state


-- | Skips a psql meta-command up to the end of its line if it only affects psql's output,
-- and refuses it otherwise.
skipPsqlCommand :: ScanState -> String -> Either Text [Text]
skipPsqlCommand state afterBackslash
    | command `elem` outputOnly || (command `elem` ["set", "unset"] && psqlVariable) =
        scan state{previousWord = Nothing} afterLine
    | otherwise = Left [i|psql's \\#{command} cannot run outside psql|]
  where
    (line, afterLine) = break (== '\n') afterBackslash
    command = takeWhile isWordChar line
    psqlVariable = case words (drop (length command) line) of
        name : _ -> all (\x -> isAsciiUpper x || isDigit x || x == '_') name
        [] -> True
    outputOnly = ["pset", "o", "out", "echo", "qecho", "timing", "x"]


-- | Adds text, in reading order, to the statement so far.
keep :: String -> ScanState -> ScanState
keep text state = state{statementRev = reverse text <> state.statementRev}


-- | Opens a quote or dollar quote with its delimiter. A quote is code, and parts
-- @BEGIN@ from @ATOMIC@.
openQuote :: Mode -> String -> ScanState -> ScanState
openQuote quoted delimiter state =
    (keep delimiter state){mode = quoted, hasCode = True, previousWord = Nothing}


-- | Closes a quote, dollar quote or comment with its delimiter.
backToPlain :: String -> ScanState -> ScanState
backToPlain delimiter state = (keep delimiter state){mode = Plain}


-- | Ends the word being read: it becomes the previous word, and may open or close a
-- @BEGIN ATOMIC@ body or a @CASE@ in one.
finishWord :: ScanState -> ScanState
finishWord state
    | null state.wordRev = state
    | otherwise =
        state
            { wordRev = ""
            , previousWord = Just word
            , atomicDepth = atomicDepthAfter state.previousWord word state.atomicDepth
            }
  where
    word = map toLower (reverse state.wordRev)


-- | The @BEGIN ATOMIC@ depth after a lowercased word, given the word before it.
atomicDepthAfter :: Maybe String -> String -> Int -> Int
atomicDepthAfter previous word depth
    | previous == Just "begin" && word == "atomic" && depth == 0 = 1
    | depth > 0 && word == "case" = depth + 1
    | depth > 0 && word == "end" = depth - 1
    | otherwise = depth


-- | Ends the statement so far, keeping it if it has code, and starts the next.
finishStatement :: ScanState -> ScanState
finishStatement state =
    startOfScript
        { statementsRev =
            if state.hasCode
                then T.strip (T.pack (reverse state.statementRev)) : state.statementsRev
                else state.statementsRev
        }


isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'


-- | The tag of a dollar quote and its closing @$@, from the text after the opening @$@.
-- A tag does not start with a digit, so @$1@ is a parameter.
dollarTag :: String -> Maybe String
dollarTag more = case span isWordChar more of
    (tag, '$' : _) | maybe True (not . isDigit) (listToMaybe tag) -> Just (tag <> "$")
    _ -> Nothing


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
