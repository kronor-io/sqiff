{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | SQL text: which parts of it are code, quoted or comment, and how sqiff writes
-- literals and dollar-quoted bodies into it. Everything that edits SQL text by its words
-- goes through 'replaceInCode', so a string literal, quoted name or comment that happens
-- to contain the words is left alone.
module Sqiff.Sql (
    Region (..),
    regions,
    codeOnly,
    topWords,
    replaceInCode,
    replaceAllInCode,
    literal,
    dollarQuoted,
    doBlock,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Vector qualified as V


data Region = Code | Quoted | Remark
    deriving stock (Eq)


-- | Each character with whether it is code, inside quotes, or inside a comment.
regions :: Text -> [(Char, Region)]
regions = code ' ' . T.unpack
  where
    code prev = \case
        [] -> []
        '-' : '-' : rest -> let (c, more) = break (== '\n') rest in map (,Remark) ('-' : '-' : c) <> code ' ' more
        '/' : '*' : rest -> ('/', Remark) : ('*', Remark) : block (1 :: Int) rest
        e : '\'' : rest | e `elem` ("eE" :: String), not (wordChar prev) -> (e, Quoted) : ('\'', Quoted) : escaped rest
        '$' : rest
            | not (wordChar prev)
            , Just tag <- dollarTag rest ->
                map (,Quoted) ('$' : tag) <> dollar ('$' : tag) (drop (length tag) rest)
        q : rest | q == '\'' || q == '"' -> (q, Quoted) : inQuote q rest
        c : rest -> (c, Code) : code c rest
    inQuote q = \case
        [] -> []
        c : d : more | c == q && d == q -> (c, Quoted) : (d, Quoted) : inQuote q more
        c : more | c == q -> (c, Quoted) : code c more
        c : more -> (c, Quoted) : inQuote q more
    escaped = \case
        [] -> []
        '\\' : x : more -> ('\\', Quoted) : (x, Quoted) : escaped more
        '\'' : '\'' : more -> ('\'', Quoted) : ('\'', Quoted) : escaped more
        '\'' : more -> ('\'', Quoted) : code '\'' more
        c : more -> (c, Quoted) : escaped more
    dollar tag s = case L.stripPrefix tag s of
        Just after -> map (,Quoted) tag <> code '$' after
        Nothing -> case s of
            [] -> []
            c : more -> (c, Quoted) : dollar tag more
    dollarTag more = case span (\c -> isAlphaNum c || c == '_') more of
        (tag, '$' : _) | maybe True (not . isDigit) (listToMaybe tag) -> Just (tag <> "$")
        _ -> Nothing
    wordChar c = isAlphaNum c || c == '_' || c == '$'
    block depth = \case
        [] -> []
        '*' : '/' : more -> ('*', Remark) : ('/', Remark) : if depth == 1 then code '/' more else block (depth - 1) more
        '/' : '*' : more -> ('/', Remark) : ('*', Remark) : block (depth + 1) more
        c : more -> (c, Remark) : block depth more


-- | The text with its comments blanked out.
codeOnly :: Text -> Text
codeOnly t = T.pack [if r == Remark then ' ' else c | (c, r) <- regions t]


-- | The words outside quotes, comments and parentheses, lower case, with where they start.
topWords :: Text -> [(Int, Text)]
topWords t = go (0 :: Int) (zip [0 ..] (regions t))
  where
    go _ [] = []
    go depth xs@((k, (c, r)) : rest)
        | r /= Code = go depth rest
        | c == '(' = go (depth + 1) rest
        | c == ')' = go (depth - 1) rest
        | depth == 0 && (isAlpha c || c == '_') =
            let (w, more) = span (\(_, (ch, rr)) -> rr == Code && (isAlphaNum ch || ch == '_' || ch == '$')) xs
             in (k, T.toLower (T.pack (map (fst . snd) w))) : go depth more
        | otherwise = go depth rest


-- | @old@ replaced by @new@ where all of it is code, outside quotes and comments.
replaceInCode :: Text -> Text -> Text -> Text
replaceInCode old new t
    | T.null old = t
    | otherwise = go 0 t
  where
    kinds = V.fromList (map snd (regions t)) :: Vector Region
    inCode at = all (\k -> kinds V.!? k == Just Code) [at .. at + T.length old - 1]
    go offset rest = case T.breakOn old rest of
        (before, "") -> before
        (before, match) ->
            let at = offset + T.length before
             in if inCode at
                    then before <> new <> go (at + T.length old) (T.drop (T.length old) match)
                    else before <> T.take 1 match <> go (at + 1) (T.drop 1 match)


-- | Each pair's replacement, in order, in code only.
replaceAllInCode :: [(Text, Text)] -> Text -> Text
replaceAllInCode pairs t = foldl' (\acc (old, new) -> replaceInCode old new acc) t pairs


-- | A string literal, like PostgreSQL's @quote_literal@: quotes doubled, and an @E@
-- string with backslashes doubled when there is one, so it reads the same whatever
-- @standard_conforming_strings@ is.
literal :: Text -> Text
literal t
    | "\\" `T.isInfixOf` t = "E'" <> T.replace "\\" "\\\\" (T.replace "'" "''" t) <> "'"
    | otherwise = "'" <> T.replace "'" "''" t <> "'"


-- | @body@ between dollar quotes whose tag it does not contain.
dollarQuoted :: Text -> Text
dollarQuoted body = quote <> body <> quote
  where
    -- The candidates never run out, so the fallback is never taken.
    quote = fromMaybe "$$" (L.find (\q -> not (q `T.isInfixOf` (body <> "$"))) candidates)
    candidates = "$$" : ["$sqiff" <> suffix <> "$" | suffix <- "" : map tshow [1 :: Int ..]]


-- | An anonymous PL/pgSQL block.
doBlock :: Text -> Text
doBlock body = "do " <> dollarQuoted body
