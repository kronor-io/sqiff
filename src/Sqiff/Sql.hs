{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | SQL text: which parts of it are code, quoted or comment, and how sqiff writes
-- literals and dollar-quoted bodies into it. Everything that edits SQL text by its words
-- goes through 'replaceInCode', so a string literal, quoted name or comment that happens
-- to contain the words is left alone.
module Sqiff.Sql (
    Region (..),
    regions,
    closed,
    codeOnly,
    topWords,
    codeWords,
    parenthesised,
    splitTopLevel,
    replaceInCode,
    replaceAllInCode,
    lowerCode,
    literal,
    dollarQuoted,
    doBlock,
    terminator,
) where

import Data.Char (isAlpha, isAlphaNum, isAsciiUpper, isDigit, isSpace, toLower)
import Data.String.Interpolate (i)
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


-- | Whether every quote, dollar quote and block comment in the text is closed: a newline
-- after it is code, not inside one of them.
closed :: Text -> Bool
closed t = fmap snd (L.lastMaybe (regions (t <> "\n"))) == Just Code


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


-- | The words, split at whitespace and at the characters in @separators@ where they are
-- code; a quoted name or literal stays inside its word, whatever it holds, and comments
-- separate words.
codeWords :: String -> Text -> [Text]
codeWords separators t = map T.pack (go [] (regions t))
  where
    go acc [] = done acc []
    go acc ((c, r) : rest)
        | r == Remark || (r == Code && (isSpace c || c `elem` separators)) = done acc (go [] rest)
        | otherwise = go (c : acc) rest
    done acc more = if null acc then more else reverse acc : more


-- | What the first parentheses in code hold, without them.
parenthesised :: Text -> Maybe Text
parenthesised t = case dropWhile (\(c, r) -> not (r == Code && c == '(')) (regions t) of
    [] -> Nothing
    _ : rest -> T.pack . map fst <$> inside (0 :: Int) [] rest
  where
    inside _ _ [] = Nothing
    inside depth acc ((c, r) : more)
        | r == Code && c == ')' && depth == 0 = Just (reverse acc)
        | r == Code && c == ')' = inside (depth - 1) ((c, r) : acc) more
        | r == Code && c == '(' = inside (depth + 1) ((c, r) : acc) more
        | otherwise = inside depth ((c, r) : acc) more


-- | The parts between @separator@s in code at the top level, outside quotes and
-- parentheses, stripped.
splitTopLevel :: Char -> Text -> [Text]
splitTopLevel separator t = map (T.strip . T.pack) (go (0 :: Int) [] (regions t))
  where
    go _ acc [] = [reverse acc]
    go depth acc ((c, r) : rest)
        | r == Code && c == separator && depth == 0 = reverse acc : go depth [] rest
        | r == Code && c == '(' = go (depth + 1) (c : acc) rest
        | r == Code && c == ')' = go (depth - 1) (c : acc) rest
        | otherwise = go depth (c : acc) rest


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


-- | The text with its code in lower case, as sqiff writes SQL, keeping quotes and
-- comments as they are. It reads the same to PostgreSQL, which folds unquoted words to
-- lower case: catalog functions such as @pg_get_viewdef@ quote every name that is not
-- already lower case. Only ASCII letters are folded, as PostgreSQL folds them.
lowerCode :: Text -> Text
lowerCode t = T.pack [if r == Code && isAsciiUpper c then toLower c else c | (c, r) <- regions t]


-- | Each pair's replacement, in order, in code only.
replaceAllInCode :: [(Text, Text)] -> Text -> Text
replaceAllInCode pairs t = foldl' (\acc (old, new) -> replaceInCode old new acc) t pairs


-- | A string literal, like PostgreSQL's @quote_literal@: quotes doubled, and an @E@
-- string with backslashes doubled when there is one, so it reads the same whatever
-- @standard_conforming_strings@ is.
literal :: Text -> Text
literal t
    | "\\" `T.isInfixOf` t = let escaped = T.replace "\\" "\\\\" doubled in [i|E'#{escaped}'|]
    | otherwise = [i|'#{doubled}'|]
  where
    doubled = T.replace "'" "''" t


-- | @body@ between dollar quotes whose tag it does not contain.
dollarQuoted :: Text -> Text
dollarQuoted body = quote <> body <> quote
  where
    -- The candidates never run out, so the fallback is never taken.
    quote = fromMaybe "$$" (L.find (\q -> not (q `T.isInfixOf` (body <> "$"))) candidates)
    candidates = "$$" : [[i|$sqiff#{suffix}$|] | suffix <- "" : map tshow [1 :: Int ..]]


-- | An anonymous PL/pgSQL block.
doBlock :: Text -> Text
doBlock body = "do " <> dollarQuoted body


-- | What ends a statement in a psql script: nothing after a "--" note or a query psql
-- runs with @\\gexec@, a semicolon otherwise.
terminator :: Text -> Text
terminator sql
    | "--" `T.isPrefixOf` sql || "\\gexec" `T.isSuffixOf` sql = ""
    | otherwise = ";"
