{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Where a database is, as libpq takes it, with a redacted form that never shows the
-- password.
module Sqiff.ConnectionString (
    ConnectionString,
    connectionString,
    fromPairs,
    libpqText,
    redacted,
) where

import Data.Char (isSpace)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO


-- | A libpq connection string, in keyword or URI form.
data ConnectionString
    = -- | The empty string: libpq's environment (@PGHOST@, @PGDATABASE@, a service file)
      -- names the database.
      FromEnvironment
    | ConnInfo Text
    deriving stock (Eq)


-- | Shows the redacted form, so a password cannot reach a log through 'show'.
instance Show ConnectionString where
    show = T.unpack . redacted


instance Display ConnectionString where
    textDisplay = redacted


connectionString :: Text -> ConnectionString
connectionString t
    | T.all isSpace t = FromEnvironment
    | otherwise = ConnInfo t


-- | A keyword connection string from its settings, each value quoted.
fromPairs :: [(Text, Text)] -> ConnectionString
fromPairs pairs = ConnInfo (T.unwords [[i|#{k}='#{escape v}'|] | (k, v) <- pairs])
  where
    escape = T.replace "'" "\\'" . T.replace "\\" "\\\\"


-- | The text to give libpq.
libpqText :: ConnectionString -> Text
libpqText = \case
    FromEnvironment -> ""
    ConnInfo t -> t


-- | The connection string for messages: the password replaced by @***@.
redacted :: ConnectionString -> Text
redacted = \case
    FromEnvironment -> "the database PGHOST, PGPORT and PGDATABASE name"
    ConnInfo t
        | any (`T.isPrefixOf` t) ["postgres://", "postgresql://"] -> redactUri t
        | otherwise -> redactKeywords t


-- | @postgresql://user:secret\@host/db?password=secret@ without either secret.
redactUri :: Text -> Text
redactUri t =
    let (scheme, rest) = T.breakOnEnd "://" t
        (authority, path) = T.break (`elem` ("/?" :: String)) rest
        authority' = case T.breakOn "@" authority of
            (userInfo, at)
                | not (T.null at)
                , (user, colon) <- T.breakOn ":" userInfo
                , not (T.null colon) ->
                    [i|#{user}:***#{at}|]
            _ -> authority
        (beforeQuery, query) = T.breakOn "?" path
        query' = case T.stripPrefix "?" query of
            Just params -> "?" <> T.intercalate "&" (map redactParam (T.split (== '&') params))
            Nothing -> query
        redactParam p = case T.breakOn "=" p of
            (key, value) | key == "password", not (T.null value) -> "password=***"
            _ -> p
     in scheme <> authority' <> beforeQuery <> query'


-- | @host=h password='a secret'@ as @host=h password=***@; values may be quoted.
redactKeywords :: Text -> Text
redactKeywords = go
  where
    go t = case T.breakOn "password" t of
        (before, "") -> before
        (before, at) ->
            let afterKey = T.drop (T.length "password") at
                (spaces, rest) = T.span isSpace afterKey
                startsKey = T.null before || isSpace (T.last before)
             in case T.stripPrefix "=" rest of
                    Just value | startsKey -> [i|#{before}password=***#{go (dropValue (T.stripStart value))}|]
                    _ -> [i|#{before}password#{spaces}#{go rest}|]
    dropValue v = case T.uncons v of
        Just ('\'', quoted) -> closing quoted
        _ -> T.dropWhile (not . isSpace) v
    closing v = case T.uncons v of
        Nothing -> ""
        Just ('\\', rest) -> closing (T.drop 1 rest)
        Just ('\'', rest) -> rest
        Just (_, rest) -> closing rest
