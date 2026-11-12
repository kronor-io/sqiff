{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Phase timings on stderr, when @SCHEMA_DIFF_TIMING@ is set.
module Kronor.SchemaDiff.Timing (timed, measure) where

import Data.ByteString (hPut)
import Data.String.Interpolate (i)
import GHC.Clock qualified as Clock
import RIO
import System.Environment (lookupEnv)
import Text.Printf (printf)


-- | Runs @action@; with @SCHEMA_DIFF_TIMING@ set, prints its label, start and duration in
-- milliseconds to stderr.
timed :: Text -> IO a -> IO a
timed label action =
    lookupEnv "SCHEMA_DIFF_TIMING" >>= \case
        Nothing -> action
        Just _ -> do
            start <- Clock.getMonotonicTimeNSec
            result <- action
            end <- Clock.getMonotonicTimeNSec
            let ms :: Word64 -> String
                ms n = printf "%.1f" (fromIntegral n / 1e6 :: Double)
            -- One write per line, so lines from several threads do not interleave.
            hPut stderr (encodeUtf8 [i|timing\t#{label}\t#{ms start}\t#{ms (end - start)}\n|])
            pure result


-- | With @SCHEMA_DIFF_TIMING@ set, evaluates @x@ and prints how long that took.
measure :: Text -> Int -> IO ()
measure label x =
    lookupEnv "SCHEMA_DIFF_TIMING" >>= \case
        Nothing -> pure ()
        Just _ -> timed label (void (evaluate x))
