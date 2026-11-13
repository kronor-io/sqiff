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
timed :: MonadIO m => Text -> m a -> m a
timed label action =
    liftIO (lookupEnv "SCHEMA_DIFF_TIMING") >>= \case
        Nothing -> action
        Just _ -> do
            start <- liftIO Clock.getMonotonicTimeNSec
            result <- action
            end <- liftIO Clock.getMonotonicTimeNSec
            let ms :: Word64 -> String
                ms n = printf "%.1f" (fromIntegral n / 1e6 :: Double)
            liftIO (hPut stderr (encodeUtf8 [i|timing\t#{label}\t#{ms start}\t#{ms (end - start)}\n|]))
            pure result


-- | With @SCHEMA_DIFF_TIMING@ set, evaluates @x@ and prints how long that took.
measure :: MonadIO m => Text -> Int -> m ()
measure label x =
    liftIO (lookupEnv "SCHEMA_DIFF_TIMING") >>= \case
        Nothing -> pure ()
        Just _ -> timed label (void (evaluate x))
