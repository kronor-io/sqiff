{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sqiff (main) where

import Data.String.Interpolate (i)
import Data.Text.IO qualified as TIO
import RIO
import Sqiff.App.Compare (runDescribe, runDiff, runPlan, runSqitch)
import Sqiff.App.Env (loadAppEnv)
import Sqiff.App.Options (Command (..), DescribeOptions (..), SqitchOptions (..), parseCommand)
import Sqiff.App.Table (tableCommand)
import Sqiff.Connection (UserError (..))
import Sqiff.Sqitch (requireSqitch)


-- | Runs the command the arguments name. A 'UserError' is one line on stderr and exit
-- code 2, as is any other failure; 'ExitCode' passes through.
main :: IO ()
main =
    handleAny unexpected
        . handle userError
        $ parseCommand
        >>= \case
            Describe opts -> do
                env <- loadAppEnv "." opts.scope
                runRIO env (runDescribe opts)
            Diff from toDb scope -> do
                env <- loadAppEnv "." scope
                runRIO env (runDiff from toDb)
            Plan from toDb scope -> do
                env <- loadAppEnv "." scope
                runRIO env (runPlan from toDb)
            Sqitch from toDb scope opts -> do
                requireSqitch
                env <- loadAppEnv opts.projectDir scope
                runRIO env (runSqitch from toDb opts)
            TableFile file db opts -> tableCommand file db opts
  where
    userError (UserError message) = TIO.hPutStrLn stderr [i|sqiff: #{message}|] >> exitWith (ExitFailure 2)
    unexpected e = case fromException e of
        Just code -> throwIO (code :: ExitCode)
        Nothing -> TIO.hPutStrLn stderr [i|sqiff: #{displayException e}|] >> exitWith (ExitFailure 2)
