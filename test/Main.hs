module Main (main) where

import Integration qualified
import Rewriting qualified
import Test.Tasty (defaultMain, testGroup)
import Unit qualified
import Writing qualified


main :: IO ()
main = do
    sqitch <- Writing.setUpSqitch
    integration <- Integration.tests sqitch
    defaultMain (testGroup "sqiff" [Unit.tests, Rewriting.tests, integration, Writing.tests sqitch])
