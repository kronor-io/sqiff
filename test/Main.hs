module Main (main) where

import Integration qualified
import Test.Tasty (defaultMain, testGroup)
import Unit qualified


main :: IO ()
main = do
    integration <- Integration.tests
    defaultMain (testGroup "sqiff" [Unit.tests, integration])
