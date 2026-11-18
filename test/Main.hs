module Main (main) where

import Integration qualified
import Test.Tasty (defaultMain, testGroup)
import Unit qualified
import Writing qualified


main :: IO ()
main = do
    integration <- Integration.tests
    writing <- Writing.tests
    defaultMain (testGroup "sqiff" [Unit.tests, integration, writing])
