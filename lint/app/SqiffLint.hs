-- | The sqiff-lint executable: hopinion with sqiff's own rules.
module SqiffLint (main) where

import Sqiff.Lint qualified


main :: IO ()
main = Sqiff.Lint.main
