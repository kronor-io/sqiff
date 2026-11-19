-- | sqiff's own hopinion rules, which check the parts of docs/readability.md a
-- tool can decide. hopinion's built-in rules are left out: they are another
-- project's style.
module Sqiff.Lint (main, rules) where

import Hopinion (hopinionWith)
import Hopinion.Rule (Rule)
import Sqiff.Lint.Rule.NamedFields qualified as NamedFields
import Sqiff.Lint.Rule.NamedQueries qualified as NamedQueries
import Sqiff.Lint.Rule.NamedTuples qualified as NamedTuples
import Sqiff.Lint.Rule.SmallWhere qualified as SmallWhere
import Sqiff.Lint.Rule.StepRole qualified as StepRole


main :: IO ()
main = hopinionWith rules


rules :: [Rule]
rules =
    [ SmallWhere.rule
    , NamedTuples.rule
    , StepRole.rule
    , NamedQueries.rule
    , NamedFields.rule
    ]
