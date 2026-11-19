{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | H1: a constructor with many fields is built by field name.
module Sqiff.Lint.Rule.NamedFields (rule) where

import Data.Char (isUpper)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (iii, __i)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan (..))
import Hopinion.Facts.Module (ModuleContext)
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id (RuleId (..))
import Sqiff.Lint.Syntax


rule :: Rule
rule =
    Rule
        { ruleId = RuleId "SqiffNamedFields"
        , ruleText =
            [iii|
                Apply a constructor to at most four positional arguments; name the fields
                beyond that.
            |]
        , ruleWhy =
            [__i|
                Past a handful of arguments, a reader has to count to know which value is which, and two fields of one type can be swapped without the compiler noticing.
                Field names say at the call which value is which.
            |]
        , ruleFix =
            [__i|
                Give the type named fields and build it with them: Step{phase = CreatePhase, role = Creates, ...}.
                For tests, a builder with defaults and a record update for the fields the test is about.
            |]
        , ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
        }


-- | The most positional arguments allowed.
limit :: Int
limit = 4


check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check context _ expressions =
    findingsResult
        [ findingAt (ruleId rule) context location (T.concat [name, " applied to ", T.pack (show count), " positional arguments."])
        | (count, name, location) <- Map.elems (widest expressions)
        , count > limit
        ]


-- | For each constructor written in an application, by where it is written, the
-- most arguments it is given there, its name and its location. A partial
-- application inside a fuller one is the same constructor at the same place, so
-- only the fullest counts.
widest :: [LHsExpr GhcPs] -> Map.Map RealSrcSpan (Int, T.Text, SrcSpan)
widest expressions =
    Map.fromListWith
        (\a@(n, _, _) b@(m, _, _) -> if n >= m then a else b)
        [ (key, (length arguments, name, location))
        | e <- expressions
        , let (function, arguments) = applicationOf e
              location = getLocA function
        , RealSrcSpan key _ <- [location]
        , Just name <- [variableName function]
        , Just (initial, _) <- [T.uncons name]
        , isUpper initial
        ]
