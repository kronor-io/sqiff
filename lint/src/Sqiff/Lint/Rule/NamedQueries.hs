{-# LANGUAGE OverloadedStrings #-}

-- | R6: the questions sqiff keeps asking of its dependents have names.
module Sqiff.Lint.Rule.NamedQueries (rule) where

import Data.Text (Text)
import GHC.Hs
import Hopinion.Facts.Module (ModuleContext)
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id (RuleId (..))
import Sqiff.Lint.Syntax


rule :: Rule
rule =
    Rule
        { ruleId = RuleId "SqiffNamedQueries"
        , ruleText = "Ask Dependents through exactDependentsOf and routinesNaming, not by looking up its maps."
        , ruleWhy =
            "Map.findWithDefault Set.empty object deps.exact spells out how the answer is stored each time it is asked.\n\
            \The named query says what is asked, and is the one place to change if the storage does."
        , ruleFix =
            "Write exactDependentsOf object deps for the exact map, and routinesNaming object deps for the named one."
        , ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
        }


check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check context _ expressions =
    findingsResult
        [ findingAt (ruleId rule) context (getLocA e) ("Looks up Dependents' " <> field <> " map; use " <> query <> ".")
        | e <- expressions
        , let (function, arguments) = applicationOf e
        , variableName function == Just "findWithDefault"
        , [fallback, _, dependents] <- [arguments]
        , variableName fallback == Just "empty"
        , Just field <- [fieldName dependents]
        , Just query <- [lookup field queries]
        ]


-- | The named query for each of Dependents' maps.
queries :: [(Text, Text)]
queries = [("exact", "exactDependentsOf"), ("named", "routinesNaming")]
