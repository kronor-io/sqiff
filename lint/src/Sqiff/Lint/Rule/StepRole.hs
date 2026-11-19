{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | R3 for steps: what a step does is in its role, not in its SQL.
module Sqiff.Lint.Rule.StepRole (rule) where

import Data.Maybe (mapMaybe)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Facts.Module (ModuleContext)
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id (RuleId (..))
import Sqiff.Lint.Syntax


rule :: Rule
rule =
    Rule
        { ruleId = RuleId "SqiffStepRole"
        , ruleText = "Decide what a step does from its role, not by reading its SQL."
        , ruleWhy =
            [__i|
                A step's SQL is output. Code that searches it for a phrase breaks silently when the SQL is worded differently, and hides what it is asking.
                The builder that makes a step knows what it does, and says so in its role.
            |]
        , ruleFix =
            [__i|
                Read the step's role, or the hazard or field that holds the fact.
                If no field holds it, add one where the step is built.
                Code that only writes the SQL out, such as adding the semicolon, can carry an [allow:SqiffStepRole] annotation saying so.
            |]
        , ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
        }


check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check context _ expressions =
    findingsResult
        [ findingAt (ruleId rule) context (getLocA e) ("Reads a step's SQL with " <> test <> ".")
        | e <- expressions
        , Just test <- [textTestOnSql e]
        ]


-- | The text function an expression applies to a @.sql@ field, prefix or infix.
textTestOnSql :: LHsExpr GhcPs -> Maybe Text
textTestOnSql e = case unLoc e of
    OpApp _ left operator right -> testOn operator [left, right]
    _ -> let (function, arguments) = applicationOf e in testOn function arguments
  where
    testOn function arguments = do
        name <- variableName function
        if name `elem` textTests && "sql" `elem` mapMaybe fieldName arguments then Just name else Nothing


-- | The functions that ask a text what it contains.
textTests :: [Text]
textTests = ["isInfixOf", "isPrefixOf", "isSuffixOf", "stripPrefix", "stripSuffix", "breakOn", "splitOn", "count"]
