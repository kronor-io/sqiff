{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | R8: text built from literal pieces and values is written as the text it makes.
module Sqiff.Lint.Rule.Interpolate (rule) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc (SrcSpan, combineSrcSpans, unLoc)
import Hopinion.Extract.Ghc (infixOperands, peelExpr, rdrText, spineOf)
import Hopinion.Facts.Module (ModuleContext)
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id (RuleId (..))
import Sqiff.Lint.Syntax


rule :: Rule
rule =
    Rule
        { ruleId = RuleId "SqiffInterpolate"
        , ruleText = "Write text made of literal pieces and values with [i|...|], not a chain of <> or ++."
        , ruleWhy =
            "A chain of <> between quoted pieces hides the text it makes: the reader has to assemble it, and a missing space between two pieces is easy to miss.\n\
            \An interpolated string reads as the text it produces, with the values in their places."
        , ruleFix =
            "Write the whole text as [i|...|] from Data.String.Interpolate, with each value as #{value}.\n\
            \Inside #{} keep to names and function applications; bind a record field or an operator expression to a name first.\n\
            \Joining lists, or two values with no literal between them, is not text building: leave <> there."
        , ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
        }


-- | The fewest pieces a chain has to join to be reported.
shortest :: Int
shortest = 3


check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check context _ expressions =
    findingsResult
        [ findingAt (ruleId rule) context (runSpan run) (T.concat ["Text joined from ", T.pack (show (length run)), " pieces with <> or ++."])
        | e <- outermost expressions
        , run <- concatRuns e
        , length run >= shortest
        , any isStringLiteral run
        ]


-- | The infix expressions that are not themselves an operand of another one, so a
-- chain is looked at once, as a whole.
outermost :: [LHsExpr GhcPs] -> [LHsExpr GhcPs]
outermost expressions = [e | e <- infixes, getLocA e `notElem` nested]
  where
    infixes = [e | e <- expressions, Just _ <- [infixOperands e]]
    nested =
        [ getLocA operand
        | e <- infixes
        , Just (l, r) <- [infixOperands e]
        , operand <- map peelExpr [l, r]
        , Just _ <- [infixOperands operand]
        ]


-- | The operands of each run of concatenation in a chain, in source order. A chain
-- such as @a <> b == c <> d@ holds two runs.
concatRuns :: LHsExpr GhcPs -> [NonEmpty (LHsExpr GhcPs)]
concatRuns e = runs (leftmost :| []) joins
  where
    (leftmost, joins) = spineOf e
    runs current = \case
        [] -> close current
        (operator, next) : more
            | isConcatenation operator -> runs (NE.cons next current) more
            | otherwise -> close current <> runs (next :| []) more
    close current = [NE.reverse current | length current > 1]


isConcatenation :: LHsExpr GhcPs -> Bool
isConcatenation e = case unLoc e of
    HsVar _ name -> rdrText (unLoc name) `elem` ["<>", "++"]
    _ -> False


-- | A string literal, overloaded or not.
isStringLiteral :: LHsExpr GhcPs -> Bool
isStringLiteral e = case unLoc (peelExpr e) of
    HsLit _ HsString{} -> True
    HsLit _ HsMultilineString{} -> True
    HsOverLit _ OverLit{ol_val = HsIsString{}} -> True
    _ -> False


runSpan :: NonEmpty (LHsExpr GhcPs) -> SrcSpan
runSpan run = combineSrcSpans (getLocA (NE.head run)) (getLocA (NE.last run))
