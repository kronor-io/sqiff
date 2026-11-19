{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | R8: text built from literal pieces and values is written as the text it makes.
module Sqiff.Lint.Rule.Interpolate (rule) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (iii, __i)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc (SrcSpan (..), combineSrcSpans, srcSpanEndLine, srcSpanStartLine, unLoc)
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
        , ruleText =
            [iii|
                Write text as the text it makes, with Data.String.Interpolate's i, __i or iii,
                not assembled from pieces.
            |]
        , ruleWhy =
            [__i|
                A chain of <> between quoted pieces, a list of lines joined with unlines, or a literal continued across lines with string gaps all hide the text they make: the reader has to assemble it, and a missing space or newline is easy to miss.
                An interpolated string reads as the text it produces, with the values in their places.
            |]
        , ruleFix =
            [__i|
                Write the whole text with the i quasi-quoter from Data.String.Interpolate, each value as \#{value}.
                For a block of lines use __i, which drops the block's indentation and its first and last newline; for one line of prose wrapped in the source, iii, which turns every run of whitespace into one space.
                A block with no values in it can also be a multiline string literal (the MultilineStrings extension), which GHC dedents the same way.
                \#{} takes any expression, record fields included: \#{qualified t}, \#{def.statement}. Bind a long one to a name so the text stays readable.
                Joining lists, or values with no literal between them, is not text building: leave <> there. Neither is choosing lines one by one, as a list comprehension does.
            |]
        , ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
        }


-- | The fewest pieces a chain has to join to be reported.
shortest :: Int
shortest = 3


check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check context _ expressions =
    findingsResult $
        [ findingAt (ruleId rule) context (runSpan run) (T.concat ["Text joined from ", T.pack (show (length run)), " pieces with <> or ++."])
        | e <- outermost expressions
        , run <- concatRuns e
        , length run >= shortest
        , any isStringLiteral run
        ]
            <> [ findingAt (ruleId rule) context (getLocA e) (T.concat ["Text written as a list of ", T.pack (show count), " fixed lines."])
               | e <- expressions
               , Just count <- [fixedLinesJoined e]
               ]
            <> [ findingAt (ruleId rule) context (getLocA e) "A string literal continued across lines."
               | e <- expressions
               , isGappedLiteral e
               , spansLines e
               ]


-- | How many lines an expression joins, when it joins a written-out list of at least
-- 'shortest' lines, each a string literal or an interpolation, with unlines or
-- intercalate "\n": @T.unlines ["a", "b", "c"]@, @T.unlines $ [...]@, @f . T.unlines $ [...]@.
fixedLinesJoined :: LHsExpr GhcPs -> Maybe Int
fixedLinesJoined e = case unLoc e of
    HsApp _ f list | joinsLines f -> fixed list
    OpApp _ f operator list | variableName operator == Just "$", joinsLines f -> fixed list
    _ -> Nothing
  where
    fixed list = case unLoc (peelExpr list) of
        ExplicitList _ lines'
            | length lines' >= shortest, all isTextPiece lines' -> Just (length lines')
        _ -> Nothing


-- | Whether a function joins lines into text: unlines, intercalate "\n", or a
-- composition that applies one of them first.
joinsLines :: LHsExpr GhcPs -> Bool
joinsLines e = case unLoc (peelExpr e) of
    OpApp _ _ operator right | variableName operator == Just "." -> joinsLines right
    _ -> case applicationOf e of
        (function, []) -> variableName function == Just "unlines"
        (function, [separator]) -> variableName function == Just "intercalate" && isNewline separator
        _ -> False
  where
    isNewline s = case unLoc (peelExpr s) of
        HsLit _ (HsString _ t) -> t == "\n"
        HsOverLit _ OverLit{ol_val = HsIsString _ t} -> t == "\n"
        _ -> False


-- | An ordinary string literal, which runs over several lines only with string gaps;
-- a multiline string literal is meant to.
isGappedLiteral :: LHsExpr GhcPs -> Bool
isGappedLiteral e = case unLoc (peelExpr e) of
    HsLit _ HsString{} -> True
    HsOverLit _ OverLit{ol_val = HsIsString{}} -> True
    _ -> False


-- | A string literal or an interpolation.
isTextPiece :: LHsExpr GhcPs -> Bool
isTextPiece e =
    isStringLiteral e || case unLoc (peelExpr e) of
        HsUntypedSplice _ HsQuasiQuote{} -> True
        _ -> False


-- | Whether an expression's source runs over more than one line.
spansLines :: LHsExpr GhcPs -> Bool
spansLines e = case getLocA e of
    RealSrcSpan s _ -> srcSpanEndLine s > srcSpanStartLine s
    _ -> False


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
