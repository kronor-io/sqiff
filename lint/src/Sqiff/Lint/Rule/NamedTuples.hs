{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | R2: a top-level signature does not pass wide or nested tuples.
module Sqiff.Lint.Rule.NamedTuples (rule) where

import Data.Generics (listify)
import Data.String.Interpolate (iii, __i)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Hopinion.Extract.Ghc (rdrText)
import Hopinion.Facts.Module (ModuleContext)
import Hopinion.Parse (ParsedModule, parsedModuleDecls)
import Hopinion.Rule
import Hopinion.Rule.Id (RuleId (..))
import Sqiff.Lint.Syntax


rule :: Rule
rule =
    Rule
        { ruleId = RuleId "SqiffNamedTuples"
        , ruleText =
            [iii|
                A top-level signature takes and returns no tuple of three or more, and no
                tuple inside a tuple.
            |]
        , ruleWhy =
            [__i|
                A tuple's parts are told apart only by position, so every caller has to remember which is which, and two parts of one type can be swapped silently.
                A pair is often obvious; three parts, or tuples inside tuples, rarely are.
            |]
        , ruleFix =
            [__i|
                Make a record whose field names say what each part is, as ComparisonInputs{source, target, bodyChecks} replaced ((Schema, Dependencies), (Schema, Dependencies), checks).
                Keep a local intermediate as a tuple if it never crosses a function boundary.
            |]
        , ruleImpl = RuleImplModule (ModuleCheckFromSource check)
        }


check :: ModuleContext -> ParsedModule -> CheckResult
check context parsed =
    findingsResult
        [ findingAt (ruleId rule) context (getLocA name) (T.concat [rdrText (unLoc name), "'s signature has ", problem, "."])
        | L _ (SigD _ (TypeSig _ names signature)) <- parsedModuleDecls parsed
        , name <- take 1 names
        , problem <- take 1 (tupleProblems signature)
        ]


-- | What is wrong with the tuples a signature mentions, worst first.
tupleProblems :: LHsSigWcType GhcPs -> [T.Text]
tupleProblems signature =
    ["a tuple inside a tuple" | any (any isTuple . tupleParts) tuples]
        <> [T.concat ["a tuple of ", T.pack (show (length (tupleParts t)))] | t <- tuples, length (tupleParts t) >= 3]
  where
    tuples = listify isTuple signature
    isTuple :: HsType GhcPs -> Bool
    isTuple = \case
        HsTupleTy _ HsBoxedOrConstraintTuple parts -> length parts >= 2
        _ -> False
    tupleParts = \case
        HsTupleTy _ _ parts -> map (unLoc . peelParens) parts
        _ -> []
    peelParens t = case unLoc t of
        HsParTy _ inner -> peelParens inner
        _ -> t
