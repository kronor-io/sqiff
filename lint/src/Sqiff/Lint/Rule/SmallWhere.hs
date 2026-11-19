{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | R1: a function's where block does not hide a program of its own.
module Sqiff.Lint.Rule.SmallWhere (rule) where

import Data.Text qualified as T
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
        { ruleId = RuleId "SqiffSmallWhere"
        , ruleText = "Keep a where block to at most twelve bindings; give the stages of a bigger one names of their own."
        , ruleWhy =
            "Bindings in a where block see every argument and every other binding, so a reader cannot tell what each depends on without reading them all.\n\
            \A large block is usually several jobs sharing one scope, as findingsFor's thirty-nine bindings were."
        , ruleFix =
            "Find the stages the block computes and make each a top-level function with a signature and explicit arguments.\n\
            \Keep small helpers local; the function that remains should read as the list of its stages.\n\
            \Where the size is the right shape, say why in an [allow:SqiffSmallWhere] annotation."
        , ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
        }


-- | The most bindings a where block may hold.
limit :: Int
limit = 12


check :: ModuleContext -> ParsedModule -> [HsBind GhcPs] -> CheckResult
check context _ binds =
    findingsResult
        [ findingAt (ruleId rule) context (getLocA name) (T.concat ["A where block of ", T.pack (show count), " bindings."])
        | FunBind{fun_id = name, fun_matches = matches} <- binds
        , let count = maximum (0 : map (localBindings . unLoc) (unLoc (mg_alts matches)))
        , count > limit
        ]


-- | How many values an equation's where block binds.
localBindings :: Match GhcPs (LHsExpr GhcPs) -> Int
localBindings m = case grhssLocalBinds (m_grhss m) of
    HsValBinds _ (ValBinds _ bindings _) -> length bindings
    _ -> 0
