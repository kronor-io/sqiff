{-# LANGUAGE LambdaCase #-}

-- | Taking expressions and signatures apart the way sqiff's rules read them.
module Sqiff.Lint.Syntax (
    applicationOf,
    variableName,
    fieldName,
    findingAt,
) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (unpackFS)
import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (SrcSpan, unLoc)
import Hopinion.Extract.Ghc (declScopeOf, peelExpr, spanOfSrcSpan)
import Hopinion.Facts.Module (ModuleContext, moduleContextDecls, moduleContextPath, moduleContextRef)
import Hopinion.Rule (Finding (..))
import Hopinion.Rule.Id (RuleId)
import Language.Haskell.Syntax.Basic (FieldLabelString (..))


-- | What an expression applies, and its value arguments in order. Type
-- applications are skipped; anything that is not an application is itself, with
-- no arguments.
applicationOf :: LHsExpr GhcPs -> (LHsExpr GhcPs, [LHsExpr GhcPs])
applicationOf = go []
  where
    go arguments e = case unLoc e of
        HsApp _ f a -> go (a : arguments) f
        HsAppType _ f _ -> go arguments f
        _ -> (e, arguments)


-- | The name a variable or constructor is written with, without its qualifier.
variableName :: LHsExpr GhcPs -> Maybe Text
variableName e = case unLoc (peelExpr e) of
    HsVar _ name -> Just (T.pack (occNameString (rdrNameOcc (unLoc name))))
    _ -> Nothing


-- | The field a record-dot projection reads, as in @step.sql@.
fieldName :: LHsExpr GhcPs -> Maybe Text
fieldName e = case unLoc (peelExpr e) of
    HsGetField{gf_field = field} -> Just (T.pack (unpackFS (field_label (unLoc (dfoLabel (unLoc field))))))
    _ -> Nothing


-- | A finding of @rule@ at @location@, scoped to the declaration it is in.
findingAt :: RuleId -> ModuleContext -> SrcSpan -> Text -> Finding
findingAt rule context location message =
    Finding
        { findingRule = rule
        , findingScope = declScopeOf (moduleContextRef context) (moduleContextDecls context) at
        , findingSpan = at
        , findingMessage = message
        }
  where
    at = spanOfSrcSpan (moduleContextPath context) location
