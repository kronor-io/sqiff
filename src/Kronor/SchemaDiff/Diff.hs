{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Kronor.SchemaDiff.Diff (
    Change (..),
    diff,
    affected,
    BodyText (..),
    bodyText,
    dependentsBy,
    reachedFrom,
    bodiesToAnalyse,
) where

import Data.Map.Merge.Strict qualified as Merge
import Kronor.SchemaDiff.Catalog (Address (..), Aspect (..), Edge (..), ObjectKey (..), Schema)
import RIO
import RIO.Char (isAlphaNum)
import RIO.HashSet qualified as HS
import RIO.Map qualified as Map
import RIO.NonEmpty qualified as L
import RIO.Set qualified as Set
import RIO.Text qualified as T


data Change
    = Added Text
    | Dropped Text
    | Changed Text Text
    deriving stock (Eq, Show)


-- | What turns @from@ into @to@, with @same key old new@ deciding when two definitions match.
diff :: (ObjectKey -> Text -> Text -> Bool) -> Schema -> Schema -> Map ObjectKey Change
diff same =
    Merge.merge
        (Merge.mapMissing \_ old -> Dropped old)
        (Merge.mapMissing \_ new -> Added new)
        (Merge.zipWithMaybeMatched \k old new -> if same k old new then Nothing else Just (Changed old new))


-- | For each dropped or changed definition, the objects in @known@ that depend on it,
-- directly or through others, and are not in the diff themselves.
affected :: Set Address -> [Edge] -> Map ObjectKey Change -> Map Address (Set Address)
affected known edges changes =
    Map.filter (not . Set.null)
        $ Map.fromList
            [ (key.address, closure key.address `Set.difference` inDiff)
            | (key, change) <- Map.toList changes
            , key.aspect == Definition
            , removesOrChanges change
            ]
  where
    inDiff = Set.fromList (map (.address) (Map.keys changes))

    dependents =
        Map.fromListWith
            (<>)
            [ (edge.referenced, Set.singleton edge.dependent)
            | edge <- edges
            , edge.referenced `Set.member` known
            , edge.dependent `Set.member` known
            ]

    direct addr = Map.findWithDefault Set.empty addr dependents

    closure start = go Set.empty (direct start)
      where
        go seen frontier
            | Set.null frontier = seen
            | otherwise =
                let seen' = seen <> frontier
                 in go seen' (foldMap direct frontier `Set.difference` seen')

    removesOrChanges = \case
        Added _ -> False
        Dropped _ -> True
        Changed _ _ -> True


-- | A definition lowercased, and its words, as 'bodiesToAnalyse' compares them with names.
data BodyText = BodyText
    { lowered :: Text
    , wordSet :: HashSet Text
    }


bodyText :: Text -> BodyText
bodyText def = BodyText lower (wordsOf lower)
  where
    lower = T.toLower def


wordsOf :: Text -> HashSet Text
wordsOf = HS.fromList . filter (not . T.null) . T.split (not . identifierChar)
  where
    identifierChar c = isAlphaNum c || c == '_' || c == '$'


-- | Who depends on an object by the edges given; a relation also counts what depends on
-- its columns.
dependentsBy :: [Edge] -> Address -> [Address]
dependentsBy edges = \addr -> direct addr <> concatMap direct (columnsOf addr)
  where
    deps = Map.fromListWith (<>) [(e.referenced, [e.dependent]) | e <- edges]
    direct addr = Map.findWithDefault [] addr deps
    columns = Map.fromListWith (<>) [((s, t), Set.singleton e.referenced) | e <- edges, e.referenced.objectType == "table column", [s, t, _] <- [e.referenced.objectNames]]
    columnsOf addr = case addr.objectNames of
        [s, n] | addr.objectType `elem` ["table", "view", "materialized view", "foreign table"] -> maybe [] Set.toList (Map.lookup (s, n) columns)
        _ -> []


-- | The objects reachable from @starts@ by one step or more of @dependents@.
reachedFrom :: (Address -> [Address]) -> Set Address -> Set Address
reachedFrom dependents starts = go Set.empty (Set.fromList (concatMap dependents (Set.toList starts)))
  where
    go seen frontier
        | Set.null frontier = seen
        | otherwise =
            let seen' = seen <> frontier
             in go seen' (Set.fromList (concatMap dependents (Set.toList frontier)) `Set.difference` seen')


-- | Of @functions@, those whose body edges 'affected' can follow from @starts@: their
-- definition names a reached object, or they are a trigger function on the closure.
bodiesToAnalyse :: (Address -> [Address]) -> Set Address -> [(a, Address, BodyText, Maybe Address)] -> [a]
bodiesToAnalyse dependents starts functions =
    [f | (f, _, def, trigger) <- functions, named def || maybe False (`Set.member` closure) trigger]
  where
    reached = reachedFrom dependents starts
    closure = starts <> reached
    counted = Set.toList reached <> filter target (Set.toList starts)
    names = HS.toList (foldMap nameWords counted)
    operators = [op | a <- counted, a.objectType == "operator", [_, op] <- [a.objectNames]]
    named def = any (`HS.member` def.wordSet) names || any (`T.isInfixOf` def.lowered) operators
    target addr = addr.objectType `notElem` ["table column", "table constraint", "domain constraint", "trigger", "policy", "index", "schema", "extension"]
    nameWords addr = wordsOf $ T.toLower case addr.objectNames of
        [n] -> L.last ("" :| T.split (== '.') n)
        [_, n] -> n
        _ : t : _ -> t
        [] -> ""
