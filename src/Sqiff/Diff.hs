{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sqiff.Diff (
    Change (..),
    Changes,
    invert,
    after,
    diff,
    affected,
    BodyText (..),
    bodyText,
    bodiesToAnalyse,
) where

import Data.Map.Merge.Strict qualified as Merge
import RIO
import RIO.Char (isAlphaNum)
import RIO.HashSet qualified as HS
import RIO.Map qualified as Map
import RIO.NonEmpty qualified as L
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Sqiff.Definition (Definition)
import Sqiff.Dependencies (Dependencies, directDependents, reachableFrom, restrictedTo)
import Sqiff.Graph (reachedFrom)
import Sqiff.Identifier (QName (..), TypeName, nameText, typeText)
import Sqiff.Model


-- | How one aspect of one object differs between two schemas.
data Change a
    = Added a
    | Dropped a
    | Changed a a
    deriving stock (Eq, Show, Functor, Foldable, Traversable)


-- | What differs between two schemas.
type Changes = Map ObjectKey (Change Definition)


-- | The change that undoes this one.
invert :: Change a -> Change a
invert = \case
    Added new -> Dropped new
    Dropped old -> Added old
    Changed old new -> Changed new old


-- | The definition a change leaves, if it leaves one.
after :: Change a -> Maybe a
after = \case
    Added new -> Just new
    Changed _ new -> Just new
    Dropped _ -> Nothing


-- | What turns @from@ into @to@, with @same key old new@ deciding when two definitions match.
diff :: Eq a => (ObjectKey -> a -> a -> Bool) -> Map ObjectKey a -> Map ObjectKey a -> Map ObjectKey (Change a)
diff same =
    Merge.merge
        (Merge.mapMissing \_ old -> Dropped old)
        (Merge.mapMissing \_ new -> Added new)
        (Merge.zipWithMaybeMatched \k old new -> if same k old new then Nothing else Just (Changed old new))


-- | For each dropped or changed definition, the objects in @known@ that depend on it,
-- directly or through others, and whose definitions are not in the diff themselves.
affected :: Set ObjectId -> Dependencies -> Map ObjectKey (Change a) -> Map ObjectId (Set ObjectId)
affected known deps changes =
    Map.filter (not . Set.null)
        $ Map.fromList
            [ (key.object, closure key.object `Set.difference` inDiff)
            | (key, change) <- Map.toList changes
            , key.aspect == Definition
            , removesOrChanges change
            ]
  where
    -- Objects whose own definition changes are planned by their own recipes; one whose
    -- comment or grants change still stands in the way.
    inDiff = Set.fromList [k.object | k <- Map.keys changes, k.aspect == Definition]

    amongKnown = restrictedTo known deps
    closure start = reachedFrom (Set.toList . directDependents amongKnown) (Set.singleton start)

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


-- | Of @functions@, those whose body edges 'affected' can follow from @starts@: their
-- definition names a reached object, or they are a trigger function on the closure.
bodiesToAnalyse :: Dependencies -> Set ObjectId -> [(a, ObjectId, BodyText, Maybe ObjectId)] -> [a]
bodiesToAnalyse deps starts functions =
    [f | (f, _, def, trigger) <- functions, named def || maybe False (`Set.member` closure) trigger]
  where
    reached = reachableFrom deps starts
    closure = starts <> reached
    counted = Set.toList reached <> filter namedInBodies (Set.toList starts)
    names = HS.toList (foldMap (wordsOf . T.toLower . bodyName) counted)
    operators = [op | Unknown a <- counted, a.objectType == "operator", [_, op] <- [a.objectNames]]
    named def = any (`HS.member` def.wordSet) names || any (`T.isInfixOf` def.lowered) operators


-- | Whether a body names this object itself when it uses it, rather than its table or type.
namedInBodies :: ObjectId -> Bool
namedInBodies = \case
    Column{} -> False
    TableConstraint{} -> False
    DomainConstraint{} -> False
    Trigger{} -> False
    Policy{} -> False
    Index _ -> False
    Schema _ -> False
    Extension _ -> False
    _ -> True


-- | The name a function body would use for the object: a part's table, a type's last name.
bodyName :: ObjectId -> Text
bodyName = \case
    Schema s -> nameText s
    Extension e -> nameText e
    Type t -> lastPart t
    DomainConstraint d _ -> lastPart d
    Column t _ -> tableName t
    OtherColumn _ t _ -> tableName t
    TableConstraint t _ -> tableName t
    Trigger t _ -> tableName t
    Policy t _ -> tableName t
    Table q -> tableName q
    View q -> tableName q
    MaterializedView q -> tableName q
    Index q -> tableName q
    Sequence q -> tableName q
    Routine _ q _ -> tableName q
    Unknown a -> case a.objectNames of
        [n] -> L.last ("" :| T.split (== '.') n)
        [_, n] -> n
        _ : t : _ -> t
        [] -> ""
  where
    tableName :: QName -> Text
    tableName q = nameText q.name
    lastPart :: TypeName -> Text
    lastPart t = L.last ("" :| T.split (== '.') (typeText t))
