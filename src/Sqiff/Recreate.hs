{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What has to go and come back around a column whose type changes: PostgreSQL refuses
-- to retype a column that views, or the policies, triggers and routines bound to them,
-- still use.
module Sqiff.Recreate (
    viewsAroundTypeChanges,
    withViewsAround,
    retypedIn,
    standardBody,
) where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Definition
import Sqiff.Diff (Change (..), Changes, affected)
import Sqiff.Graph (topoOrder)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (context)
import Sqiff.Recipe (Context (..), Step (..), recipe, revokeGranted)


-- | Adds to the context what to drop and create again around each retyped column, by
-- what depends on it in @schema@ through @edges@.
withViewsAround :: Schema -> [Edge] -> Changes -> Context -> Context
withViewsAround schema edges changes ctx =
    ctx{viewsAround = Map.map snd (viewsAroundTypeChanges exact schema edges (retypedIn changes))}
  where
    known = Set.fromList [k.object | k <- Map.keys schema, k.aspect == Definition]
    exact = affected known edges changes


-- | The columns whose type or collation changes, with their identities.
retypedIn :: Changes -> [(QName, Identifier, ObjectId)]
retypedIn changes =
    [ (t, c, k.object)
    | (k, Changed (ColumnDefinition old) (ColumnDefinition new)) <- Map.toList changes
    , k.aspect == Definition
    , Column t c <- [k.object]
    , old.colType /= new.colType || old.collation /= new.collation
    ]


-- | For each retyped column, the statements that drop the views, policies, triggers and
-- routines that hold it, and the ones that create them again.
viewsAroundTypeChanges :: Map ObjectId (Set ObjectId) -> Schema -> [Edge] -> [(QName, Identifier, ObjectId)] -> Map (QName, Identifier) (Set ObjectId, ([Text], [Text]))
viewsAroundTypeChanges exact schema edges retyped =
    Map.fromList
        [ ((t, c), (members, (map dropStatement (reverse order), concatMap createAgain order)))
        | (t, c, column) <- retyped
        , let closure = Map.findWithDefault Set.empty column exact
              onColumn = Set.filter (\a -> isPolicyOrTrigger a && any (\e -> e.dependent == a && e.referenced == column) edges) closure
              members = held column closure (Set.filter isView closure <> onColumn)
              order = topoOrder members [(d, r) | e <- edges, let d = relationOf e.dependent, let r = relationOf e.referenced, d `Set.member` members, r `Set.member` members]
        , not (Set.null members)
        ]
  where
    isPolicyOrTrigger = \case
        Policy{} -> True
        Trigger{} -> True
        _ -> False
    -- Adds the routines whose definitions hold the column or one of the members, until
    -- no more do.
    held column closure members =
        let views = Set.filter isView members
            holds r = case printed <$> Map.lookup (definitionKey r) schema of
                Just def
                    | standardBody def -> any (\e -> e.dependent == r && (e.referenced == column || relationOf e.referenced `Set.member` members)) edges
                    | otherwise -> any (\v -> qualifiedName v `T.isInfixOf` signature def) views
                Nothing -> False
            more = Set.filter (\r -> isRoutine r && r `Set.notMember` members && holds r) closure
         in if Set.null more then members else held column closure (members <> more)
    signature = T.unlines . takeWhile (\l -> not ("AS " `T.isPrefixOf` T.stripStart l || "BEGIN ATOMIC" `T.isInfixOf` l)) . T.lines
    ctx0 = context Map.empty Map.empty Map.empty [] Map.empty
    stepsFor key c = map (.sql) (recipe ctx0 key c)
    createAgain a =
        concat
            [ [revokeGranted (qualifiedName a) | aspect == Privileges, isView a] <> stepsFor key (Added def)
            | aspect <- [Definition, Privileges]
            , let key = ObjectKey aspect a
            , Just def <- [Map.lookup key schema]
            ]
            <> [ printed def
               | (k, def) <- Map.toList schema
               , k.aspect == Definition
               , case k.object of
                    Index _ | IndexDefinition ix <- def -> isRelation a == Just ix.table
                    Trigger t _ -> isRelation a == Just t
                    _ -> False
               ]
            <> concat
                [ stepsFor k (Added def)
                | (k, def) <- Map.toList schema
                , k.aspect == Comment
                , k.object == a || (relationOf k.object == a && k.object /= a)
                ]


-- | The relation a column belongs to; any other object is its own.
relationOf :: ObjectId -> ObjectId
relationOf = \case
    Column t _ -> Table t
    OtherColumn OfView t _ -> View t
    OtherColumn OfMaterializedView t _ -> MaterializedView t
    OtherColumn OfCompositeType t _ -> Unknown (Address "composite type" [nameText t.schema, nameText t.name] [])
    OtherColumn OfForeignTable t _ -> Unknown (Address "foreign table" [nameText t.schema, nameText t.name] [])
    object -> object


-- | An object's name as SQL writes it after its kind.
qualifiedName :: ObjectId -> Text
qualifiedName object = case (toAddress object).objectNames of
    [s, n] -> qualified (qname s n)
    names -> T.intercalate "." names


-- | The statement that drops a view, routine, policy or trigger around a type change.
dropStatement :: ObjectId -> Text
dropStatement object = case object of
    Routine _ q args ->
        let keyword = (toAddress object).objectType
            list = T.intercalate ", " (map typeText args)
         in [i|drop #{keyword} #{qualified q}(#{list})|]
    Policy t n -> [i|drop policy #{quoted n} on #{qualified t}|]
    Trigger t n -> [i|drop trigger #{quoted n} on #{qualified t}|]
    _ -> let keyword = (toAddress object).objectType in [i|drop #{keyword} #{qualifiedName object}|]


-- | A SQL-standard body, which PostgreSQL parses once and tracks in pg_depend.
standardBody :: Text -> Bool
standardBody def = "BEGIN ATOMIC" `T.isInfixOf` def || any ("RETURN " `T.isPrefixOf`) (T.lines def)
