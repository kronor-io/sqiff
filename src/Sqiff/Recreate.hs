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
    withViewsReplaced,
    retypedIn,
    standardBody,
) where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Definition
import Sqiff.Dependencies (Dependencies, dependsOn, edges, noDependencies, reachableFrom, referencesOf)
import Sqiff.Diff (Change (..), Changes, affected)
import Sqiff.Graph (topoOrder)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Plan (context)
import Sqiff.Recipe (Context (..), Step (..), recipe, revokeGranted)


-- | Adds to the context what to drop and create again around each retyped column, by
-- what depends on it in @schema@ by @deps@.
withViewsAround :: Schema -> Dependencies -> Changes -> Context -> Context
withViewsAround schema deps changes ctx =
    ctx{viewsAround = Map.map snd (viewsAroundTypeChanges exact schema deps (retypedIn changes))}
  where
    known = Set.fromList [k.object | k <- Map.keys schema, k.aspect == Definition]
    exact = affected known deps changes


-- | Adds to the context what to drop and create again around each view whose columns
-- change, by what depends on it in the database being changed (@deps@), as the desired
-- one (@toSchema@) has it.
withViewsReplaced :: Schema -> Dependencies -> Changes -> Context -> Context
withViewsReplaced toSchema deps changes ctx =
    ctx{viewDependents = viewsToRecreate toSchema deps changes}


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
viewsAroundTypeChanges :: Map ObjectId (Set ObjectId) -> Schema -> Dependencies -> [(QName, Identifier, ObjectId)] -> Map (QName, Identifier) (Set ObjectId, ([Text], [Text]))
viewsAroundTypeChanges exact schema deps retyped =
    Map.fromList
        [ ((t, c), (members, (map dropStatement (reverse order), concatMap (createAgain schema) order)))
        | (t, c, column) <- retyped
        , let closure = Map.findWithDefault Set.empty column exact
              onColumn = Set.filter (\a -> isPolicyOrTrigger a && dependsOn deps a column) closure
              members = held schema deps column closure (Set.filter isView closure <> onColumn)
              order = inDependencyOrder deps members
        , not (Set.null members)
        ]
  where
    isPolicyOrTrigger = \case
        Policy{} -> True
        Trigger{} -> True
        _ -> False


-- | For each view whose columns change other than by new ones at the end, which
-- @create or replace view@ refuses, the statements that drop what depends on it in the
-- database being changed, and the ones that create those again as @toSchema@ has them;
-- one the plan drops is not created again.
viewsToRecreate :: Schema -> Dependencies -> Changes -> Map QName ([Text], [Text])
viewsToRecreate toSchema deps changes =
    Map.fromList
        [ (v, (map (dropWith " if exists") (reverse order), concatMap (createAgain toSchema) (filter wanted order)))
        | (k, Changed (ViewDefinition old) (ViewDefinition new)) <- Map.toList changes
        , k.aspect == Definition
        , View v <- [k.object]
        , not (extendsColumns old new)
        , -- Every dependent, changed ones included: they go before the view does.
        let closure = reachableFrom deps (Set.singleton k.object)
            order = inDependencyOrder deps (held toSchema deps k.object closure (Set.filter isView closure))
        ]
  where
    wanted a = definitionKey a `Map.member` toSchema


-- | The members in the order they can be created, by the relations they depend on.
inDependencyOrder :: Dependencies -> Set ObjectId -> [ObjectId]
inDependencyOrder deps members =
    topoOrder members [(d, r) | e <- edges deps, let d = relationOf e.dependent, let r = relationOf e.referenced, d `Set.member` members, r `Set.member` members]


-- | @members@ and the routines whose definitions hold @start@ or one of them, until no
-- more do.
held :: Schema -> Dependencies -> ObjectId -> Set ObjectId -> Set ObjectId -> Set ObjectId
held schema deps start closure members =
    let views = Set.filter isView members
        holds r = case printed <$> Map.lookup (definitionKey r) schema of
            Just def
                | standardBody def -> any (\x -> x == start || relationOf x `Set.member` members) (referencesOf deps r)
                | otherwise -> any (\v -> qualifiedName v `T.isInfixOf` signature def) views
            Nothing -> False
        more = Set.filter (\r -> isRoutine r && r `Set.notMember` members && holds r) closure
     in if Set.null more then members else held schema deps start closure (members <> more)
  where
    signature = T.unlines . takeWhile (\l -> not ("AS " `T.isPrefixOf` T.stripStart l || "BEGIN ATOMIC" `T.isInfixOf` l)) . T.lines


-- | The statements that create @a@ as @schema@ has it, with its grants, indexes, triggers
-- and comments.
createAgain :: Schema -> ObjectId -> [Text]
createAgain schema a =
    concat
        [ [revokeGranted (qualifiedName a) | aspect == Privileges, isView a] <> stepsFor key (Added def)
        | aspect <- [Definition, Privileges]
        , let key = ObjectKey aspect a
        , Just def <- [Map.lookup key schema]
        , -- A view created again starts with the default privileges.
        aspect == Definition || not (isDefault def)
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
  where
    ctx0 = context Map.empty Map.empty Map.empty noDependencies Map.empty
    stepsFor key c = map (.sql) (recipe ctx0 key c)


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
dropStatement = dropWith ""


-- | 'dropStatement' with @ifExists@ after the kind of object.
dropWith :: Text -> ObjectId -> Text
dropWith ifExists object = case object of
    Routine _ q args ->
        let list = T.intercalate ", " (map typeText args)
         in [i|drop #{keyword}#{ifExists} #{qualified q}(#{list})|]
    Policy t n -> [i|drop policy#{ifExists} #{quoted n} on #{qualified t}|]
    Trigger t n -> [i|drop trigger#{ifExists} #{quoted n} on #{qualified t}|]
    _ -> [i|drop #{keyword}#{ifExists} #{qualifiedName object}|]
  where
    keyword = (toAddress object).objectType


-- | A SQL-standard body, which PostgreSQL parses once and tracks in pg_depend.
standardBody :: Text -> Bool
standardBody def = "BEGIN ATOMIC" `T.isInfixOf` def || any ("RETURN " `T.isPrefixOf`) (T.lines def)


isDefault :: Definition -> Bool
isDefault = \case
    AclDefinition DefaultAcl -> True
    _ -> False
