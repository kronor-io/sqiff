{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Recreates functions with SQL-standard bodies from their printed text in rolled-back
-- transactions, to see whether that text parses and prints back the same.
module Sqiff.Body (
    BodyCheck (..),
    checkBodies,
    sameDefinition,
) where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TH (singletonStatement, vectorStatement)
import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.Vector qualified as V
import Sqiff.Catalog (errorText, failWith, qualifiedNames)
import Sqiff.Model (Address (..), Aspect (..), ObjectKey (..), fromAddress, isRoutine, toAddress)


data BodyCheck
    = -- | Recreating from the printed text prints it back unchanged.
      RoundTrips
    | -- | It parses, but prints back as this other text.
      Reprints Text
    | -- | It does not parse; this repaired text does, and prints back the original.
      Repaired Text
    | -- | Neither the printed text nor a repair parses.
      DoesNotParse Text
    deriving stock (Eq, Show)


-- | Checks every function in @schema@ that has a SQL-standard body; needs a database
-- that accepts writes.
checkBodies :: Connection.Connection -> Map ObjectKey Text -> IO (Map ObjectKey BodyCheck)
checkBodies conn schema = qualifiedNames conn do
    sqlBodyFunctions <-
        if any (isRoutine . (.object)) (Map.keys schema)
            then toList <$> run sqlBodies ()
            else pure []
    let keys = Set.fromList [definitionKey (toList names) (toList args) kind | (names, args, kind) <- sqlBodyFunctions]
    Map.traverseWithKey roundTrip (Map.restrictKeys schema keys)
  where
    definitionKey names args kind = ObjectKey Definition (fromAddress (Address kind names args))

    -- Runs the printed definition and compares it with what PostgreSQL prints back.
    roundTrip k original =
        rolledBack (recreate original *> printDefinition k) >>= \case
            Right reprinted
                | reprinted == original -> pure RoundTrips
                | otherwise -> pure (Reprints reprinted)
            Left parseError -> case repair original of
                Nothing -> pure (DoesNotParse parseError)
                Just repaired -> do
                    fromRepaired <- rolledBack (recreate repaired *> printDefinition k)
                    pure if fromRepaired == Right original then Repaired repaired else DoesNotParse parseError

    run :: Statement.Statement p a -> p -> IO a
    run statement params = Connection.use conn (Session.statement params statement) >>= either (\e -> failWith [i|query failed: #{errorText e}|]) pure

    recreate :: Text -> Session.Session ()
    recreate definition = Session.script (definition <> ";")

    printDefinition :: ObjectKey -> Session.Session Text
    printDefinition k = let addr = toAddress k.object in Session.statement (V.fromList addr.objectNames, V.fromList addr.objectArgs) printedSql

    rolledBack :: Session.Session a -> IO (Either Text a)
    rolledBack session =
        Connection.use conn (Session.script "begin" *> session <* Session.script "rollback") >>= \case
            Right a -> pure (Right a)
            Left err -> do
                void (Connection.use conn (Session.script "rollback"))
                pure . Left . T.take 300 . T.unwords . T.words . errorText $ err

    printedSql :: Statement.Statement (Vector Text, Vector Text) Text
    printedSql =
        [singletonStatement|
            select pg_get_functiondef(to_regprocedure(format('%I.%I(%s)', ($1::text[])[1], ($1::text[])[2], array_to_string($2::text[], ', '))))::text
        |]

    sqlBodies :: Statement.Statement () (Vector (Vector Text, Vector Text, Text))
    sqlBodies =
        [vectorStatement|
            select
                a.object_names::text[]
              , a.object_args::text[]
              , a.type::text
            from pg_proc p
            join pg_namespace n
              on n.oid = p.pronamespace
            cross join lateral pg_identify_object_as_address('pg_proc'::regclass, p.oid, 0) a
            where p.prosqlbody is not null
              and n.nspname <> 'information_schema'
              and n.nspname !~ '^pg_'
        |]


-- | PostgreSQL can print an @ON CONFLICT@ reference to @EXCLUDED@ as @excluded_1@, which
-- does not parse back.
repair :: Text -> Maybe Text
repair def =
    let fixed = foldl' (\t n -> T.replace [i|excluded_#{n}.|] "excluded." t) def [1 :: Int .. 9]
     in if fixed == def then Nothing else Just fixed


-- | Two definitions are the same object when the texts match, or when the desired one
-- reprints as the current one: that is what recreating it from its printed text gives.
sameDefinition :: Map ObjectKey BodyCheck -> ObjectKey -> Text -> Text -> Bool
sameDefinition checks k current desired =
    current == desired || case Map.lookup k checks of
        Just (Reprints p) -> current == p
        _ -> False
