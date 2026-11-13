{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Recreates functions with SQL-standard bodies from their printed text in rolled-back
-- transactions, to see whether that text parses and prints back the same.
module Kronor.SchemaDiff.Body (
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
import Kronor.SchemaDiff.Catalog (Address (..), Aspect (..), ObjectKey (..), failWith, qualifiedNames)
import RIO
import RIO.Map qualified as Map
import RIO.Vector qualified as V


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
    candidates <-
        if any (\k -> k.address.objectType `elem` ["function", "procedure"]) (Map.keys schema)
            then toList <$> run sqlBodies ()
            else pure []
    fmap Map.fromList . for [(k, def) | (names, args, kind) <- candidates, let { k = key (toList names) (toList args) kind }, Just def <- [Map.lookup k schema]] $ \(k, def) -> do
        printed <- reprint k def
        check <- case printed of
            Right p
                | p == def -> pure RoundTrips
                | otherwise -> pure (Reprints p)
            Left err -> case repair def of
                Just candidate -> do
                    again <- reprint k candidate
                    pure case again of
                        Right p | p == def -> Repaired candidate
                        _ -> DoesNotParse err
                Nothing -> pure (DoesNotParse err)
        pure (k, check)
  where
    key names args kind = ObjectKey Definition (Address kind names args)

    run :: Statement.Statement p a -> p -> IO a
    run statement params = Connection.use conn (Session.statement params statement) >>= either (\e -> failWith ("query failed: " <> tshow e)) pure

    reprint k def = do
        result <-
            Connection.use conn do
                Session.script "begin"
                Session.script (def <> ";")
                printed <-
                    Session.statement (V.fromList k.address.objectNames, V.fromList k.address.objectArgs) printedSql
                Session.script "rollback"
                pure printed
        case result of
            Right printed -> pure (Right printed)
            Left err -> do
                void (Connection.use conn (Session.script "rollback"))
                pure (Left (T.take 300 (T.unwords (T.words (T.pack (show err))))))

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
