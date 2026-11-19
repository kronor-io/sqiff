{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Catalog ids and the encoders and decoders the catalog queries share.
module Sqiff.Catalog.Rows (
    ClassOid (..),
    pgClass,
    pgProc,
    CatalogId (..),
    excludedParam,
    catalogIdsParams,
    catalogIdRow,
    int8Column,
    int8Array,
    int4Array,
    objectRow,
    textColumn,
    textArrayColumn,
) where

import Data.Functor.Contravariant (contramap)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import RIO
import Sqiff.Model (Address (..), ObjectId, fromAddress)


-- | The oid of a system catalog, such as @pg_class@.
newtype ClassOid = ClassOid Int64
    deriving newtype (Eq, Ord, Show, NFData)


pgClass, pgProc :: ClassOid
pgClass = ClassOid 1259
pgProc = ClassOid 1255


-- | An object's class, object and sub-object id, as in @pg_depend@.
data CatalogId = CatalogId
    { classId :: ClassOid
    , objectOid :: Int64
    , subId :: Int32
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)


excludedParam :: E.Params [Text]
excludedParam = E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text)))


-- | Catalog ids as three parallel arrays: classes, objects and sub-objects.
catalogIdsParams :: E.Params [CatalogId]
catalogIdsParams =
    contramap (map (\c -> let ClassOid oid = c.classId in oid)) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int8))))
        <> contramap (map (.objectOid)) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int8))))
        <> contramap (map (.subId)) (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.int4))))


catalogIdRow :: D.Row CatalogId
catalogIdRow = CatalogId <$> (ClassOid <$> int8Column) <*> int8Column <*> D.column (D.nonNullable D.int4)


int8Column :: D.Row Int64
int8Column = D.column (D.nonNullable D.int8)


int8Array :: D.Row [Int64]
int8Array = D.column (D.nonNullable (D.listArray (D.nonNullable D.int8)))


int4Array :: D.Row [Int32]
int4Array = D.column (D.nonNullable (D.listArray (D.nonNullable D.int4)))


-- | An object as @pg_identify_object_as_address@ names it, parsed.
objectRow :: D.Row ObjectId
objectRow = fmap fromAddress (Address <$> textColumn <*> textArrayColumn <*> textArrayColumn)


textColumn :: D.Row Text
textColumn = D.column (D.nonNullable D.text)


textArrayColumn :: D.Row [Text]
textArrayColumn = D.column (D.nonNullable (D.listArray (D.nonNullable D.text)))
