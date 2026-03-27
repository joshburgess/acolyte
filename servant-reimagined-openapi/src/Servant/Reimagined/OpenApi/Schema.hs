{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | JSON Schema derivation for OpenAPI.
--
-- 'ToSchema' produces a JSON Schema object for a type, used in
-- request body and response schemas in the OpenAPI spec.
--
-- For record types, use @deriving Generic@ and the default implementation:
--
-- @
-- data User = User { userName :: Text, userAge :: Int }
--   deriving (Generic)
--
-- instance ToSchema User
-- -- Produces: { "type": "object", "properties": { "userName": { "type": "string" }, "userAge": { "type": "integer" } } }
-- @
module Servant.Reimagined.OpenApi.Schema
  ( -- * Schema type
    Schema (..)
  , schemaToJson
    -- * Schema class
  , ToSchema (..)
    -- * Generic derivation helper
  , GToSchema (..)
  , GToSchemaCon (..)
  , genericToSchema
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy (Proxy (..))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

import Servant.Reimagined.Core.Endpoint (Json)


-- | A simplified JSON Schema representation.
data Schema = Schema
  { schemaType       :: !Text
  , schemaProperties :: ![(Text, Schema)]
  , schemaItems      :: !(Maybe Schema)
  , schemaRef        :: !(Maybe Text)
  , schemaOneOf      :: !(Maybe [Schema])
  , schemaEnum       :: !(Maybe [Text])
  } deriving (Show, Eq)


-- | Convert a schema to an aeson Value.
schemaToJson :: Schema -> Value
schemaToJson s
  | Just ref <- schemaRef s = object ["$ref" .= ref]
  | Just variants <- schemaOneOf s = object ["oneOf" .= map schemaToJson variants]
  | Just values <- schemaEnum s = object ["type" .= schemaType s, "enum" .= values]
  | schemaType s == "array" = object $
      ["type" .= schemaType s]
      ++ maybe [] (\items -> ["items" .= schemaToJson items]) (schemaItems s)
  | schemaType s == "object" && not (null (schemaProperties s)) = object
      [ "type" .= schemaType s
      , "properties" .= object
          [ Key.fromText k .= schemaToJson v | (k, v) <- schemaProperties s ]
      ]
  | otherwise = object ["type" .= schemaType s]


-- | Derive a JSON Schema for a type.
--
-- For record types with a 'Generic' instance, the default implementation
-- uses 'genericToSchema' to produce an object schema with properties
-- derived from field names:
--
-- @
-- data User = User { userName :: Text, userAge :: Int }
--   deriving (Generic)
--
-- instance ToSchema User
-- @
class ToSchema a where
  toSchema :: Schema
  default toSchema :: (Generic a, GToSchema (Rep a)) => Schema
  toSchema = genericToSchema @a


-- | Derive a 'ToSchema' instance from a type's 'Generic' representation.
--
-- Produces an object schema with properties extracted from record
-- selectors. Field names become property keys, field types become
-- property schemas (via their own 'ToSchema' instances).
genericToSchema :: forall a. (Generic a, GToSchema (Rep a)) => Schema
genericToSchema = gToSchemaFull @(Rep a)


-- | Walk a Generic representation to extract (fieldName, schema) pairs.
class GToSchema (f :: Type -> Type) where
  gToSchema :: [(Text, Schema)]
  gToSchemaFull :: Schema
  gToSchemaFull = Schema "object" (gToSchema @f) Nothing Nothing Nothing Nothing

-- Metadata wrapper (datatype info) — delegate to inner
instance GToSchema f => GToSchema (D1 meta f) where
  gToSchema = gToSchema @f
  gToSchemaFull = gToSchemaFull @f

-- Constructor wrapper — delegate to inner
instance GToSchema f => GToSchema (C1 meta f) where
  gToSchema = gToSchema @f
  gToSchemaFull = gToSchemaFull @f

-- Product: combine left and right fields
instance (GToSchema f, GToSchema g) => GToSchema (f :*: g) where
  gToSchema = gToSchema @f ++ gToSchema @g

-- Record selector with a named field
instance (KnownSymbol name, ToSchema t)
  => GToSchema (S1 ('MetaSel ('Just name) su ss ds) (Rec0 t)) where
    gToSchema = [(T.pack (symbolVal (Proxy @name)), toSchema @t)]

-- No fields (unit constructor)
instance GToSchema U1 where
  gToSchema = []

-- Sum types — produce oneOf schema with tagged variants
instance (GToSchemaCon (f :+: g)) => GToSchema (f :+: g) where
  gToSchema = []
  gToSchemaFull = Schema "object" [] Nothing Nothing (Just (gToSchemaCon @(f :+: g))) Nothing


-- ===================================================================
-- Sum type constructor schemas
-- ===================================================================

-- | Per-constructor schema (for oneOf in sum types).
class GToSchemaCon (f :: Type -> Type) where
  gToSchemaCon :: [Schema]

instance (GToSchemaCon f, GToSchemaCon g) => GToSchemaCon (f :+: g) where
  gToSchemaCon = gToSchemaCon @f ++ gToSchemaCon @g

instance (KnownSymbol name, GToSchema f)
  => GToSchemaCon (C1 ('MetaCons name fix rec) f) where
  gToSchemaCon =
    let props = gToSchema @f
        tagProp = ("tag", Schema "string" [] Nothing Nothing Nothing
                    (Just [T.pack (symbolVal (Proxy @name))]))
    in [Schema "object" (tagProp : props) Nothing Nothing Nothing Nothing]


-- ===================================================================
-- Primitive instances
-- ===================================================================

instance ToSchema Int where
  toSchema = Schema "integer" [] Nothing Nothing Nothing Nothing

instance ToSchema Integer where
  toSchema = Schema "integer" [] Nothing Nothing Nothing Nothing

instance ToSchema Double where
  toSchema = Schema "number" [] Nothing Nothing Nothing Nothing

instance ToSchema Float where
  toSchema = Schema "number" [] Nothing Nothing Nothing Nothing

instance ToSchema Bool where
  toSchema = Schema "boolean" [] Nothing Nothing Nothing Nothing

instance ToSchema Text where
  toSchema = Schema "string" [] Nothing Nothing Nothing Nothing

instance ToSchema String where
  toSchema = Schema "string" [] Nothing Nothing Nothing Nothing

instance ToSchema () where
  toSchema = Schema "object" [] Nothing Nothing Nothing Nothing

instance ToSchema a => ToSchema [a] where
  toSchema = Schema "array" [] (Just (toSchema @a)) Nothing Nothing Nothing

instance ToSchema a => ToSchema (Maybe a) where
  toSchema = toSchema @a  -- nullable in OpenAPI terms

instance (ToSchema a, ToSchema b) => ToSchema (Either a b) where
  toSchema = Schema "object" [] Nothing Nothing (Just [toSchema @a, toSchema @b]) Nothing

instance ToSchema a => ToSchema (Json a) where
  toSchema = toSchema @a
