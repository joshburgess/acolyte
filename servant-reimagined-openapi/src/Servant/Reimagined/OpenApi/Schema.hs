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
  } deriving (Show, Eq)


-- | Convert a schema to an aeson Value.
schemaToJson :: Schema -> Value
schemaToJson s
  | Just ref <- schemaRef s = object ["$ref" .= ref]
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
genericToSchema = Schema "object" (gToSchema @(Rep a)) Nothing Nothing


-- | Walk a Generic representation to extract (fieldName, schema) pairs.
class GToSchema (f :: Type -> Type) where
  gToSchema :: [(Text, Schema)]

-- Metadata wrapper (datatype info) — delegate to inner
instance GToSchema f => GToSchema (D1 meta f) where
  gToSchema = gToSchema @f

-- Constructor wrapper — delegate to inner
instance GToSchema f => GToSchema (C1 meta f) where
  gToSchema = gToSchema @f

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

-- Sum types — treat as object with no properties (fallback)
instance (GToSchema f, GToSchema g) => GToSchema (f :+: g) where
  gToSchema = []


-- ===================================================================
-- Primitive instances
-- ===================================================================

instance ToSchema Int where
  toSchema = Schema "integer" [] Nothing Nothing

instance ToSchema Integer where
  toSchema = Schema "integer" [] Nothing Nothing

instance ToSchema Double where
  toSchema = Schema "number" [] Nothing Nothing

instance ToSchema Float where
  toSchema = Schema "number" [] Nothing Nothing

instance ToSchema Bool where
  toSchema = Schema "boolean" [] Nothing Nothing

instance ToSchema Text where
  toSchema = Schema "string" [] Nothing Nothing

instance ToSchema String where
  toSchema = Schema "string" [] Nothing Nothing

instance ToSchema () where
  toSchema = Schema "object" [] Nothing Nothing

instance ToSchema a => ToSchema [a] where
  toSchema = Schema "array" [] (Just (toSchema @a)) Nothing

instance ToSchema a => ToSchema (Maybe a) where
  toSchema = toSchema @a  -- nullable in OpenAPI terms

instance ToSchema a => ToSchema (Json a) where
  toSchema = toSchema @a
