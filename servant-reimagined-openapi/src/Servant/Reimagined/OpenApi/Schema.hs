-- | JSON Schema derivation for OpenAPI.
--
-- 'ToSchema' produces a JSON Schema object for a type, used in
-- request body and response schemas in the OpenAPI spec.
module Servant.Reimagined.OpenApi.Schema
  ( -- * Schema type
    Schema (..)
    -- * Schema class
  , ToSchema (..)
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T


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
      -- Note: T.unpack because aeson expects String keys in older versions.
      -- In aeson 2.x, Key is used — we use .= which handles this.
      ]
  | otherwise = object ["type" .= schemaType s]


-- | Derive a JSON Schema for a type.
class ToSchema a where
  toSchema :: Schema

-- Primitive instances
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
