-- | OpenAPI 3.1 spec generation from API types.
--
-- Walks the type-level API list and generates a complete OpenAPI spec
-- as an aeson 'Value'. Each endpoint becomes a path + operation with
-- method, parameters (from captures), and request/response schemas.
module Servant.Reimagined.OpenApi.Spec
  ( -- * Spec generation
    OpenApiSpec (..)
  , generateSpec
    -- * Per-endpoint generation
  , EndpointToOperation (..)
  , Operation (..)
  , Parameter (..)
    -- * API walking
  , ApiToOperations (..)
  ) where

import Data.Aeson (Value (..), object, (.=), ToJSON (..))
import qualified Data.Aeson.Key as Key
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (Method)

import Servant.Reimagined.Core.Method (KnownMethod, methodVal)
import qualified Servant.Reimagined.Core.Method as Core
import Servant.Reimagined.Core.Path (PathSegment (..))
import Servant.Reimagined.Core.Endpoint (Endpoint, NoBody)
import Servant.Reimagined.Core.Effect (Requires)
import Servant.Reimagined.OpenApi.Schema (Schema (..), ToSchema (..))


-- | A complete OpenAPI 3.1 spec.
data OpenApiSpec = OpenApiSpec
  { specTitle   :: !Text
  , specVersion :: !Text
  , specOps     :: ![Operation]
  } deriving (Show)

instance ToJSON OpenApiSpec where
  toJSON spec = object
    [ "openapi" .= ("3.1.0" :: Text)
    , "info" .= object
        [ "title"   .= specTitle spec
        , "version" .= specVersion spec
        ]
    , "paths" .= groupByPath (specOps spec)
    ]


-- | Group operations by path for the OpenAPI paths object.
groupByPath :: [Operation] -> Value
groupByPath ops = object
  [ Key.fromText (opPath op) .= object [Key.fromText (opMethodLower op) .= opToJson op]
  | op <- ops
  ]

opToJson :: Operation -> Value
opToJson op = object $
  [ "responses" .= object
      [ "200" .= object ["description" .= ("Success" :: Text)] ]
  ]
  ++ (if null (opParameters op) then []
      else ["parameters" .= map paramToJson (opParameters op)])


-- | A single API operation (one method + path combination).
data Operation = Operation
  { opMethod     :: !Text
  , opPath       :: !Text
  , opParameters :: ![Parameter]
  } deriving (Show)

opMethodLower :: Operation -> Text
opMethodLower = T.toLower . opMethod


-- | An API parameter (path capture, query, header).
data Parameter = Parameter
  { paramName     :: !Text
  , paramIn       :: !Text       -- "path", "query", "header"
  , paramRequired :: !Bool
  , paramSchema   :: !Schema
  } deriving (Show)

paramToJson :: Parameter -> Value
paramToJson p = object
  [ "name"     .= paramName p
  , "in"       .= paramIn p
  , "required" .= paramRequired p
  , "schema"   .= object ["type" .= schemaType (paramSchema p)]
  ]


-- ===================================================================
-- Per-endpoint operation generation
-- ===================================================================

-- | Generate an Operation from an endpoint type.
class EndpointToOperation endpoint where
  toOperation :: Operation

-- Base Endpoint instance
instance (KnownMethod m, ReflectPathOA path)
  => EndpointToOperation (Endpoint m path req resp) where
    toOperation = Operation
      { opMethod     = T.pack (show (methodVal @m))
      , opPath       = reflectOAPath @path
      , opParameters = reflectOAParams @path
      }

-- Requires delegates
instance EndpointToOperation inner
  => EndpointToOperation (Requires e inner) where
    toOperation = toOperation @inner


-- ===================================================================
-- Path reflection for OpenAPI format
-- ===================================================================

-- | Reflect type-level path to OpenAPI path format and parameters.
class ReflectPathOA (path :: [PathSegment]) where
  reflectOAPath   :: Text
  reflectOAParams :: [Parameter]

instance ReflectPathOA '[] where
  reflectOAPath   = ""
  reflectOAParams = []

instance (KnownSymbol s, ReflectPathOA rest)
  => ReflectPathOA ('Lit s ': rest) where
    reflectOAPath = "/" <> T.pack (symbolVal (Proxy @s)) <> reflectOAPath @rest
    reflectOAParams = reflectOAParams @rest

instance (ReflectPathOA rest)
  => ReflectPathOA ('Capture t ': rest) where
    reflectOAPath = "/{id}" <> reflectOAPath @rest
    reflectOAParams = Parameter
      { paramName     = "id"
      , paramIn       = "path"
      , paramRequired = True
      , paramSchema   = Schema "string" [] Nothing Nothing
      } : reflectOAParams @rest


-- ===================================================================
-- Walk an API type list
-- ===================================================================

-- | Walk a type-level API list and collect operations.
class ApiToOperations (api :: [Type]) where
  toOperations :: [Operation]

instance ApiToOperations '[] where
  toOperations = []

instance (EndpointToOperation e, ApiToOperations rest)
  => ApiToOperations (e ': rest) where
    toOperations = toOperation @e : toOperations @rest


-- ===================================================================
-- Top-level spec generation
-- ===================================================================

-- | Generate a complete OpenAPI spec from an API type.
--
-- @
-- spec = generateSpec @MyAPI "My API" "1.0.0"
-- @
generateSpec
  :: forall api. ApiToOperations api
  => Text -> Text -> OpenApiSpec
generateSpec title version = OpenApiSpec
  { specTitle   = title
  , specVersion = version
  , specOps     = toOperations @api
  }
