-- | OpenAPI 3.1 spec generation from API types.
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
    -- * Helpers
  , opMethodLower
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
import Servant.Reimagined.OpenApi.Schema (Schema (..), ToSchema (..), schemaToJson)


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


groupByPath :: [Operation] -> Value
groupByPath ops = object
  [ Key.fromText (opPath op) .= object [Key.fromText (opMethodLower op) .= opToJson op]
  | op <- ops
  ]


opToJson :: Operation -> Value
opToJson op = object $ concat
  [ [ "responses" .= object
        [ Key.fromText (T.pack (show (opStatusCode op))) .= responseObj ]
    ]
  , if null (opParameters op) then []
    else ["parameters" .= map paramToJson (opParameters op)]
  , case opRequestBody op of
      Nothing -> []
      Just schema ->
        [ "requestBody" .= object
            [ "required" .= True
            , "content" .= object
                [ "application/json" .= object
                    [ "schema" .= schemaToJson schema ]
                ]
            ]
        ]
  ]
  where
    responseObj = case opResponseSchema op of
      Nothing -> object ["description" .= ("Success" :: Text)]
      Just schema -> object
        [ "description" .= ("Success" :: Text)
        , "content" .= object
            [ "application/json" .= object
                [ "schema" .= schemaToJson schema ]
            ]
        ]


-- | A single API operation.
data Operation = Operation
  { opMethod         :: !Text
  , opPath           :: !Text
  , opParameters     :: ![Parameter]
  , opStatusCode     :: !Int
  , opResponseSchema :: !(Maybe Schema)
  , opRequestBody    :: !(Maybe Schema)
  } deriving (Show)

opMethodLower :: Operation -> Text
opMethodLower = T.toLower . opMethod


-- | An API parameter.
data Parameter = Parameter
  { paramName     :: !Text
  , paramIn       :: !Text
  , paramRequired :: !Bool
  , paramSchema   :: !Schema
  } deriving (Show)

paramToJson :: Parameter -> Value
paramToJson p = object
  [ "name"     .= paramName p
  , "in"       .= paramIn p
  , "required" .= paramRequired p
  , "schema"   .= schemaToJson (paramSchema p)
  ]


-- ===================================================================
-- Per-endpoint operation generation
-- ===================================================================

class EndpointToOperation endpoint where
  toOperation :: Operation

-- GET/DELETE/HEAD (no request body)
instance (KnownMethod m, ReflectPathOA path, m ~ 'Core.GET)
  => EndpointToOperation (Endpoint m path NoBody resp) where
    toOperation = Operation
      { opMethod         = T.pack (show (methodVal @m))
      , opPath           = reflectOAPath @path
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Nothing  -- would need ToSchema resp constraint
      , opRequestBody    = Nothing
      }

instance (ReflectPathOA path)
  => EndpointToOperation (Endpoint 'Core.DELETE path NoBody resp) where
    toOperation = Operation
      { opMethod         = "DELETE"
      , opPath           = reflectOAPath @path
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Nothing
      , opRequestBody    = Nothing
      }

-- POST (with request body)
instance (ReflectPathOA path)
  => EndpointToOperation (Endpoint 'Core.POST path req resp) where
    toOperation = Operation
      { opMethod         = "POST"
      , opPath           = reflectOAPath @path
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 201
      , opResponseSchema = Nothing
      , opRequestBody    = Nothing  -- would need ToSchema req constraint
      }

-- PUT (with request body)
instance (ReflectPathOA path)
  => EndpointToOperation (Endpoint 'Core.PUT path req resp) where
    toOperation = Operation
      { opMethod         = "PUT"
      , opPath           = reflectOAPath @path
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Nothing
      , opRequestBody    = Nothing
      }

-- PATCH (with request body)
instance (ReflectPathOA path)
  => EndpointToOperation (Endpoint 'Core.PATCH path req resp) where
    toOperation = Operation
      { opMethod         = "PATCH"
      , opPath           = reflectOAPath @path
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Nothing
      , opRequestBody    = Nothing
      }

-- Requires delegates
instance EndpointToOperation inner
  => EndpointToOperation (Requires e inner) where
    toOperation = toOperation @inner


-- ===================================================================
-- Path reflection for OpenAPI format
-- ===================================================================

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

generateSpec
  :: forall api. ApiToOperations api
  => Text -> Text -> OpenApiSpec
generateSpec title version = OpenApiSpec
  { specTitle   = title
  , specVersion = version
  , specOps     = toOperations @api
  }
