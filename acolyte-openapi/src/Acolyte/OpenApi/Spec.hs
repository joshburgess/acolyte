{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- | OpenAPI 3.1 spec generation from API types.
module Acolyte.OpenApi.Spec
  ( -- * Spec generation
    OpenApiSpec (..)
  , generateSpec
    -- * Per-endpoint generation
  , EndpointToOperation (..)
  , Operation (..)
  , Parameter (..)
    -- * API walking
  , ApiToOperations (..)
    -- * Capture name reflection
  , KnownCaptureName (..)
    -- * Helpers
  , opMethodLower
  ) where

import Data.Aeson (Value (..), object, (.=), ToJSON (..))
import qualified Data.Aeson.Key as Key
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, KnownNat, symbolVal, natVal)
import Network.HTTP.Types (Method)

import Acolyte.Core.Method (KnownMethod, methodVal)
import qualified Acolyte.Core.Method as Core
import Acolyte.Core.Path (PathSegment (..))
import Acolyte.Core.Endpoint (Endpoint, NoBody)
import Acolyte.Core.Effect (Requires)
import Data.Typeable (Typeable, typeRep)
import Acolyte.Core.Wrapper
  ( Describe, Description, Named, Versioned, ApiVersion (..)
  , WithParams, QP, WithHeaders, HH
  , ServerStream, ClientStream, BidiStream, RespondsWith
  )
import Acolyte.OpenApi.Schema (Schema (..), ToSchema (..), schemaToJson)


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
  [ case opOperationId op of
      Nothing  -> []
      Just oid -> ["operationId" .= oid]
  , case opSummary op of
      Nothing -> []
      Just s  -> ["summary" .= s]
  , case opDescription op of
      Nothing -> []
      Just d  -> ["description" .= d]
  , [ "responses" .= object
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
  , opOperationId    :: !(Maybe Text)
  , opSummary        :: !(Maybe Text)
  , opDescription    :: !(Maybe Text)
  , opParameters     :: ![Parameter]
  , opStatusCode     :: !Int
  , opResponseSchema :: !(Maybe Schema)
  , opRequestBody    :: !(Maybe Schema)
  } deriving (Show)

-- | Return the operation's HTTP method in lowercase (e.g. @"get"@, @"post"@).
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

-- | Convert an endpoint type into an OpenAPI 'Operation'.
class EndpointToOperation endpoint where
  toOperation :: Operation

-- GET (no request body)
instance (KnownMethod m, ReflectPathOA path, m ~ 'Core.GET, ToSchema resp)
  => EndpointToOperation (Endpoint m path NoBody resp) where
    toOperation = Operation
      { opMethod         = T.pack (show (methodVal @m))
      , opPath           = reflectOAPath @path
      , opOperationId    = Nothing
      , opSummary        = Nothing
      , opDescription    = Nothing
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Just (toSchema @resp)
      , opRequestBody    = Nothing
      }

-- DELETE (no request body)
instance (ReflectPathOA path, ToSchema resp)
  => EndpointToOperation (Endpoint 'Core.DELETE path NoBody resp) where
    toOperation = Operation
      { opMethod         = "DELETE"
      , opPath           = reflectOAPath @path
      , opOperationId    = Nothing
      , opSummary        = Nothing
      , opDescription    = Nothing
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Just (toSchema @resp)
      , opRequestBody    = Nothing
      }

-- POST (with request body)
instance (ReflectPathOA path, ToSchema req, ToSchema resp)
  => EndpointToOperation (Endpoint 'Core.POST path req resp) where
    toOperation = Operation
      { opMethod         = "POST"
      , opPath           = reflectOAPath @path
      , opOperationId    = Nothing
      , opSummary        = Nothing
      , opDescription    = Nothing
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 201
      , opResponseSchema = Just (toSchema @resp)
      , opRequestBody    = Just (toSchema @req)
      }

-- PUT (with request body)
instance (ReflectPathOA path, ToSchema req, ToSchema resp)
  => EndpointToOperation (Endpoint 'Core.PUT path req resp) where
    toOperation = Operation
      { opMethod         = "PUT"
      , opPath           = reflectOAPath @path
      , opOperationId    = Nothing
      , opSummary        = Nothing
      , opDescription    = Nothing
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Just (toSchema @resp)
      , opRequestBody    = Just (toSchema @req)
      }

-- PATCH (with request body)
instance (ReflectPathOA path, ToSchema req, ToSchema resp)
  => EndpointToOperation (Endpoint 'Core.PATCH path req resp) where
    toOperation = Operation
      { opMethod         = "PATCH"
      , opPath           = reflectOAPath @path
      , opOperationId    = Nothing
      , opSummary        = Nothing
      , opDescription    = Nothing
      , opParameters     = reflectOAParams @path
      , opStatusCode     = 200
      , opResponseSchema = Just (toSchema @resp)
      , opRequestBody    = Just (toSchema @req)
      }

-- Requires delegates
instance EndpointToOperation inner
  => EndpointToOperation (Requires e inner) where
    toOperation = toOperation @inner

-- Describe extracts the endpoint summary from the type-level description
instance (KnownSymbol desc, EndpointToOperation inner)
  => EndpointToOperation (Describe desc inner) where
    toOperation =
      let op = toOperation @inner
      in op { opSummary = Just (T.pack (symbolVal (Proxy @desc))) }

-- Description sets the operation description (longer text, distinct from summary)
instance (KnownSymbol desc, EndpointToOperation inner)
  => EndpointToOperation (Description desc inner) where
    toOperation =
      let op = toOperation @inner
      in op { opDescription = Just (T.pack (symbolVal (Proxy @desc))) }

-- WithParams reflects query parameters into the operation
instance (ReflectParams ps, EndpointToOperation inner)
  => EndpointToOperation (WithParams ps inner) where
    toOperation =
      let op = toOperation @inner
      in op { opParameters = opParameters op ++ reflectParams @ps }

-- WithHeaders reflects header parameters into the operation
instance (ReflectHeaders hs, EndpointToOperation inner)
  => EndpointToOperation (WithHeaders hs inner) where
    toOperation =
      let op = toOperation @inner
      in op { opParameters = opParameters op ++ reflectHeaders @hs }

-- ServerStream delegates (streaming marker)
instance EndpointToOperation inner
  => EndpointToOperation (ServerStream inner) where
    toOperation = toOperation @inner

-- ClientStream delegates (streaming marker)
instance EndpointToOperation inner
  => EndpointToOperation (ClientStream inner) where
    toOperation = toOperation @inner

-- BidiStream delegates (streaming marker)
instance EndpointToOperation inner
  => EndpointToOperation (BidiStream inner) where
    toOperation = toOperation @inner

-- RespondsWith extracts the status code from the type-level Nat
instance (KnownNat s, EndpointToOperation inner)
  => EndpointToOperation (RespondsWith s inner) where
    toOperation =
      let op = toOperation @inner
      in op { opStatusCode = fromIntegral (natVal (Proxy @s)) }

-- Named sets operationId from the type-level name
instance (KnownSymbol name, EndpointToOperation inner)
  => EndpointToOperation (Named name inner) where
    toOperation =
      let op = toOperation @inner
      in op { opOperationId = Just (T.pack (symbolVal (Proxy @name))) }

-- Versioned prepends the version prefix to the path
instance (ApiVersion v, EndpointToOperation inner)
  => EndpointToOperation (Versioned v inner) where
    toOperation =
      let op = toOperation @inner
      in op { opPath = "/" <> versionPrefix @v <> opPath op }


-- ===================================================================
-- Query parameter reflection
-- ===================================================================

-- | Reflect type-level query parameter declarations into runtime Parameter values.
class ReflectParams (ps :: [Type]) where
  reflectParams :: [Parameter]

instance ReflectParams '[] where
  reflectParams = []

instance (KnownSymbol name, ReflectParams rest)
  => ReflectParams (QP name a ': rest) where
    reflectParams = Parameter
      { paramName     = T.pack (symbolVal (Proxy @name))
      , paramIn       = "query"
      , paramRequired = False
      , paramSchema   = Schema "string" [] Nothing Nothing Nothing Nothing False
      } : reflectParams @rest


-- ===================================================================
-- Header parameter reflection
-- ===================================================================

-- | Reflect type-level header declarations into runtime Parameter values.
class ReflectHeaders (hs :: [Type]) where
  reflectHeaders :: [Parameter]

instance ReflectHeaders '[] where
  reflectHeaders = []

instance (KnownSymbol name, ReflectHeaders rest)
  => ReflectHeaders (HH name a ': rest) where
    reflectHeaders = Parameter
      { paramName     = T.pack (symbolVal (Proxy @name))
      , paramIn       = "header"
      , paramRequired = True
      , paramSchema   = Schema "string" [] Nothing Nothing Nothing Nothing False
      } : reflectHeaders @rest


-- ===================================================================
-- Capture name reflection
-- ===================================================================

-- | Derive a parameter name for a capture type.
-- Override this for custom names; the default lowercases the type name.
class KnownCaptureName (t :: Type) where
  captureName :: Text

instance {-# OVERLAPPABLE #-} Typeable t => KnownCaptureName t where
  captureName = T.toLower (T.pack (show (typeRep (Proxy @t))))


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

instance (KnownCaptureName t, ToSchema t, ReflectPathOA rest)
  => ReflectPathOA ('Capture t ': rest) where
    reflectOAPath = "/{" <> captureName @t <> "}" <> reflectOAPath @rest
    reflectOAParams = Parameter
      { paramName     = captureName @t
      , paramIn       = "path"
      , paramRequired = True
      , paramSchema   = toSchema @t
      } : reflectOAParams @rest

instance (KnownSymbol name, ToSchema t, ReflectPathOA rest)
  => ReflectPathOA ('CaptureNamed name t ': rest) where
    reflectOAPath = "/{" <> T.pack (symbolVal (Proxy @name)) <> "}" <> reflectOAPath @rest
    reflectOAParams = Parameter
      { paramName     = T.pack (symbolVal (Proxy @name))
      , paramIn       = "path"
      , paramRequired = True
      , paramSchema   = toSchema @t
      } : reflectOAParams @rest


-- ===================================================================
-- Walk an API type list
-- ===================================================================

-- | Walk a promoted list of endpoint types, collecting their 'Operation's.
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

-- | Generate a complete OpenAPI 3.1 spec from an API type, title, and version.
generateSpec
  :: forall api. ApiToOperations api
  => Text -> Text -> OpenApiSpec
generateSpec title version = OpenApiSpec
  { specTitle   = title
  , specVersion = version
  , specOps     = toOperations @api
  }
