-- | Build a gRPC server from a acolyte API type.
--
-- The same API type that drives REST routing also drives gRPC:
--
-- @
-- type API = '[ Get HealthPath Text
--             , Post UsersPath (Json CreateUser) (Json User)
--             ]
--
-- -- REST server (existing):
-- restServer = mkServer @API restHandlers
--
-- -- gRPC server (new):
-- grpcSvc = grpcServer (mkGrpcServiceMap @API "mypackage" "MyService" grpcHandlers)
-- @
module Acolyte.Grpc.Server
  ( -- * Service map construction
    mkGrpcServiceMap
    -- * Handler types
  , GrpcHandlerFn
    -- * Type class for building
  , BuildGrpcHandlers (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy (..))

import Acolyte.Core.Method (KnownMethod, methodVal, Method(..))
import Acolyte.Core.Path (PathSegment(..))
import Acolyte.Core.Endpoint (Endpoint, NoBody)
import Acolyte.Core.Effect (Requires)
import Acolyte.Core.Wrapper
  ( Describe, WithParams, WithHeaders
  , ServerStream, ClientStream, BidiStream, RespondsWith
  )

import Spire.Grpc (GrpcServiceMap, GrpcHandler(..), GrpcRequest(..), GrpcResponse(..), grpcServiceMap, grpcOk, grpcInternal, unaryHandler)
import Spire.Grpc.Codec (encodeMessage, decodeMessage, GrpcMessage(..))

import Acolyte.Grpc.Proto (GrpcCodec(..))


-- | A gRPC handler function: takes raw request bytes, returns raw response bytes.
type GrpcHandlerFn = ByteString -> IO (Either Text ByteString)


-- | Build a gRPC service map from an API type.
--
-- @
-- let services = mkGrpcServiceMap @MyAPI "pkg" "MySvc"
--       (handler1, handler2, handler3)
-- @
--
-- Each handler in the tuple is a 'GrpcHandlerFn'. The API type
-- determines method names from endpoint paths.
mkGrpcServiceMap
  :: forall api handlers
   . BuildGrpcHandlers api handlers
  => Text     -- ^ package name (e.g. "myapp")
  -> Text     -- ^ service name (e.g. "UserService")
  -> handlers -- ^ tuple of GrpcHandlerFn
  -> GrpcServiceMap
mkGrpcServiceMap pkg svc handlers =
  let serviceName = pkg <> "." <> svc
      entries = buildGrpcHandlers @api handlers
  in grpcServiceMap
    [ (serviceName, methodName, mkHandler fn)
    | (methodName, fn) <- entries
    ]
  where
    mkHandler fn = unaryHandler $ \reqBytes -> do
      result <- fn reqBytes
      case result of
        Right resp -> pure (Right resp)
        Left err   -> pure (Left (grpcInternal err))


-- | Type class to walk the API and extract method names + handlers.
class BuildGrpcHandlers (api :: [Type]) handlers where
  buildGrpcHandlers :: handlers -> [(Text, GrpcHandlerFn)]


-- Arity 1
instance EndpointMethodName e => BuildGrpcHandlers '[e] GrpcHandlerFn where
  buildGrpcHandlers h = [(endpointMethodName @e, h)]

-- Arity 2
instance (EndpointMethodName e1, EndpointMethodName e2)
  => BuildGrpcHandlers '[e1, e2] (GrpcHandlerFn, GrpcHandlerFn) where
  buildGrpcHandlers (h1, h2) =
    [ (endpointMethodName @e1, h1)
    , (endpointMethodName @e2, h2)
    ]

-- Arity 3
instance (EndpointMethodName e1, EndpointMethodName e2, EndpointMethodName e3)
  => BuildGrpcHandlers '[e1, e2, e3] (GrpcHandlerFn, GrpcHandlerFn, GrpcHandlerFn) where
  buildGrpcHandlers (h1, h2, h3) =
    [ (endpointMethodName @e1, h1)
    , (endpointMethodName @e2, h2)
    , (endpointMethodName @e3, h3)
    ]

-- Arity 4
instance (EndpointMethodName e1, EndpointMethodName e2, EndpointMethodName e3, EndpointMethodName e4)
  => BuildGrpcHandlers '[e1, e2, e3, e4] (GrpcHandlerFn, GrpcHandlerFn, GrpcHandlerFn, GrpcHandlerFn) where
  buildGrpcHandlers (h1, h2, h3, h4) =
    [ (endpointMethodName @e1, h1), (endpointMethodName @e2, h2)
    , (endpointMethodName @e3, h3), (endpointMethodName @e4, h4)
    ]

-- Arity 5
instance (EndpointMethodName e1, EndpointMethodName e2, EndpointMethodName e3, EndpointMethodName e4, EndpointMethodName e5)
  => BuildGrpcHandlers '[e1, e2, e3, e4, e5] (GrpcHandlerFn, GrpcHandlerFn, GrpcHandlerFn, GrpcHandlerFn, GrpcHandlerFn) where
  buildGrpcHandlers (h1, h2, h3, h4, h5) =
    [ (endpointMethodName @e1, h1), (endpointMethodName @e2, h2)
    , (endpointMethodName @e3, h3), (endpointMethodName @e4, h4)
    , (endpointMethodName @e5, h5)
    ]


-- | Extract a gRPC method name from an endpoint type.
--
-- Uses the first path literal as the method name.
-- E.g., @Get '[ 'Lit "health" ] Text@ -> "Health"
-- E.g., @Post '[ 'Lit "users" ] req resp@ -> "Users"
class EndpointMethodName (e :: Type) where
  endpointMethodName :: Text

instance EndpointMethodName inner => EndpointMethodName (Requires e inner) where
  endpointMethodName = endpointMethodName @inner

instance EndpointMethodName inner => EndpointMethodName (Describe desc inner) where
  endpointMethodName = endpointMethodName @inner

instance EndpointMethodName inner => EndpointMethodName (WithParams ps inner) where
  endpointMethodName = endpointMethodName @inner

instance EndpointMethodName inner => EndpointMethodName (WithHeaders hs inner) where
  endpointMethodName = endpointMethodName @inner

instance EndpointMethodName inner => EndpointMethodName (ServerStream inner) where
  endpointMethodName = endpointMethodName @inner

instance EndpointMethodName inner => EndpointMethodName (ClientStream inner) where
  endpointMethodName = endpointMethodName @inner

instance EndpointMethodName inner => EndpointMethodName (BidiStream inner) where
  endpointMethodName = endpointMethodName @inner

instance EndpointMethodName inner => EndpointMethodName (RespondsWith s inner) where
  endpointMethodName = endpointMethodName @inner

instance PathToMethodName path => EndpointMethodName (Endpoint m path req resp) where
  endpointMethodName = pathToMethodName @path


-- | Convert a type-level path to a PascalCase gRPC method name.
class PathToMethodName (path :: [PathSegment]) where
  pathToMethodName :: Text

instance PathToMethodName '[] where
  pathToMethodName = "Unknown"

instance (KnownSymbol s, PathToMethodName rest) => PathToMethodName ('Lit s ': rest) where
  pathToMethodName = capitalize (T.pack (symbolVal (Proxy @s)))

instance PathToMethodName rest => PathToMethodName ('Capture t ': rest) where
  pathToMethodName = pathToMethodName @rest


-- | Capitalize the first letter of a text value.
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise             = c
