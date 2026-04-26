{-# LANGUAGE OverloadedStrings #-}
-- | gRPC server reflection service.
--
-- Allows tools like @grpcurl@ to discover services at runtime without
-- requiring a @.proto@ file. Implements a simplified text-based
-- reflection protocol at the standard reflection service path.
--
-- @
-- let services = grpcServiceMap [...]
-- let infos = [ ServiceInfo "myapp.Greeter" [...] "..." ]
-- let withRefl = withReflection infos services
-- runServerH2 (defaultH2Config 50051) (grpcServer withRefl)
-- @
module Spire.Grpc.Reflection
  ( -- * Reflection service
    reflectionService
  , ServiceInfo (..)
  , MethodInfo (..)
    -- * Adding reflection to a server
  , withReflection
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Spire.Grpc.Server (GrpcServiceMap, GrpcHandler (..), GrpcRequest (..), GrpcResponse (..))
import Spire.Grpc.Status


-- | Description of a single RPC method.
data MethodInfo = MethodInfo
  { miName       :: !Text
  , miInputType  :: !Text
  , miOutputType :: !Text
  } deriving (Show, Eq)


-- | Description of a gRPC service for reflection.
data ServiceInfo = ServiceInfo
  { siFullName  :: !Text       -- ^ e.g. "myapp.UserService"
  , siMethods   :: ![MethodInfo]
  , siProtoFile :: !Text       -- ^ the .proto text (generated or provided)
  } deriving (Show, Eq)


-- | Build a 'GrpcServiceMap' containing the reflection service handlers.
reflectionService :: [ServiceInfo] -> GrpcServiceMap
reflectionService infos = reflectionHandlers infos


-- | Add reflection to an existing 'GrpcServiceMap'.
--
-- Registers the reflection service alongside your application services.
-- The reflection service itself is also listed in service discovery.
withReflection :: [ServiceInfo] -> GrpcServiceMap -> GrpcServiceMap
withReflection infos existing = Map.union (reflectionHandlers infos) existing


-- | Internal: build the handler map for the reflection service.
reflectionHandlers :: [ServiceInfo] -> GrpcServiceMap
reflectionHandlers infos =
  Map.fromList
    [ ( ("grpc.reflection.v1alpha.ServerReflection", "ServerReflectionInfo")
      , reflectionHandler infos
      )
    ]


-- | The reflection handler. Implements a simple text-based protocol:
--
-- * @"list_services"@ -> newline-separated list of service full names
-- * @"file_containing_symbol:<symbol>"@ -> .proto text for that service
-- * anything else -> NOT_FOUND
reflectionHandler :: [ServiceInfo] -> GrpcHandler
reflectionHandler infos = GrpcHandler $ \req -> do
  let payload = grpcBody req
      payloadText = TE.decodeUtf8 payload
  pure $ case parseReflectionRequest payloadText of
    ListServices ->
      let names = T.intercalate "\n" (map siFullName infos)
      in GrpcUnary (grpcOk "") (TE.encodeUtf8 names)

    FileContainingSymbol symbol ->
      case findServiceBySymbol infos symbol of
        Just info ->
          GrpcUnary (grpcOk "") (TE.encodeUtf8 (siProtoFile info))
        Nothing ->
          GrpcError (grpcNotFound ("Symbol not found: " <> symbol))

    UnknownRequest ->
      GrpcError (grpcNotFound "Unknown reflection request")


-- | Parsed reflection request types.
data ReflectionRequest
  = ListServices
  | FileContainingSymbol !Text
  | UnknownRequest


-- | Parse the text payload into a reflection request.
parseReflectionRequest :: Text -> ReflectionRequest
parseReflectionRequest txt
  | T.strip txt == "list_services" = ListServices
  | "file_containing_symbol:" `T.isPrefixOf` txt =
      let symbol = T.strip (T.drop (T.length "file_containing_symbol:") txt)
      in FileContainingSymbol symbol
  | otherwise = UnknownRequest


-- | Find a service whose full name matches the symbol, or whose methods
-- contain the symbol (as "ServiceName.MethodName").
findServiceBySymbol :: [ServiceInfo] -> Text -> Maybe ServiceInfo
findServiceBySymbol infos symbol =
  case filter (\si -> siFullName si == symbol) infos of
    (x:_) -> Just x
    []    ->
      -- Try matching as ServiceName.MethodName
      case filter (hasMethod symbol) infos of
        (x:_) -> Just x
        []    -> Nothing
  where
    hasMethod sym si =
      any (\mi -> siFullName si <> "." <> miName mi == sym) (siMethods si)
