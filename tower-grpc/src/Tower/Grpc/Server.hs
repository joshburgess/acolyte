{-# LANGUAGE OverloadedStrings #-}
-- | gRPC server dispatch layer.
--
-- Sits between the HTTP/2 transport (tower-server) and application
-- handlers. Parses incoming gRPC requests, dispatches to the right
-- method handler, and constructs gRPC responses with trailers.
--
-- @
-- import Tower.Grpc.Server
--
-- myService :: GrpcServiceMap
-- myService = fromList
--   [ ("greet.Greeter", "SayHello", unaryHandler sayHello)
--   ]
--
-- main = runServerH2 (defaultH2Config 50051) (grpcServer myService)
-- @
module Tower.Grpc.Server
  ( -- * Service map
    GrpcServiceMap
  , GrpcHandler (..)
  , grpcServiceMap
    -- * Method handlers
  , unaryHandler
  , serverStreamHandler
  , clientStreamHandler
  , bidiStreamHandler
    -- * Server construction
  , grpcServer
    -- * Request/response types
  , GrpcRequest (..)
  , GrpcResponse (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200, status415)

import Tower.Service (Service (..))
import Http.Core
import Http.Core.Body

import Tower.Grpc.Status as GS
import Tower.Grpc.Codec
import Tower.Grpc.Compression


-- | A parsed gRPC request.
data GrpcRequest = GrpcRequest
  { grpcService  :: !Text       -- ^ e.g. "greet.Greeter"
  , grpcMethod   :: !Text       -- ^ e.g. "SayHello"
  , grpcBody     :: !ByteString -- ^ first message payload (after framing)
  , grpcRawBody  :: !ByteString -- ^ raw body bytes (with length-prefixed framing)
  , grpcMetadata :: ![(CI.CI ByteString, ByteString)]  -- ^ request metadata (headers)
  } deriving (Show)


-- | A gRPC response from a handler.
data GrpcResponse
  = GrpcUnary !GrpcStatus !ByteString
    -- ^ Unary response: status + single response message payload
  | GrpcStream !GrpcStatus ![ByteString]
    -- ^ Server streaming: status + list of response message payloads
  | GrpcError !GrpcStatus
    -- ^ Error with no body (trailers-only response)
  deriving (Show)


-- | A gRPC method handler.
newtype GrpcHandler = GrpcHandler
  { runGrpcHandler :: GrpcRequest -> IO GrpcResponse
  }


-- | Map from (service, method) to handler.
type GrpcServiceMap = Map (Text, Text) GrpcHandler


-- | Build a service map from a list of (service, method, handler) triples.
grpcServiceMap :: [(Text, Text, GrpcHandler)] -> GrpcServiceMap
grpcServiceMap = Map.fromList . map (\(s, m, h) -> ((s, m), h))


-- | Create a unary handler (one request message -> one response message).
--
-- @
-- sayHello :: ByteString -> IO (Either GrpcStatus ByteString)
-- sayHello reqBytes = do
--   let name = decodeProto reqBytes  -- your deserialization
--   pure $ Right (encodeProto (HelloReply ("Hello " <> name)))
-- @
unaryHandler :: (ByteString -> IO (Either GrpcStatus ByteString)) -> GrpcHandler
unaryHandler f = GrpcHandler $ \req -> do
  result <- f (grpcBody req)
  pure $ case result of
    Right respBytes -> GrpcUnary (grpcOk "") respBytes
    Left status     -> GrpcError status


-- | Create a server-streaming handler.
--
-- @
-- listFeatures :: ByteString -> IO (Either GrpcStatus [ByteString])
-- listFeatures reqBytes = do
--   let area = decodeProto reqBytes
--   features <- queryFeatures area
--   pure $ Right (map encodeProto features)
-- @
serverStreamHandler :: (ByteString -> IO (Either GrpcStatus [ByteString])) -> GrpcHandler
serverStreamHandler f = GrpcHandler $ \req -> do
  result <- f (grpcBody req)
  pure $ case result of
    Right chunks -> GrpcStream (grpcOk "") chunks
    Left status  -> GrpcError status


-- | Create a client-streaming handler.
-- Receives multiple messages from the client, returns one response.
--
-- Note: This is a simplified (buffered) implementation that collects all
-- client messages before processing. True streaming would require a
-- different handler type with pull/push channels.
--
-- @
-- recordRoute :: [ByteString] -> IO (Either GrpcStatus ByteString)
-- recordRoute points = do
--   let count = length points
--   pure $ Right (encodeProto (RouteSummary count))
-- @
clientStreamHandler :: ([ByteString] -> IO (Either GrpcStatus ByteString)) -> GrpcHandler
clientStreamHandler f = GrpcHandler $ \req -> do
  -- The raw body contains multiple length-prefixed messages
  let msgs = decodeMessages (grpcRawBody req)
      payloads = map gmPayload msgs
  result <- f payloads
  pure $ case result of
    Right resp -> GrpcUnary (grpcOk "") resp
    Left status -> GrpcError status


-- | Create a bidirectional-streaming handler.
-- Receives multiple messages, returns multiple messages.
--
-- Note: This is a simplified (buffered) implementation that collects all
-- client messages before processing. True streaming would require a
-- different handler type with pull/push channels.
--
-- @
-- routeChat :: [ByteString] -> IO (Either GrpcStatus [ByteString])
-- routeChat notes = do
--   responses <- mapM processNote notes
--   pure $ Right responses
-- @
bidiStreamHandler :: ([ByteString] -> IO (Either GrpcStatus [ByteString])) -> GrpcHandler
bidiStreamHandler f = GrpcHandler $ \req -> do
  let msgs = decodeMessages (grpcRawBody req)
      payloads = map gmPayload msgs
  result <- f payloads
  pure $ case result of
    Right resps -> GrpcStream (grpcOk "") resps
    Left status -> GrpcError status


-- | Maximum gRPC request body size (4 MiB).
--
-- Protects against unbounded memory consumption from oversized messages.
-- Individual services can implement their own limits for finer control.
maxGrpcBodySize :: Int
maxGrpcBodySize = 4 * 1024 * 1024


-- | Build a tower Service that dispatches gRPC requests.
--
-- Parses the gRPC path, looks up the handler in the service map,
-- frames the response with gRPC trailers.
grpcServer :: GrpcServiceMap -> Service IO (Request Body) (Response Body)
grpcServer services = Service $ \req -> do
  -- Validate content-type
  let ct = lookup "content-type" (requestHeaders req)
  case ct of
    Just v | "application/grpc" `BS.isPrefixOf` v -> do
      body <- bodyToStrict (requestBody req)
      if BS.length body > maxGrpcBodySize
        then pure $ grpcTrailersOnly (GS.grpcResourceExhausted "Request body too large")
        else handleGrpc services req body
    _ ->
      pure $ Response status415 [] (fromBytes "Unsupported content type")


-- | Handle a validated gRPC request.
handleGrpc
  :: GrpcServiceMap
  -> Request Body
  -> ByteString
  -> IO (Response Body)
handleGrpc services req body = do
  let pathRaw = requestPathRaw req
      (service, method) = parseGrpcPath (TE.decodeUtf8Lenient pathRaw)
  case Map.lookup (service, method) services of
    Nothing ->
      pure $ grpcTrailersOnly (grpcUnimplemented
        ("Method not found: " <> service <> "/" <> method))
    Just handler -> do
      -- Check if client sent compressed messages
      let encoding = lookup "grpc-encoding" (requestHeaders req)
          clientCompression = case encoding of
            Just "gzip" -> Gzip
            _           -> NoCompression
      -- Decompress the body if needed, then decode the request message
      let decompressResult = case clientCompression of
            Gzip -> decompressGrpcBody body
            NoCompression -> Right body
      case decompressResult of
        Left err ->
          pure $ grpcTrailersOnly (grpcResourceExhausted err)
        Right decompressedBody -> do
          let payload = case decodeMessage decompressedBody of
                Just (msg, _) -> gmPayload msg
                Nothing       -> decompressedBody  -- fallback: raw bytes
          let grpcReq = GrpcRequest
                { grpcService  = service
                , grpcMethod   = method
                , grpcBody     = payload
                , grpcRawBody  = decompressedBody
                , grpcMetadata = requestHeaders req
                }
          resp <- runGrpcHandler handler grpcReq
          pure $ grpcResponseToHttp resp


-- | Parse a gRPC path like "/pkg.Service/Method" into (service, method).
parseGrpcPath :: Text -> (Text, Text)
parseGrpcPath path =
  let stripped = T.dropWhile (== '/') path
  in case T.breakOn "/" stripped of
    (service, rest) -> (service, T.drop 1 rest)


-- | Convert a GrpcResponse to an HTTP Response with proper trailers.
--
-- NOTE: gRPC spec requires grpc-status in HTTP/2 trailers.
-- In our HTTP/1.1 path, trailers are sent as trailing headers
-- (which is non-compliant but widely accepted by clients).
-- For the HTTP/2 path, proper trailer support requires changes
-- to the H2 bridge to use responseStreamingWithTrailers.
-- For gRPC-Web, trailers are correctly encoded in the response body.
grpcResponseToHttp :: GrpcResponse -> Response Body
grpcResponseToHttp (GrpcUnary status payload) =
  let body = encodeMessage payload
      trailers = grpcTrailers status
  in Response status200
    (grpcResponseHeaders ++ trailers)
    (fromBytes body)

grpcResponseToHttp (GrpcStream status payloads) =
  let body = encodeMessages payloads
      trailers = grpcTrailers status
  in Response status200
    (grpcResponseHeaders ++ trailers)
    (fromBytes body)

grpcResponseToHttp (GrpcError status) =
  grpcTrailersOnly status


-- | Trailers-only response (error, no body).
grpcTrailersOnly :: GrpcStatus -> Response Body
grpcTrailersOnly status =
  Response status200
    (grpcResponseHeaders ++ grpcTrailers status)
    emptyBody


-- | Standard gRPC response headers.
grpcResponseHeaders :: [(CI.CI ByteString, ByteString)]
grpcResponseHeaders =
  [ ("content-type", "application/grpc+proto")
  ]


-- | gRPC trailers from a status.
grpcTrailers :: GrpcStatus -> [(CI.CI ByteString, ByteString)]
grpcTrailers status =
  [ ("grpc-status", GS.grpcStatusCode status)
  ] ++ if T.null (gsMessage status)
       then []
       else [("grpc-message", TE.encodeUtf8 (gsMessage status))]
