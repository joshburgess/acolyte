{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits (shiftL)
import Data.Word (Word32)
import qualified Network.HTTP.Types as HTTP

import Tower.Service (Service (..))
import Http.Core

import Tower.Layer (applyLayer)
import Tower.Grpc


-- ===================================================================
-- Test helpers
-- ===================================================================

assert :: String -> Bool -> IO ()
assert msg True  = putStrLn $ "  OK: " ++ msg
assert msg False = error   $ "FAIL: " ++ msg


-- ===================================================================
-- Codec tests
-- ===================================================================

testCodec :: IO ()
testCodec = do
  putStrLn "Codec:"

  -- Encode a simple message
  let encoded = encodeMessage "hello"
  assert "encode: 5-byte header + payload" (BS.length encoded == 10)
  assert "encode: first byte is 0 (uncompressed)" (BS.index encoded 0 == 0)
  assert "encode: length bytes = 5" (BS.index encoded 4 == 5)

  -- Decode it back
  case decodeMessage encoded of
    Just (msg, rest) -> do
      assert "decode: not compressed" (not (gmCompressed msg))
      assert "decode: payload matches" (gmPayload msg == "hello")
      assert "decode: no remainder" (BS.null rest)
    Nothing -> error "FAIL: decode returned Nothing"

  -- Roundtrip multiple messages
  let multi = encodeMessages ["aaa", "bb", "c"]
  let decoded = decodeMessages multi
  assert "multi: 3 messages" (length decoded == 3)
  assert "multi: first payload" (gmPayload (decoded !! 0) == "aaa")
  assert "multi: second payload" (gmPayload (decoded !! 1) == "bb")
  assert "multi: third payload" (gmPayload (decoded !! 2) == "c")

  -- Empty input
  assert "empty: no messages" (null (decodeMessages ""))

  -- Incomplete input
  assert "incomplete: Nothing" (decodeMessage "\x00\x00\x00\x00\x05hi" == Nothing)


-- ===================================================================
-- Status tests
-- ===================================================================

testStatus :: IO ()
testStatus = do
  putStrLn "\nStatus:"

  assert "OK code = 0" (gsCode (grpcOk "ok") == 0)
  assert "INTERNAL code = 13" (gsCode (grpcInternal "err") == 13)
  assert "UNIMPLEMENTED code = 12" (gsCode (grpcUnimplemented "nope") == 12)
  assert "statusCode OK" (grpcStatusCode (grpcOk "") == "0")
  assert "statusCode UNAVAILABLE" (grpcStatusCode (grpcUnavailable "") == "14")
  assert "statusFromCode 5" (gsCode (statusFromCode 5) == 5)


-- ===================================================================
-- Server dispatch tests
-- ===================================================================

testServer :: IO ()
testServer = do
  putStrLn "\nServer dispatch:"

  -- Build a simple service
  let services = grpcServiceMap
        [ ("test.Echo", "Echo", unaryHandler echoHandler)
        , ("test.Echo", "Fail", unaryHandler failHandler)
        ]
  let svc = grpcServer services

  -- Unary RPC: success
  resp <- callGrpc svc "test.Echo" "Echo" (encodeMessage "hello")
  assert "echo: status 200" (HTTP.statusCode (responseStatus resp) == 200)
  let respBody = responseBody resp
  respBytes <- bodyToStrict respBody
  case decodeMessage respBytes of
    Just (msg, _) -> assert "echo: payload" (gmPayload msg == "ECHO:hello")
    Nothing -> error "FAIL: could not decode echo response"
  assert "echo: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")

  -- Unary RPC: handler error
  resp2 <- callGrpc svc "test.Echo" "Fail" (encodeMessage "bad")
  assert "fail: grpc-status 13"
    (lookup "grpc-status" (responseHeaders resp2) == Just "13")

  -- Unimplemented method
  resp3 <- callGrpc svc "test.Echo" "Nope" (encodeMessage "x")
  assert "unimplemented: grpc-status 12"
    (lookup "grpc-status" (responseHeaders resp3) == Just "12")

  -- Wrong content-type
  resp4 <- callRaw svc "application/json" "/test.Echo/Echo" ""
  assert "wrong ct: status 415" (HTTP.statusCode (responseStatus resp4) == 415)


-- Test handlers
echoHandler :: ByteString -> IO (Either GrpcStatus ByteString)
echoHandler payload = pure $ Right ("ECHO:" <> payload)

failHandler :: ByteString -> IO (Either GrpcStatus ByteString)
failHandler _ = pure $ Left (grpcInternal "something broke")


-- ===================================================================
-- Server streaming test
-- ===================================================================

testStreaming :: IO ()
testStreaming = do
  putStrLn "\nServer streaming:"

  let services = grpcServiceMap
        [ ("test.Numbers", "List", serverStreamHandler listHandler)
        ]
  let svc = grpcServer services

  resp <- callGrpc svc "test.Numbers" "List" (encodeMessage "3")
  respBytes <- bodyToStrict (responseBody resp)
  let msgs = decodeMessages respBytes
  assert "stream: 3 messages" (length msgs == 3)
  assert "stream: first" (gmPayload (msgs !! 0) == "item-0")
  assert "stream: second" (gmPayload (msgs !! 1) == "item-1")
  assert "stream: third" (gmPayload (msgs !! 2) == "item-2")
  assert "stream: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")

listHandler :: ByteString -> IO (Either GrpcStatus [ByteString])
listHandler _ = pure $ Right
  [ "item-0", "item-1", "item-2" ]


-- ===================================================================
-- Helpers
-- ===================================================================

-- | Send a gRPC request to the service.
callGrpc
  :: Service IO (Request Body) (Response Body)
  -> Text -> Text -> ByteString
  -> IO (Response Body)
callGrpc svc service method body = do
  let path = "/" <> T.unpack service <> "/" <> T.unpack method
  callRaw svc "application/grpc+proto" (BS.pack (map (fromIntegral . fromEnum) path)) body

-- | Send a raw request.
callRaw
  :: Service IO (Request Body) (Response Body)
  -> ByteString -> ByteString -> ByteString
  -> IO (Response Body)
callRaw (Service handler) ct path body = do
  exts <- emptyExtensions
  let req = Request
        { requestMethod     = "POST"
        , requestPathRaw    = path
        , requestPath       = []
        , requestQuery      = []
        , requestHeaders    = [("content-type", ct)]
        , requestBody       = fromBytes body
        , requestExtensions = exts
        }
  handler req


-- ===================================================================
-- Codec edge cases
-- ===================================================================

testCodecEdgeCases :: IO ()
testCodecEdgeCases = do
  putStrLn "\nCodec edge cases:"

  -- Zero-length message
  let zeroEnc = encodeMessage ""
  assert "zero-len: 5-byte frame" (BS.length zeroEnc == 5)
  case decodeMessage zeroEnc of
    Just (msg, rest) -> do
      assert "zero-len: empty payload" (BS.null (gmPayload msg))
      assert "zero-len: no remainder" (BS.null rest)
    Nothing -> error "FAIL: zero-len decode returned Nothing"

  -- Large message (100KB)
  let bigPayload = BS.replicate (100 * 1024) 0x42
  let bigEnc = encodeMessage bigPayload
  case decodeMessage bigEnc of
    Just (msg, rest) -> do
      assert "100KB: roundtrip payload" (gmPayload msg == bigPayload)
      assert "100KB: no remainder" (BS.null rest)
    Nothing -> error "FAIL: 100KB decode returned Nothing"

  -- Multiple messages with trailing garbage
  let msg1 = encodeMessage "first"
  let msg2 = encodeMessage "second"
  let garbage = BS.pack [0xDE, 0xAD, 0xFF]
  let combined = msg1 <> msg2 <> garbage
  let decoded = decodeMessages combined
  assert "partial trailing: 2 complete messages" (length decoded == 2)
  assert "partial trailing: first payload" (gmPayload (decoded !! 0) == "first")
  assert "partial trailing: second payload" (gmPayload (decoded !! 1) == "second")

  -- Compressed flag
  let compressedFrame = BS.pack [0x01, 0x00, 0x00, 0x00, 0x03] <> "abc"
  case decodeMessage compressedFrame of
    Just (msg, _) -> do
      assert "compressed flag: gmCompressed = True" (gmCompressed msg)
      assert "compressed flag: payload" (gmPayload msg == "abc")
    Nothing -> error "FAIL: compressed frame decode returned Nothing"


-- ===================================================================
-- Server edge cases
-- ===================================================================

testServerEdgeCases :: IO ()
testServerEdgeCases = do
  putStrLn "\nServer edge cases:"

  -- Empty service map: any method returns grpc-status 12
  let emptySvc = grpcServer (grpcServiceMap [])
  resp1 <- callGrpc emptySvc "any.Service" "AnyMethod" (encodeMessage "x")
  assert "empty map: grpc-status 12"
    (lookup "grpc-status" (responseHeaders resp1) == Just "12")

  -- Path parsing edge cases
  resp2 <- callRaw emptySvc "application/grpc+proto" "/" ""
  assert "path /: grpc-status 12"
    (lookup "grpc-status" (responseHeaders resp2) == Just "12")

  resp3 <- callRaw emptySvc "application/grpc+proto" "" ""
  assert "path empty: grpc-status 12"
    (lookup "grpc-status" (responseHeaders resp3) == Just "12")

  resp4 <- callRaw emptySvc "application/grpc+proto" "/noSlash" ""
  assert "path /noSlash: grpc-status 12"
    (lookup "grpc-status" (responseHeaders resp4) == Just "12")

  resp5 <- callRaw emptySvc "application/grpc+proto" "/a/b/c" ""
  assert "path /a/b/c: grpc-status 12"
    (lookup "grpc-status" (responseHeaders resp5) == Just "12")

  -- Metadata propagation
  let services = grpcServiceMap
        [ ("test.Meta", "Check", metadataHandler)
        ]
  let metaSvc = grpcServer services
  resp6 <- callGrpcWithHeaders metaSvc "test.Meta" "Check" (encodeMessage "")
             [("x-custom-header", "custom-value"), ("content-type", "application/grpc+proto")]
  respBytes6 <- bodyToStrict (responseBody resp6)
  case decodeMessage respBytes6 of
    Just (msg, _) -> assert "metadata: custom header propagated"
      (gmPayload msg == "found")
    Nothing -> error "FAIL: metadata response decode failed"

  -- grpc-message trailer on error
  let errServices = grpcServiceMap
        [ ("test.Err", "Boom", unaryHandler (\_ -> pure (Left (grpcInternal "kaboom"))))
        ]
  let errSvc = grpcServer errServices
  resp7 <- callGrpc errSvc "test.Err" "Boom" (encodeMessage "x")
  assert "grpc-message: present on error"
    (lookup "grpc-message" (responseHeaders resp7) == Just "kaboom")
  assert "grpc-message: status 13"
    (lookup "grpc-status" (responseHeaders resp7) == Just "13")

  -- grpc-message on unimplemented
  let resp1Msg = lookup "grpc-message" (responseHeaders resp1)
  assert "grpc-message: present on unimplemented" (resp1Msg /= Nothing)


-- | Handler that checks if x-custom-header is in metadata
metadataHandler :: GrpcHandler
metadataHandler = GrpcHandler $ \req ->
  let hdrs = grpcMetadata req
      found = case lookup "x-custom-header" hdrs of
        Just "custom-value" -> True
        _                   -> False
      payload = if found then "found" else "not-found"
  in pure $ GrpcUnary (grpcOk "") payload

-- | Send a gRPC request with custom headers.
callGrpcWithHeaders
  :: Service IO (Request Body) (Response Body)
  -> Text -> Text -> ByteString
  -> [(BS.ByteString, BS.ByteString)]
  -> IO (Response Body)
callGrpcWithHeaders (Service handler) service method body hdrs = do
  exts <- emptyExtensions
  let path = "/" <> T.unpack service <> "/" <> T.unpack method
  let req = Request
        { requestMethod     = "POST"
        , requestPathRaw    = BS.pack (map (fromIntegral . fromEnum) path)
        , requestPath       = []
        , requestQuery      = []
        , requestHeaders    = map (\(k, v) -> (CI.mk k, v)) hdrs
        , requestBody       = fromBytes body
        , requestExtensions = exts
        }
  handler req


-- ===================================================================
-- Status code exhaustive tests
-- ===================================================================

testStatusExhaustive :: IO ()
testStatusExhaustive = do
  putStrLn "\nStatus exhaustive:"

  -- All 17 status codes with correct numeric values
  assert "OK = 0"                  (gsCode (grpcOk "") == 0)
  assert "CANCELLED = 1"           (gsCode (grpcCancelled "") == 1)
  assert "UNKNOWN = 2"             (gsCode (grpcUnknown "") == 2)
  assert "INVALID_ARGUMENT = 3"    (gsCode (grpcInvalidArgument "") == 3)
  assert "DEADLINE_EXCEEDED = 4"   (gsCode (grpcDeadlineExceeded "") == 4)
  assert "NOT_FOUND = 5"           (gsCode (grpcNotFound "") == 5)
  assert "ALREADY_EXISTS = 6"      (gsCode (grpcAlreadyExists "") == 6)
  assert "PERMISSION_DENIED = 7"   (gsCode (grpcPermissionDenied "") == 7)
  assert "RESOURCE_EXHAUSTED = 8"  (gsCode (grpcResourceExhausted "") == 8)
  assert "FAILED_PRECONDITION = 9" (gsCode (grpcFailedPrecondition "") == 9)
  assert "ABORTED = 10"            (gsCode (grpcAborted "") == 10)
  assert "OUT_OF_RANGE = 11"       (gsCode (grpcOutOfRange "") == 11)
  assert "UNIMPLEMENTED = 12"      (gsCode (grpcUnimplemented "") == 12)
  assert "INTERNAL = 13"           (gsCode (grpcInternal "") == 13)
  assert "UNAVAILABLE = 14"        (gsCode (grpcUnavailable "") == 14)
  assert "DATA_LOSS = 15"          (gsCode (grpcDataLoss "") == 15)
  assert "UNAUTHENTICATED = 16"    (gsCode (grpcUnauthenticated "") == 16)

  -- statusFromCode roundtrips with gsCode
  let codes = [0..16]
  let roundtrips = all (\c -> gsCode (statusFromCode c) == c) codes
  assert "statusFromCode roundtrip 0..16" roundtrips

  -- grpcStatusCode produces correct string representations
  assert "grpcStatusCode OK = \"0\""  (grpcStatusCode (grpcOk "") == "0")
  assert "grpcStatusCode 16 = \"16\"" (grpcStatusCode (grpcUnauthenticated "") == "16")
  assert "grpcStatusCode 7 = \"7\""   (grpcStatusCode (grpcPermissionDenied "") == "7")


-- ===================================================================
-- Multiplex tests
-- ===================================================================

testMultiplex :: IO ()
testMultiplex = do
  putStrLn "\nMultiplex (REST + gRPC):"

  -- REST echo service: responds with "REST:" ++ body
  let restSvc = Service $ \req -> do
        body <- bodyToStrict (requestBody req)
        pure $ Response HTTP.status200
          [("content-type", "application/json")]
          (fromBytes ("REST:" <> body))

  -- gRPC echo service
  let grpcServices = grpcServiceMap
        [ ("test.Echo", "Echo", unaryHandler echoHandler)
        ]
  let grpcSvc = grpcServer grpcServices

  -- Combined service
  let combined = multiplex restSvc grpcSvc

  -- application/json -> REST
  resp1 <- callRaw combined "application/json" "/test" "hello"
  body1 <- bodyToStrict (responseBody resp1)
  assert "json -> REST handler" (body1 == "REST:hello")
  assert "json -> status 200" (HTTP.statusCode (responseStatus resp1) == 200)

  -- application/grpc+proto -> gRPC
  resp2 <- callGrpc combined "test.Echo" "Echo" (encodeMessage "world")
  respBytes2 <- bodyToStrict (responseBody resp2)
  case decodeMessage respBytes2 of
    Just (msg, _) -> assert "grpc+proto -> gRPC handler" (gmPayload msg == "ECHO:world")
    Nothing -> error "FAIL: could not decode gRPC response in multiplex test"
  assert "grpc+proto -> grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp2) == Just "0")

  -- application/grpc (bare) -> gRPC
  resp3 <- callRaw combined "application/grpc" "/test.Echo/Echo" (encodeMessage "bare")
  respBytes3 <- bodyToStrict (responseBody resp3)
  case decodeMessage respBytes3 of
    Just (msg, _) -> assert "grpc bare -> gRPC handler" (gmPayload msg == "ECHO:bare")
    Nothing -> error "FAIL: could not decode bare gRPC response in multiplex test"

  -- No content-type -> REST
  resp4 <- callNoContentType combined "/test" "fallback"
  body4 <- bodyToStrict (responseBody resp4)
  assert "no content-type -> REST handler" (body4 == "REST:fallback")


-- | Send a request with no Content-Type header.
callNoContentType
  :: Service IO (Request Body) (Response Body)
  -> ByteString -> ByteString
  -> IO (Response Body)
callNoContentType (Service handler) path body = do
  exts <- emptyExtensions
  let req = Request
        { requestMethod     = "POST"
        , requestPathRaw    = path
        , requestPath       = []
        , requestQuery      = []
        , requestHeaders    = []
        , requestBody       = fromBytes body
        , requestExtensions = exts
        }
  handler req


-- ===================================================================
-- Reflection tests
-- ===================================================================

testReflection :: IO ()
testReflection = do
  putStrLn "\nReflection:"

  let userMethods =
        [ MethodInfo "GetUser" "GetUserRequest" "GetUserResponse"
        , MethodInfo "ListUsers" "ListUsersRequest" "ListUsersResponse"
        ]
  let orderMethods =
        [ MethodInfo "CreateOrder" "CreateOrderRequest" "CreateOrderResponse"
        ]
  let userProto = "syntax = \"proto3\";\nservice UserService { rpc GetUser ... }"
  let orderProto = "syntax = \"proto3\";\nservice OrderService { rpc CreateOrder ... }"
  let infos =
        [ ServiceInfo "myapp.UserService" userMethods userProto
        , ServiceInfo "myapp.OrderService" orderMethods orderProto
        ]

  let services = withReflection infos (grpcServiceMap [])
  let svc = grpcServer services

  -- 1. list_services: both service names appear
  resp1 <- callGrpc svc "grpc.reflection.v1alpha.ServerReflection"
             "ServerReflectionInfo" (encodeMessage "list_services")
  respBytes1 <- bodyToStrict (responseBody resp1)
  case decodeMessage respBytes1 of
    Just (msg, _) -> do
      let body = gmPayload msg
      assert "reflection list: contains UserService"
        (BS.isInfixOf "myapp.UserService" body)
      assert "reflection list: contains OrderService"
        (BS.isInfixOf "myapp.OrderService" body)
    Nothing -> error "FAIL: could not decode reflection list response"
  assert "reflection list: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp1) == Just "0")

  -- 2. file_containing_symbol with known service
  resp2 <- callGrpc svc "grpc.reflection.v1alpha.ServerReflection"
             "ServerReflectionInfo"
             (encodeMessage "file_containing_symbol:myapp.UserService")
  respBytes2 <- bodyToStrict (responseBody resp2)
  case decodeMessage respBytes2 of
    Just (msg, _) -> do
      let body = gmPayload msg
      assert "reflection file_containing_symbol: returns proto"
        (BS.isInfixOf "UserService" body)
      assert "reflection file_containing_symbol: correct proto text"
        (body == "syntax = \"proto3\";\nservice UserService { rpc GetUser ... }")
    Nothing -> error "FAIL: could not decode reflection file response"
  assert "reflection file: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp2) == Just "0")

  -- 3. file_containing_symbol with unknown symbol -> NOT_FOUND (5)
  resp3 <- callGrpc svc "grpc.reflection.v1alpha.ServerReflection"
             "ServerReflectionInfo"
             (encodeMessage "file_containing_symbol:unknown.Service")
  assert "reflection unknown: grpc-status 5"
    (lookup "grpc-status" (responseHeaders resp3) == Just "5")

  -- 4. Unknown request type -> NOT_FOUND (5)
  resp4 <- callGrpc svc "grpc.reflection.v1alpha.ServerReflection"
             "ServerReflectionInfo"
             (encodeMessage "something_else")
  assert "reflection bad request: grpc-status 5"
    (lookup "grpc-status" (responseHeaders resp4) == Just "5")


-- ===================================================================
-- Client streaming tests
-- ===================================================================

testClientStreaming :: IO ()
testClientStreaming = do
  putStrLn "\nClient streaming:"

  let services = grpcServiceMap
        [ ("test.Agg", "Sum", clientStreamHandler sumHandler)
        ]
  let svc = grpcServer services

  -- Send 3 messages, verify handler receives all 3 payloads
  let body = encodeMessages ["aaa", "bbb", "ccc"]
  resp <- callGrpc svc "test.Agg" "Sum" body
  assert "client-stream: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")
  respBytes <- bodyToStrict (responseBody resp)
  case decodeMessage respBytes of
    Just (msg, _) -> assert "client-stream: received all 3 payloads"
      (gmPayload msg == "3:aaa,bbb,ccc")
    Nothing -> error "FAIL: could not decode client-stream response"

  -- Error case
  let errServices = grpcServiceMap
        [ ("test.Agg", "Fail", clientStreamHandler (\_ -> pure (Left (grpcInternal "client stream error"))))
        ]
  let errSvc = grpcServer errServices
  resp2 <- callGrpc errSvc "test.Agg" "Fail" (encodeMessages ["x"])
  assert "client-stream error: grpc-status 13"
    (lookup "grpc-status" (responseHeaders resp2) == Just "13")

sumHandler :: [ByteString] -> IO (Either GrpcStatus ByteString)
sumHandler payloads = do
  let count = show (length payloads)
      joined = BS.intercalate "," payloads
  pure $ Right (BS8.pack count <> ":" <> joined)


-- ===================================================================
-- Bidirectional streaming tests
-- ===================================================================

testBidiStreaming :: IO ()
testBidiStreaming = do
  putStrLn "\nBidirectional streaming:"

  let services = grpcServiceMap
        [ ("test.Echo", "BidiEcho", bidiStreamHandler bidiEchoHandler)
        ]
  let svc = grpcServer services

  -- Send 3 messages, verify 3 response messages
  let body = encodeMessages ["one", "two", "three"]
  resp <- callGrpc svc "test.Echo" "BidiEcho" body
  assert "bidi-stream: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")
  respBytes <- bodyToStrict (responseBody resp)
  let msgs = decodeMessages respBytes
  assert "bidi-stream: 3 response messages" (length msgs == 3)
  assert "bidi-stream: first" (gmPayload (msgs !! 0) == "ECHO:one")
  assert "bidi-stream: second" (gmPayload (msgs !! 1) == "ECHO:two")
  assert "bidi-stream: third" (gmPayload (msgs !! 2) == "ECHO:three")

  -- Error case
  let errServices = grpcServiceMap
        [ ("test.Echo", "BidiFail", bidiStreamHandler (\_ -> pure (Left (grpcAborted "bidi fail"))))
        ]
  let errSvc = grpcServer errServices
  resp2 <- callGrpc errSvc "test.Echo" "BidiFail" (encodeMessages ["x"])
  assert "bidi-stream error: grpc-status 10"
    (lookup "grpc-status" (responseHeaders resp2) == Just "10")

bidiEchoHandler :: [ByteString] -> IO (Either GrpcStatus [ByteString])
bidiEchoHandler payloads = pure $ Right (map ("ECHO:" <>) payloads)


-- ===================================================================
-- Health check tests
-- ===================================================================

testHealthCheck :: IO ()
testHealthCheck = do
  putStrLn "\nHealth check:"

  -- SERVING status
  let services = withHealthCheck (pure Serving) (grpcServiceMap [])
  let svc = grpcServer services

  resp <- callGrpc svc "grpc.health.v1.Health" "Check" (encodeMessage "")
  assert "health: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")
  respBytes <- bodyToStrict (responseBody resp)
  case decodeMessage respBytes of
    Just (msg, _) -> assert "health: SERVING = byte 1"
      (gmPayload msg == BS.singleton 1)
    Nothing -> error "FAIL: could not decode health check response"

  -- NOT_SERVING status
  let services2 = withHealthCheck (pure NotServing) (grpcServiceMap [])
  let svc2 = grpcServer services2
  resp2 <- callGrpc svc2 "grpc.health.v1.Health" "Check" (encodeMessage "")
  respBytes2 <- bodyToStrict (responseBody resp2)
  case decodeMessage respBytes2 of
    Just (msg, _) -> assert "health: NOT_SERVING = byte 2"
      (gmPayload msg == BS.singleton 2)
    Nothing -> error "FAIL: could not decode health check NOT_SERVING response"

  -- UNKNOWN status
  let services3 = withHealthCheck (pure Unknown) (grpcServiceMap [])
  let svc3 = grpcServer services3
  resp3 <- callGrpc svc3 "grpc.health.v1.Health" "Check" (encodeMessage "")
  respBytes3 <- bodyToStrict (responseBody resp3)
  case decodeMessage respBytes3 of
    Just (msg, _) -> assert "health: UNKNOWN = byte 0"
      (gmPayload msg == BS.singleton 0)
    Nothing -> error "FAIL: could not decode health check UNKNOWN response"


-- ===================================================================
-- Compression tests
-- ===================================================================

testCompression :: IO ()
testCompression = do
  putStrLn "\nCompression:"

  -- Roundtrip: compress then decompress
  let original = "Hello, gRPC compression test! This is a payload."
  let compressed = compressMessage original
  let Right decompressed = decompressMessage compressed
  assert "compress roundtrip: equality" (decompressed == original)
  assert "compress: output differs from input" (compressed /= original)

  -- Encode compressed message: verify flag byte is 1
  let encoded = encodeMessageCompressed "test-payload"
  assert "encodeCompressed: flag byte is 1" (BS.index encoded 0 == 1)

  -- Decode the compressed message frame
  case decodeMessage encoded of
    Just (msg, rest) -> do
      assert "encodeCompressed: compressed flag set" (gmCompressed msg)
      assert "encodeCompressed: no remainder" (BS.null rest)
      -- Decompress the payload and verify
      let Right payload = decompressMessage (gmPayload msg)
      assert "encodeCompressed: payload roundtrip" (payload == "test-payload")
    Nothing -> error "FAIL: could not decode compressed message"

  -- Empty payload roundtrip
  let emptyCompressed = compressMessage ""
  let Right emptyDecompressed = decompressMessage emptyCompressed
  assert "compress empty: roundtrip" (emptyDecompressed == "")

  -- Large payload (10KB) roundtrip
  let bigPayload = BS.replicate (10 * 1024) 0xAB
  let bigCompressed = compressMessage bigPayload
  let Right bigDecompressed = decompressMessage bigCompressed
  assert "compress 10KB: roundtrip" (bigDecompressed == bigPayload)
  assert "compress 10KB: compressed differs" (bigCompressed /= bigPayload)

  -- Roundtrip via encodeMessageCompressed + decode + decompress
  let roundtripPayload = "roundtrip through encode/decode"
  let rtEncoded = encodeMessageCompressed roundtripPayload
  case decodeMessage rtEncoded of
    Just (msg, _) -> do
      let Right rtDecoded = decompressMessage (gmPayload msg)
      assert "compress encode/decode roundtrip" (rtDecoded == roundtripPayload)
    Nothing -> error "FAIL: could not decode roundtrip compressed message"


-- ===================================================================
-- Health check: additional coverage
-- ===================================================================

testHealthCheckExtended :: IO ()
testHealthCheckExtended = do
  putStrLn "\nHealth check (extended):"

  -- Health service is at the correct path
  let services = withHealthCheck (pure Serving) (grpcServiceMap [])
  let svc = grpcServer services

  -- Call with exact expected path: "grpc.health.v1.Health"/"Check"
  resp <- callGrpc svc "grpc.health.v1.Health" "Check" (encodeMessage "")
  assert "health path: correct service/method"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")

  -- withHealthCheck adds to existing service map (non-empty)
  let existingServices = grpcServiceMap
        [ ("test.Echo", "Echo", unaryHandler echoHandler)
        ]
  let combined = withHealthCheck (pure Serving) existingServices
  let combinedSvc = grpcServer combined

  -- Original service still works
  resp2 <- callGrpc combinedSvc "test.Echo" "Echo" (encodeMessage "hi")
  assert "health+existing: original service works"
    (lookup "grpc-status" (responseHeaders resp2) == Just "0")
  respBytes2 <- bodyToStrict (responseBody resp2)
  case decodeMessage respBytes2 of
    Just (msg, _) -> assert "health+existing: original handler" (gmPayload msg == "ECHO:hi")
    Nothing -> error "FAIL: could not decode echo in combined health test"

  -- Health service also works in combined
  resp3 <- callGrpc combinedSvc "grpc.health.v1.Health" "Check" (encodeMessage "")
  assert "health+existing: health works"
    (lookup "grpc-status" (responseHeaders resp3) == Just "0")
  respBytes3 <- bodyToStrict (responseBody resp3)
  case decodeMessage respBytes3 of
    Just (msg, _) -> assert "health+existing: SERVING" (gmPayload msg == BS.singleton 1)
    Nothing -> error "FAIL: could not decode health in combined test"

  -- Wrong method on health service -> unimplemented (12)
  resp4 <- callGrpc combinedSvc "grpc.health.v1.Health" "Watch" (encodeMessage "")
  assert "health: wrong method -> grpc-status 12"
    (lookup "grpc-status" (responseHeaders resp4) == Just "12")


-- ===================================================================
-- Client streaming: additional coverage
-- ===================================================================

testClientStreamingExtended :: IO ()
testClientStreamingExtended = do
  putStrLn "\nClient streaming (extended):"

  let services = grpcServiceMap
        [ ("test.Agg", "Sum", clientStreamHandler sumHandler)
        ]
  let svc = grpcServer services

  -- Send 0 messages, verify handler receives empty list
  let emptyBody = encodeMessages []
  resp <- callGrpc svc "test.Agg" "Sum" emptyBody
  assert "client-stream 0 msgs: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")
  respBytes <- bodyToStrict (responseBody resp)
  case decodeMessage respBytes of
    Just (msg, _) -> assert "client-stream 0 msgs: empty list"
      (gmPayload msg == "0:")
    Nothing -> error "FAIL: could not decode client-stream 0 msgs response"

  -- Handler error propagates to grpc-status
  let errServices = grpcServiceMap
        [ ("test.Agg", "Err", clientStreamHandler
            (\_ -> pure (Left (grpcPermissionDenied "not allowed"))))
        ]
  let errSvc = grpcServer errServices
  resp2 <- callGrpc errSvc "test.Agg" "Err" (encodeMessages ["a", "b"])
  assert "client-stream error: grpc-status 7 (PERMISSION_DENIED)"
    (lookup "grpc-status" (responseHeaders resp2) == Just "7")
  assert "client-stream error: grpc-message propagated"
    (lookup "grpc-message" (responseHeaders resp2) == Just "not allowed")


-- ===================================================================
-- Bidi streaming: additional coverage
-- ===================================================================

testBidiStreamingExtended :: IO ()
testBidiStreamingExtended = do
  putStrLn "\nBidirectional streaming (extended):"

  -- Send 3, get 3 with transformation
  let services = grpcServiceMap
        [ ("test.Transform", "Upper", bidiStreamHandler upperHandler)
        ]
  let svc = grpcServer services

  let body = encodeMessages ["foo", "bar", "baz"]
  resp <- callGrpc svc "test.Transform" "Upper" body
  assert "bidi-extended: grpc-status 0"
    (lookup "grpc-status" (responseHeaders resp) == Just "0")
  respBytes <- bodyToStrict (responseBody resp)
  let msgs = decodeMessages respBytes
  assert "bidi-extended: 3 response messages" (length msgs == 3)
  assert "bidi-extended: first is UPPER:foo" (gmPayload (msgs !! 0) == "UPPER:foo")
  assert "bidi-extended: second is UPPER:bar" (gmPayload (msgs !! 1) == "UPPER:bar")
  assert "bidi-extended: third is UPPER:baz" (gmPayload (msgs !! 2) == "UPPER:baz")

  -- Handler error propagates with correct status code
  let errServices = grpcServiceMap
        [ ("test.Transform", "Fail", bidiStreamHandler
            (\_ -> pure (Left (grpcUnavailable "service down"))))
        ]
  let errSvc = grpcServer errServices
  resp2 <- callGrpc errSvc "test.Transform" "Fail" (encodeMessages ["x", "y"])
  assert "bidi-extended error: grpc-status 14 (UNAVAILABLE)"
    (lookup "grpc-status" (responseHeaders resp2) == Just "14")
  assert "bidi-extended error: grpc-message"
    (lookup "grpc-message" (responseHeaders resp2) == Just "service down")

upperHandler :: [ByteString] -> IO (Either GrpcStatus [ByteString])
upperHandler payloads = pure $ Right (map (\p -> "UPPER:" <> p) payloads)


-- ===================================================================
-- gRPC-Web tests
-- ===================================================================

testGrpcWeb :: IO ()
testGrpcWeb = do
  putStrLn "\ngRPC-Web:"

  -- Build a gRPC service wrapped with the gRPC-Web layer
  let services = grpcServiceMap
        [ ("test.Echo", "Echo", unaryHandler echoHandler)
        ]
  let svc = applyLayer grpcWebLayer (grpcServer services)

  -- 1. gRPC-Web content-type is rewritten so the inner service handles it
  resp1 <- callRawWithCT svc "application/grpc-web" "/test.Echo/Echo" (encodeMessage "web-hello")
  assert "grpc-web: status 200" (HTTP.statusCode (responseStatus resp1) == 200)

  -- 2. Response content-type should be application/grpc-web+proto
  let ct1 = lookup "content-type" (responseHeaders resp1)
  assert "grpc-web: response content-type is grpc-web+proto"
    (ct1 == Just "application/grpc-web+proto")

  -- 3. grpc-status and grpc-message should NOT be in headers (moved to body)
  assert "grpc-web: grpc-status not in headers"
    (lookup "grpc-status" (responseHeaders resp1) == Nothing)

  -- 4. Body should contain the original message frame + trailer frame
  body1 <- bodyToStrict (responseBody resp1)
  -- Decode the first message (the actual response)
  case decodeMessage body1 of
    Just (msg, rest) -> do
      assert "grpc-web: payload correct" (gmPayload msg == "ECHO:web-hello")
      -- The rest should be a trailer frame starting with 0x80
      assert "grpc-web: trailer frame present" (not (BS.null rest))
      assert "grpc-web: trailer flag is 0x80" (BS.index rest 0 == 0x80)
      -- Decode the trailer frame
      let trailerLen = decodeWord32BE (BS.take 4 (BS.drop 1 rest))
          trailerData = BS.drop 5 rest
      assert "grpc-web: trailer length matches" (fromIntegral (BS.length trailerData) == trailerLen)
      -- Trailer should contain grpc-status: 0
      assert "grpc-web: trailer contains grpc-status"
        (BS.isInfixOf "grpc-status: 0" trailerData)
    Nothing -> error "FAIL: could not decode grpc-web response body"

  -- 5. application/grpc-web+proto also works
  resp2 <- callRawWithCT svc "application/grpc-web+proto" "/test.Echo/Echo" (encodeMessage "proto")
  ct2 <- pure $ lookup "content-type" (responseHeaders resp2)
  assert "grpc-web+proto: response content-type is grpc-web+proto"
    (ct2 == Just "application/grpc-web+proto")

  -- 6. Regular gRPC passes through unmodified
  resp3 <- callRaw svc "application/grpc+proto" "/test.Echo/Echo" (encodeMessage "native")
  assert "native grpc: grpc-status in headers"
    (lookup "grpc-status" (responseHeaders resp3) == Just "0")
  assert "native grpc: content-type unchanged"
    (lookup "content-type" (responseHeaders resp3) == Just "application/grpc+proto")

  -- 7. Error responses also encode trailers in body
  let errServices = grpcServiceMap
        [ ("test.Err", "Boom", unaryHandler (\_ -> pure (Left (grpcInternal "kaboom"))))
        ]
  let errSvc = applyLayer grpcWebLayer (grpcServer errServices)
  resp4 <- callRawWithCT errSvc "application/grpc-web" "/test.Err/Boom" (encodeMessage "x")
  assert "grpc-web error: grpc-status not in headers"
    (lookup "grpc-status" (responseHeaders resp4) == Nothing)
  body4 <- bodyToStrict (responseBody resp4)
  -- Even for error responses, trailers should be in the body
  -- The body may be empty (trailers-only) but the trailer frame should exist
  assert "grpc-web error: trailer frame in body" (BS.isInfixOf "grpc-status: 13" body4)
  assert "grpc-web error: grpc-message in trailer" (BS.isInfixOf "grpc-message: kaboom" body4)


-- | Helper to decode a big-endian Word32 from 4 bytes (for test use)
decodeWord32BE :: ByteString -> Word32
decodeWord32BE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word32
      b1 = fromIntegral (BS.index bs 1) :: Word32
      b2 = fromIntegral (BS.index bs 2) :: Word32
      b3 = fromIntegral (BS.index bs 3) :: Word32
  in (b0 `shiftL` 24) + (b1 `shiftL` 16) + (b2 `shiftL` 8) + b3

-- | Send a request with specific content-type (CI headers).
callRawWithCT
  :: Service IO (Request Body) (Response Body)
  -> ByteString -> ByteString -> ByteString
  -> IO (Response Body)
callRawWithCT (Service handler) ct path body = do
  exts <- emptyExtensions
  let req = Request
        { requestMethod     = "POST"
        , requestPathRaw    = path
        , requestPath       = []
        , requestQuery      = []
        , requestHeaders    = [(CI.mk "content-type", ct)]
        , requestBody       = fromBytes body
        , requestExtensions = exts
        }
  handler req


-- ===================================================================
-- Error details tests
-- ===================================================================

testErrorDetails :: IO ()
testErrorDetails = do
  putStrLn "\nError details:"

  -- 1. richError constructs a RichGrpcStatus
  let rich = richError (grpcInvalidArgument "bad input")
        [ DetailBadRequest $ BadRequest
            [ FieldViolation "name" "must not be empty"
            , FieldViolation "age" "must be positive"
            ]
        ]
  assert "richError: status code 3" (gsCode (rsStatus rich) == 3)
  assert "richError: message" (gsMessage (rsStatus rich) == "bad input")
  assert "richError: 1 detail" (length (rsDetails rich) == 1)

  -- 2. encodeDetails produces valid-looking JSON
  let json = encodeDetails (rsDetails rich)
  assert "encodeDetails: starts with [" (T.isPrefixOf "[" json)
  assert "encodeDetails: ends with ]" (T.isSuffixOf "]" json)
  assert "encodeDetails: contains BadRequest" (T.isInfixOf "BadRequest" json)
  assert "encodeDetails: contains field name" (T.isInfixOf "\"name\"" json)
  assert "encodeDetails: contains must not be empty" (T.isInfixOf "must not be empty" json)

  -- 3. richErrorResponse produces a GrpcError
  let resp = richErrorResponse rich
  case resp of
    GrpcError status -> do
      assert "richErrorResponse: status code 3" (gsCode status == 3)
      assert "richErrorResponse: message contains details"
        (T.isInfixOf "[details:" (gsMessage status))
      assert "richErrorResponse: message contains BadRequest"
        (T.isInfixOf "BadRequest" (gsMessage status))
    _ -> error "FAIL: richErrorResponse should produce GrpcError"

  -- 4. All detail types encode properly
  let allDetails =
        [ DetailBadRequest $ BadRequest [FieldViolation "f" "d"]
        , DetailDebugInfo $ DebugInfo ["frame1", "frame2"] "crash"
        , DetailRetryInfo $ RetryInfo 5000
        , DetailHelp $ Help [HelpLink "docs" "https://example.com"]
        , DetailResource $ ResourceInfo "bucket" "my-bucket" "admin" "not found"
        ]
  let allJson = encodeDetails allDetails
  assert "all details: contains DebugInfo" (T.isInfixOf "DebugInfo" allJson)
  assert "all details: contains RetryInfo" (T.isInfixOf "RetryInfo" allJson)
  assert "all details: contains retry_delay_ms" (T.isInfixOf "\"retry_delay_ms\":5000" allJson)
  assert "all details: contains Help" (T.isInfixOf "Help" allJson)
  assert "all details: contains ResourceInfo" (T.isInfixOf "ResourceInfo" allJson)
  assert "all details: contains resource_type" (T.isInfixOf "\"resource_type\":\"bucket\"" allJson)
  assert "all details: contains url" (T.isInfixOf "https://example.com" allJson)

  -- 5. Empty details list produces no detail annotation
  let emptyRich = richError (grpcNotFound "nope") []
  let emptyResp = richErrorResponse emptyRich
  case emptyResp of
    GrpcError status -> do
      assert "empty details: status code 5" (gsCode status == 5)
      assert "empty details: message unchanged" (gsMessage status == "nope")
    _ -> error "FAIL: empty richErrorResponse should produce GrpcError"

  -- 6. JSON escaping works
  let escapeDetail = DetailDebugInfo $ DebugInfo [] "line1\nline2\ttab\"quote"
  let escapeJson = encodeDetails [escapeDetail]
  assert "json escape: newline escaped" (T.isInfixOf "\\n" escapeJson)
  assert "json escape: tab escaped" (T.isInfixOf "\\t" escapeJson)
  assert "json escape: quote escaped" (T.isInfixOf "\\\"quote" escapeJson)

  -- 7. Eq and Show instances work
  let rich2 = richError (grpcInvalidArgument "bad input")
        [ DetailBadRequest $ BadRequest
            [ FieldViolation "name" "must not be empty"
            , FieldViolation "age" "must be positive"
            ]
        ]
  assert "Eq: identical RichGrpcStatus" (rich == rich2)
  assert "Show: non-empty" (not (null (show rich)))


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "tower-grpc tests:\n"
  testCodec
  testCodecEdgeCases
  testStatus
  testStatusExhaustive
  testServer
  testServerEdgeCases
  testStreaming
  testClientStreaming
  testBidiStreaming
  testMultiplex
  testReflection
  testHealthCheck
  testHealthCheckExtended
  testCompression
  testClientStreamingExtended
  testBidiStreamingExtended
  testGrpcWeb
  testErrorDetails
  putStrLn "\nAll tower-grpc tests passed."
