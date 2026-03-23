{-# LANGUAGE OverloadedStrings #-}
-- | Real network gRPC integration tests.
--
-- Tests the full stack: gRPC framing -> tower Service -> HTTP/1.1 transport.
-- Starts a real tower-server on a TCP port, sends HTTP requests with
-- gRPC content-type and framed bodies via http-client, and verifies
-- responses over the wire.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (statusCode)

import Tower.Service (Service (..))
import Http.Core (Request, Response, Body)
import Tower.Server (runServerWithShutdown, defaultConfig)
import Tower.Grpc


-- ===================================================================
-- Test helpers
-- ===================================================================

assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label

testPort :: Int
testPort = 18951

baseUrl :: String
baseUrl = "http://127.0.0.1:" ++ show testPort


-- ===================================================================
-- Test gRPC service
-- ===================================================================

-- | Echo handler: returns "ECHO:" prepended to the input.
echoHandler :: ByteString -> IO (Either GrpcStatus ByteString)
echoHandler payload = pure $ Right ("ECHO:" <> payload)

-- | Fail handler: always returns INTERNAL error.
failHandler :: ByteString -> IO (Either GrpcStatus ByteString)
failHandler _ = pure $ Left (grpcInternal "something broke")

-- | Streaming handler: returns N items.
listHandler :: ByteString -> IO (Either GrpcStatus [ByteString])
listHandler _ = pure $ Right ["item-0", "item-1", "item-2"]

testGrpcService :: Service IO (Request Body) (Response Body)
testGrpcService = grpcServer $ grpcServiceMap
  [ ("test.Echo", "Echo", unaryHandler echoHandler)
  , ("test.Echo", "Fail", unaryHandler failHandler)
  , ("test.Numbers", "List", serverStreamHandler listHandler)
  ]


-- ===================================================================
-- Server lifecycle helpers
-- ===================================================================

-- | Start the gRPC server in a background thread, returning a shutdown action.
startServer :: IO (MVar ())
startServer = do
  shutdownVar <- newEmptyMVar
  _ <- forkIO $ runServerWithShutdown (defaultConfig testPort) testGrpcService shutdownVar
  -- Wait for the server to start listening
  threadDelay 300000  -- 300ms
  pure shutdownVar

-- | Shut down the server and wait for it to drain.
stopServer :: MVar () -> IO ()
stopServer shutdownVar = do
  putMVar shutdownVar ()
  threadDelay 300000  -- 300ms


-- ===================================================================
-- HTTP request helpers
-- ===================================================================

-- | Build a gRPC POST request for the given service/method with a framed body.
grpcRequest :: HC.Manager -> String -> String -> ByteString -> IO (HC.Response LBS.ByteString)
grpcRequest mgr service method payload = do
  let url = baseUrl ++ "/" ++ service ++ "/" ++ method
  initReq <- HC.parseRequest url
  let req = initReq
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyBS (encodeMessage payload)
        , HC.requestHeaders =
            [ ("Content-Type", "application/grpc+proto")
            , ("TE", "trailers")
            ]
        }
  HC.httpLbs req mgr

-- | Send a raw POST with custom content-type.
rawPostRequest :: HC.Manager -> String -> ByteString -> ByteString -> IO (HC.Response LBS.ByteString)
rawPostRequest mgr path ct body = do
  initReq <- HC.parseRequest (baseUrl ++ path)
  let req = initReq
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyBS body
        , HC.requestHeaders = [("Content-Type", ct)]
        }
  HC.httpLbs req mgr

-- | Look up a response header value.
lookupRespHeader :: HC.Response a -> ByteString -> Maybe ByteString
lookupRespHeader resp name =
  lookup (CI.mk name) (HC.responseHeaders resp)


-- ===================================================================
-- Integration tests
-- ===================================================================

testUnaryEcho :: HC.Manager -> IO ()
testUnaryEcho mgr = do
  putStrLn "  Unary echo RPC:"
  resp <- grpcRequest mgr "test.Echo" "Echo" "hello-world"

  -- HTTP status is always 200 for gRPC
  assert "echo: HTTP status 200" (statusCode (HC.responseStatus resp) == 200)

  -- grpc-status trailer should be 0 (OK)
  assert "echo: grpc-status 0" (lookupRespHeader resp "grpc-status" == Just "0")

  -- Decode the gRPC-framed response body
  let respBody = LBS.toStrict (HC.responseBody resp)
  case decodeMessage respBody of
    Just (msg, _) ->
      assert "echo: response payload" (gmPayload msg == "ECHO:hello-world")
    Nothing ->
      error "FAIL: echo: could not decode gRPC response frame"

testUnaryError :: HC.Manager -> IO ()
testUnaryError mgr = do
  putStrLn "  Unary error RPC:"
  resp <- grpcRequest mgr "test.Echo" "Fail" "bad-input"

  assert "fail: HTTP status 200" (statusCode (HC.responseStatus resp) == 200)
  assert "fail: grpc-status 13 (INTERNAL)" (lookupRespHeader resp "grpc-status" == Just "13")
  assert "fail: grpc-message present" (lookupRespHeader resp "grpc-message" == Just "something broke")

testUnimplementedMethod :: HC.Manager -> IO ()
testUnimplementedMethod mgr = do
  putStrLn "  Unimplemented method:"
  resp <- grpcRequest mgr "test.Echo" "DoesNotExist" "x"

  assert "unimplemented: HTTP status 200" (statusCode (HC.responseStatus resp) == 200)
  assert "unimplemented: grpc-status 12" (lookupRespHeader resp "grpc-status" == Just "12")
  assert "unimplemented: grpc-message present" (lookupRespHeader resp "grpc-message" /= Nothing)

testUnimplementedService :: HC.Manager -> IO ()
testUnimplementedService mgr = do
  putStrLn "  Unimplemented service:"
  resp <- grpcRequest mgr "no.Such.Service" "Method" "x"

  assert "unknown service: HTTP status 200" (statusCode (HC.responseStatus resp) == 200)
  assert "unknown service: grpc-status 12" (lookupRespHeader resp "grpc-status" == Just "12")

testWrongContentType :: HC.Manager -> IO ()
testWrongContentType mgr = do
  putStrLn "  Wrong content-type:"
  resp <- rawPostRequest mgr "/test.Echo/Echo" "application/json" "{}"

  -- Should get 415 Unsupported Media Type
  assert "wrong ct: HTTP status 415" (statusCode (HC.responseStatus resp) == 415)

testServerStreaming :: HC.Manager -> IO ()
testServerStreaming mgr = do
  putStrLn "  Server streaming RPC:"
  resp <- grpcRequest mgr "test.Numbers" "List" "go"

  assert "stream: HTTP status 200" (statusCode (HC.responseStatus resp) == 200)
  assert "stream: grpc-status 0" (lookupRespHeader resp "grpc-status" == Just "0")

  let respBody = LBS.toStrict (HC.responseBody resp)
  let msgs = decodeMessages respBody
  assert "stream: 3 messages" (length msgs == 3)
  assert "stream: first payload" (gmPayload (msgs !! 0) == "item-0")
  assert "stream: second payload" (gmPayload (msgs !! 1) == "item-1")
  assert "stream: third payload" (gmPayload (msgs !! 2) == "item-2")

testEmptyPayload :: HC.Manager -> IO ()
testEmptyPayload mgr = do
  putStrLn "  Empty payload:"
  resp <- grpcRequest mgr "test.Echo" "Echo" ""

  assert "empty: HTTP status 200" (statusCode (HC.responseStatus resp) == 200)
  assert "empty: grpc-status 0" (lookupRespHeader resp "grpc-status" == Just "0")

  let respBody = LBS.toStrict (HC.responseBody resp)
  case decodeMessage respBody of
    Just (msg, _) ->
      assert "empty: response is ECHO: with empty input" (gmPayload msg == "ECHO:")
    Nothing ->
      error "FAIL: empty: could not decode gRPC response frame"

testLargePayload :: HC.Manager -> IO ()
testLargePayload mgr = do
  putStrLn "  Large payload (64KB):"
  let bigPayload = BS.replicate (64 * 1024) 0x41  -- 64KB of 'A'
  resp <- grpcRequest mgr "test.Echo" "Echo" bigPayload

  assert "large: HTTP status 200" (statusCode (HC.responseStatus resp) == 200)
  assert "large: grpc-status 0" (lookupRespHeader resp "grpc-status" == Just "0")

  let respBody = LBS.toStrict (HC.responseBody resp)
  case decodeMessage respBody of
    Just (msg, _) ->
      assert "large: response payload size" (BS.length (gmPayload msg) == 5 + 64 * 1024)
      -- "ECHO:" (5 bytes) + 64KB payload
    Nothing ->
      error "FAIL: large: could not decode gRPC response frame"


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "tower-grpc integration tests (real network):\n"

  -- Start server
  putStrLn "Starting gRPC server on port " >> putStr (show testPort) >> putStrLn "..."
  shutdownVar <- startServer
  mgr <- HC.newManager HC.defaultManagerSettings

  -- Run all integration tests
  putStrLn ""
  testUnaryEcho mgr
  putStrLn ""
  testUnaryError mgr
  putStrLn ""
  testUnimplementedMethod mgr
  putStrLn ""
  testUnimplementedService mgr
  putStrLn ""
  testWrongContentType mgr
  putStrLn ""
  testServerStreaming mgr
  putStrLn ""
  testEmptyPayload mgr
  putStrLn ""
  testLargePayload mgr

  -- Shutdown
  putStrLn "\nStopping server..."
  stopServer shutdownVar
  putStrLn "\nAll tower-grpc integration tests passed."
