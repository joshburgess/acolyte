{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (lookup)
import Network.HTTP.Types (status200, status204, status408, methodGet, methodPost, statusCode)

import Tower
import Tower.Http
import Http.Core


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- | Make a simple test service that returns 200 with "ok".
echoService :: Service IO (Request ByteString) (Response ByteString)
echoService = Service $ \_ -> pure (ok [] "ok")


-- | Make a test request.
mkReq :: ByteString -> ByteString -> IO (Request ByteString)
mkReq method path = do
  exts <- emptyExtensions
  pure Request
    { requestMethod     = method
    , requestPathRaw    = path
    , requestPath       = []
    , requestQuery      = []
    , requestHeaders    = []
    , requestBody       = ""
    , requestExtensions = exts
    }


-- ===================================================================
-- SecureHeaders tests
-- ===================================================================

testSecureHeaders :: IO ()
testSecureHeaders = do
  let svc = echoService |> secureHeadersLayer defaultSecureHeaders
  req <- mkReq methodGet "/test"
  resp <- runService svc req

  let hdrs = responseHeaders resp
  assert "adds X-Content-Type-Options" (lookup "X-Content-Type-Options" hdrs == Just "nosniff")
  assert "adds X-Frame-Options" (lookup "X-Frame-Options" hdrs == Just "DENY")
  assert "adds Referrer-Policy" (lookup "Referrer-Policy" hdrs == Just "strict-origin-when-cross-origin")
  assert "adds CSP" (lookup "Content-Security-Policy" hdrs == Just "default-src 'self'")
  assert "no HSTS by default" (lookup "Strict-Transport-Security" hdrs == Nothing)

  -- With HSTS enabled
  let cfg = defaultSecureHeaders { strictTransportSecurity = Just "max-age=63072000" }
      svc2 = echoService |> secureHeadersLayer cfg
  resp2 <- runService svc2 req
  let hdrs2 = responseHeaders resp2
  assert "HSTS when configured" (lookup "Strict-Transport-Security" hdrs2 == Just "max-age=63072000")


-- ===================================================================
-- RequestId tests
-- ===================================================================

testRequestId :: IO ()
testRequestId = do
  ridLayer <- requestIdLayer
  let svc = echoService |> ridLayer
  req <- mkReq methodGet "/test"

  -- First request gets an ID
  resp1 <- runService svc req
  let hdrs1 = responseHeaders resp1
  assert "adds X-Request-Id header" (lookup "X-Request-Id" hdrs1 /= Nothing)

  -- Second request gets a different ID
  req2 <- mkReq methodGet "/test2"
  resp2 <- runService svc req2
  let rid1 = lookup "X-Request-Id" (responseHeaders resp1)
      rid2 = lookup "X-Request-Id" (responseHeaders resp2)
  assert "sequential IDs differ" (rid1 /= rid2)

  -- Existing X-Request-Id is preserved
  exts <- emptyExtensions
  let req3 = Request
        { requestMethod = methodGet
        , requestPathRaw = "/test3"
        , requestPath = []
        , requestQuery = []
        , requestHeaders = [("X-Request-Id", "custom-123")]
        , requestBody = ""
        , requestExtensions = exts
        }
  resp3 <- runService svc req3
  assert "preserves existing ID" (lookup "X-Request-Id" (responseHeaders resp3) == Just "custom-123")

  -- RequestId stored in extensions
  exts4 <- emptyExtensions
  let checkExtSvc = Service $ \req -> do
        mRid <- lookupExtension @RequestId (requestExtensions req)
        case mRid of
          Just (RequestId rid) -> pure (ok [] rid)
          Nothing              -> pure (ok [] "no-id")
      svc4 = checkExtSvc |> ridLayer
      req4 = Request
        { requestMethod = methodGet
        , requestPathRaw = "/ext"
        , requestPath = []
        , requestQuery = []
        , requestHeaders = []
        , requestBody = ""
        , requestExtensions = exts4
        }
  resp4 <- runService svc4 req4
  assert "RequestId in extensions" (responseBody resp4 /= "no-id")


-- ===================================================================
-- Trace tests
-- ===================================================================

testTrace :: IO ()
testTrace = do
  ref <- newIORef ([] :: [TraceEntry])
  let svc = echoService |> traceLayer (\e -> modifyIORef' ref (++ [e]))
  req <- mkReq methodPost "/users"

  _ <- runService svc req
  entries <- readIORef ref

  assert "trace captures one entry" (length entries == 1)
  let e = head entries
  assert "trace: method" (traceMethod e == methodPost)
  assert "trace: path" (tracePath e == "/users")
  assert "trace: status 200" (traceStatus e == 200)


-- ===================================================================
-- Composition test: all three layers stacked
-- ===================================================================

testComposition :: IO ()
testComposition = do
  ref <- newIORef ([] :: [TraceEntry])
  ridLayer <- requestIdLayer
  let svc = echoService
            |> ridLayer
            |> traceLayer (\e -> modifyIORef' ref (++ [e]))
            |> secureHeadersLayer defaultSecureHeaders
  req <- mkReq methodGet "/health"
  resp <- runService svc req

  let hdrs = responseHeaders resp
  assert "composed: has security headers" (lookup "X-Frame-Options" hdrs == Just "DENY")
  assert "composed: has request ID" (lookup "X-Request-Id" hdrs /= Nothing)

  entries <- readIORef ref
  assert "composed: trace fired" (length entries == 1)


-- ===================================================================
-- CORS tests
-- ===================================================================

testCors :: IO ()
testCors = do
  let svc = echoService |> corsLayer permissiveCors

  -- Preflight OPTIONS -> 204 with CORS headers
  exts1 <- emptyExtensions
  let optReq = Request
        { requestMethod = "OPTIONS"
        , requestPathRaw = "/test"
        , requestPath = ["test"]
        , requestQuery = []
        , requestHeaders = [("origin", "http://example.com")]
        , requestBody = ""
        , requestExtensions = exts1
        }
  resp1 <- runService svc optReq
  assert "CORS: preflight 204" (statusCode (responseStatus resp1) == 204)
  assert "CORS: Allow-Origin on preflight" (lookup "Access-Control-Allow-Origin" (responseHeaders resp1) == Just "*")
  assert "CORS: Allow-Methods on preflight" (lookup "Access-Control-Allow-Methods" (responseHeaders resp1) /= Nothing)

  -- Normal GET -> 200 with CORS headers added
  req2 <- mkReq methodGet "/test"
  resp2 <- runService svc req2
  assert "CORS: normal 200" (statusCode (responseStatus resp2) == 200)
  assert "CORS: Allow-Origin on GET" (lookup "Access-Control-Allow-Origin" (responseHeaders resp2) == Just "*")


-- ===================================================================
-- Compression tests
-- ===================================================================

testCompression :: IO ()
testCompression = do
  -- Big body service (above min threshold)
  let bigBody = BS.replicate 2000 65  -- 2000 'A's
      bigSvc = Service $ \_ -> pure (ok [("Content-Type", "text/plain")] bigBody)
      svc = bigSvc |> compressionLayer defaultCompression

  -- With Accept-Encoding: gzip
  exts1 <- emptyExtensions
  let req1 = Request
        { requestMethod = "GET"
        , requestPathRaw = "/big"
        , requestPath = ["big"]
        , requestQuery = []
        , requestHeaders = [("accept-encoding", "gzip, deflate")]
        , requestBody = ""
        , requestExtensions = exts1
        }
  resp1 <- runService svc req1
  assert "compression: Content-Encoding gzip" (lookup "Content-Encoding" (responseHeaders resp1) == Just "gzip")
  assert "compression: body smaller" (BS.length (responseBody resp1) < 2000)

  -- Without Accept-Encoding -> no compression
  req2 <- mkReq methodGet "/big"
  resp2 <- runService svc req2
  assert "compression: no encoding without header" (lookup "Content-Encoding" (responseHeaders resp2) == Nothing)
  assert "compression: body unchanged" (BS.length (responseBody resp2) == 2000)

  -- Small body -> skip compression
  let smallSvc = echoService |> compressionLayer defaultCompression
  exts3 <- emptyExtensions
  let req3 = Request
        { requestMethod = "GET"
        , requestPathRaw = "/small"
        , requestPath = ["small"]
        , requestQuery = []
        , requestHeaders = [("accept-encoding", "gzip")]
        , requestBody = ""
        , requestExtensions = exts3
        }
  resp3 <- runService smallSvc req3
  assert "compression: skip small body" (lookup "Content-Encoding" (responseHeaders resp3) == Nothing)


-- ===================================================================
-- Timeout tests
-- ===================================================================

testTimeout :: IO ()
testTimeout = do
  -- Fast handler: completes within timeout
  let fastSvc :: Service IO (Request ByteString) (Response ByteString)
      fastSvc = Service $ \_ -> pure (ok [] "fast")
      svc1 = fastSvc |> timeoutLayer 1000000  -- 1 second
  req1 <- mkReq methodGet "/fast"
  resp1 <- runService svc1 req1
  assert "timeout: fast handler succeeds" (statusCode (responseStatus resp1) == 200)
  assert "timeout: fast body" (responseBody resp1 == "fast")

  -- Slow handler: exceeds timeout
  let slowSvc :: Service IO (Request ByteString) (Response ByteString)
      slowSvc = Service $ \_ -> do
        threadDelay 2000000  -- 2 seconds
        pure (ok [] "slow")
      svc2 = slowSvc |> timeoutLayer 200000  -- 200ms timeout
  req2 <- mkReq methodGet "/slow"
  resp2 <- runService svc2 req2
  assert "timeout: slow handler -> 408" (statusCode (responseStatus resp2) == 408)


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "tower-http tests:"
  putStrLn ""
  putStrLn "SecureHeaders:"
  testSecureHeaders
  putStrLn ""
  putStrLn "RequestId:"
  testRequestId
  putStrLn ""
  putStrLn "Trace:"
  testTrace
  putStrLn ""
  putStrLn "Composition:"
  testComposition
  putStrLn ""
  putStrLn "CORS:"
  testCors
  putStrLn ""
  putStrLn "Compression:"
  testCompression
  putStrLn ""
  putStrLn "Timeout:"
  testTimeout
  putStrLn ""
  putStrLn "All tower-http tests passed."
