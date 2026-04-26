{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (lookup)
import Network.HTTP.Types

import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai (ResponseReceived (..))

import Spire
import Spire.Http (secureHeadersLayer, defaultSecureHeaders)
import Spire.Wai
import Http.Core


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- toWaiApp / fromWaiRequest / toWaiResponse
-- ===================================================================

testToWaiApp :: IO ()
testToWaiApp = do
  let svc :: Service IO (Request ByteString) (Response ByteString)
      svc = Service $ \req -> do
        let body = requestMethod req <> " " <> requestPathRaw req
        pure (ok [("Content-Type", "text/plain")] body)

  let waiApp = toWaiApp svc

  let waiReq = Wai.defaultRequest
        { Wai.requestMethod = methodGet
        , Wai.rawPathInfo   = "/hello"
        , Wai.pathInfo      = ["hello"]
        }

  resultVar <- newEmptyMVar
  _ <- waiApp waiReq $ \waiResp -> do
    putMVar resultVar waiResp
    pure Wai.ResponseReceived
  waiResp <- takeMVar resultVar

  assert "toWaiApp: status 200" (Wai.responseStatus waiResp == status200)
  assert "toWaiApp: has Content-Type" $
    lookup "Content-Type" (Wai.responseHeaders waiResp) == Just "text/plain"


-- ===================================================================
-- fromWaiRequest preserves fields
-- ===================================================================

testFromWaiRequest :: IO ()
testFromWaiRequest = do
  let waiReq = Wai.defaultRequest
        { Wai.requestMethod  = methodPost
        , Wai.rawPathInfo    = "/users/42"
        , Wai.pathInfo       = ["users", "42"]
        , Wai.queryString    = [("format", Just "json")]
        , Wai.requestHeaders = [("Authorization", "Bearer token")]
        }
  req <- fromWaiRequest waiReq

  assert "fromWai: method" (requestMethod req == methodPost)
  assert "fromWai: rawPath" (requestPathRaw req == "/users/42")
  assert "fromWai: path segments" (requestPath req == ["users", "42"])
  assert "fromWai: query" (requestQuery req == [("format", Just "json")])
  assert "fromWai: headers" (lookup "Authorization" (requestHeaders req) == Just "Bearer token")


-- ===================================================================
-- toWaiResponse roundtrip
-- ===================================================================

testToWaiResponse :: IO ()
testToWaiResponse = do
  let resp = ok [("X-Custom", "test")] "hello world"
      waiResp = toWaiResponse resp

  assert "toWaiResp: status 200" (Wai.responseStatus waiResp == status200)
  assert "toWaiResp: custom header" $
    lookup "X-Custom" (Wai.responseHeaders waiResp) == Just "test"


-- ===================================================================
-- fromWaiMiddleware: wrapping WAI middleware as spire Layer
-- ===================================================================

testFromWaiMiddleware :: IO ()
testFromWaiMiddleware = do
  -- A WAI middleware that adds a response header
  let addHeader :: Wai.Middleware
      addHeader app waiReq respond = app waiReq $ \waiResp ->
        respond $ Wai.mapResponseHeaders (("X-WAI-MW", "present") :) waiResp

  let svc :: Service IO (Request ByteString) (Response ByteString)
      svc = Service $ \_ -> pure (ok [] "inner-body")

  let wrappedSvc = svc |> fromWaiMiddleware addHeader

  reqData <- defaultRequest
  resp <- runService wrappedSvc reqData

  -- The WAI middleware should have added the header.
  -- Note: the response body comes through via the inner toWaiApp/toWaiResponse
  -- path, so it's preserved.
  assert "fromWaiMiddleware: header added" $
    lookup "X-WAI-MW" (responseHeaders resp) == Just "present"


-- ===================================================================
-- Integration: spire-http layers + spire-wai adapter
-- ===================================================================

testIntegration :: IO ()
testIntegration = do
  let svc :: Service IO (Request ByteString) (Response ByteString)
      svc = Service $ \_ -> pure (ok [] "ok")

  let fullSvc = svc |> secureHeadersLayer defaultSecureHeaders
      waiApp = toWaiApp fullSvc

  let waiReq = Wai.defaultRequest { Wai.rawPathInfo = "/" }
  resultVar <- newEmptyMVar
  _ <- waiApp waiReq $ \waiResp -> do
    putMVar resultVar waiResp
    pure Wai.ResponseReceived
  waiResp <- takeMVar resultVar

  assert "integration: security headers present" $
    lookup "X-Frame-Options" (Wai.responseHeaders waiResp) == Just "DENY"


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "spire-wai tests:"
  putStrLn ""
  putStrLn "toWaiApp:"
  testToWaiApp
  putStrLn ""
  putStrLn "fromWaiRequest:"
  testFromWaiRequest
  putStrLn ""
  putStrLn "toWaiResponse:"
  testToWaiResponse
  putStrLn ""
  putStrLn "fromWaiMiddleware:"
  testFromWaiMiddleware
  putStrLn ""
  putStrLn "Integration:"
  testIntegration
  putStrLn ""
  putStrLn "All spire-wai tests passed."
