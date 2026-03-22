{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import qualified Network.HTTP.Client as HC

import Tower.Service (Service (..))
import Http.Core
import Http.Core.Body
import Tower.Server
import Tower.Server.Parse
import Tower.Server.Render (renderFull)


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- Test service
-- ===================================================================

testService :: Service IO (Request Body) (Response Body)
testService = Service $ \req ->
  let path = requestPath req
      method = requestMethod req
  in case (method, path) of
    ("GET", ["health"]) ->
      pure (Response status200 [("Content-Type", "text/plain")] (fromBytes "ok"))

    ("GET", ["users"]) ->
      pure (Response status200 [("Content-Type", "application/json")] (fromBytes "[\"alice\",\"bob\"]"))

    ("GET", ["users", uid]) ->
      pure (Response status200 [("Content-Type", "application/json")]
        (fromBytes (BS8.pack ("\"user-" ++ T.unpack uid ++ "\""))))

    ("POST", ["echo"]) -> do
      body <- bodyToStrict (requestBody req)
      pure (Response status200 [("Content-Type", "application/octet-stream")] (fromBytes body))

    _ -> pure (Response status404 [] (fromBytes "Not Found"))


-- ===================================================================
-- Parser tests (unit, no network)
-- ===================================================================

testParser :: IO ()
testParser = do
  -- Parse a simple GET request
  let raw = "GET /health HTTP/1.1\r\nHost: localhost\r\nAccept: */*\r\n\r\n"
  case parseRequestHead raw of
    Just (rh, remainder) -> do
      assert "parse: method GET" (rhMethod rh == "GET")
      assert "parse: path /health" (rhPath rh == "/health")
      assert "parse: has Host header" (lookup "host" (rhHeaders rh) == Just "localhost")
      assert "parse: no leftover" (BS.null remainder)
    Nothing -> error "FAIL: parse returned Nothing"

  -- Parse POST with body
  let rawPost = "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\n\r\nhello"
  case parseRequestHead rawPost of
    Just (rh, remainder) -> do
      assert "parse POST: method" (rhMethod rh == "POST")
      assert "parse POST: path" (rhPath rh == "/echo")
      assert "parse POST: content-length header" (lookup "content-length" (rhHeaders rh) == Just "5")
      assert "parse POST: body in remainder" (remainder == "hello")
    Nothing -> error "FAIL: parse POST returned Nothing"


-- ===================================================================
-- Renderer tests (unit, no network)
-- ===================================================================

testRenderer :: IO ()
testRenderer = do
  let resp = renderFull status200 [("Content-Type", "text/plain")] "hello"
  assert "render: starts with HTTP/1.1 200" (BS.isPrefixOf "HTTP/1.1 200" resp)
  assert "render: contains Content-Length" (BS8.isInfixOf "Content-Length: 5" resp)
  assert "render: ends with body" (BS8.isSuffixOf "hello" resp)

  -- Empty response
  let respEmpty = renderFull status204 [] ""
  assert "render 204: no body" (BS8.isSuffixOf "\r\n\r\n" respEmpty)


-- ===================================================================
-- Integration test: start server, make real HTTP requests
-- ===================================================================

testIntegration :: IO ()
testIntegration = do
  let port = 18923  -- use a non-standard port
  shutdownVar <- newEmptyMVar

  -- Start server in background
  _ <- forkIO $ runServerWithShutdown (defaultConfig port) testService shutdownVar

  -- Wait for server to start
  threadDelay 200000  -- 200ms

  mgr <- HC.newManager HC.defaultManagerSettings

  -- GET /health
  req1 <- HC.parseRequest ("http://127.0.0.1:" ++ show port ++ "/health")
  resp1 <- HC.httpLbs req1 mgr
  assert "integration: GET /health -> 200" (HC.responseStatus resp1 == status200)
  assert "integration: GET /health body" (HC.responseBody resp1 == "ok")

  -- GET /users
  req2 <- HC.parseRequest ("http://127.0.0.1:" ++ show port ++ "/users")
  resp2 <- HC.httpLbs req2 mgr
  assert "integration: GET /users -> 200" (HC.responseStatus resp2 == status200)
  assert "integration: GET /users body" (HC.responseBody resp2 == "[\"alice\",\"bob\"]")

  -- GET /users/42
  req3 <- HC.parseRequest ("http://127.0.0.1:" ++ show port ++ "/users/42")
  resp3 <- HC.httpLbs req3 mgr
  assert "integration: GET /users/42 -> 200" (HC.responseStatus resp3 == status200)
  assert "integration: GET /users/42 body" (HC.responseBody resp3 == "\"user-42\"")

  -- POST /echo
  let req4 = (HC.parseRequest_ ("http://127.0.0.1:" ++ show port ++ "/echo"))
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS "echo-this"
        }
  resp4 <- HC.httpLbs req4 mgr
  assert "integration: POST /echo -> 200" (HC.responseStatus resp4 == status200)
  assert "integration: POST /echo body" (HC.responseBody resp4 == "echo-this")

  -- 404
  req5 <- HC.parseRequest ("http://127.0.0.1:" ++ show port ++ "/nope")
  resp5 <- HC.httpLbs req5 mgr
  assert "integration: GET /nope -> 404" (HC.responseStatus resp5 == status404)

  -- Shutdown
  putMVar shutdownVar ()
  threadDelay 300000  -- let it drain


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "tower-server tests:"
  putStrLn ""
  putStrLn "Parser:"
  testParser
  putStrLn ""
  putStrLn "Renderer:"
  testRenderer
  putStrLn ""
  putStrLn "Integration (real HTTP):"
  testIntegration
  putStrLn ""
  putStrLn "All tower-server tests passed."
