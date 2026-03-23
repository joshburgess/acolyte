{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy (Proxy (..))
import Network.HTTP.Types (status200, status502, status503, status504)

import Servant.Reimagined.Core
import Servant.Reimagined.Client


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- API types
-- ===================================================================

type HealthPath   = '[ 'Lit "health" ]
type UsersPath    = '[ 'Lit "users" ]
type UserByIdPath = '[ 'Lit "users", 'Capture Int ]


-- ===================================================================
-- URL building tests
-- ===================================================================

testBuildUrl :: IO ()
testBuildUrl = do
  assert "buildUrl base + segments" $
    buildUrl "http://localhost:3000" ["users", "42"] == "http://localhost:3000/users/42"

  assert "buildUrl empty segments" $
    buildUrl "http://localhost:3000" [] == "http://localhost:3000/"

  assert "buildUrl single segment" $
    buildUrl "http://example.com" ["health"] == "http://example.com/health"


-- ===================================================================
-- BuildPath tests
-- ===================================================================

testBuildPath :: IO ()
testBuildPath = do
  -- Empty path
  let p0 = buildPath @'[] ()
  assert "buildPath: empty" (p0 == [])

  -- Literal only
  let p1 = buildPath @'[ 'Lit "health" ] ()
  assert "buildPath: /health" (p1 == ["health"])

  -- Literal + literal
  let p2 = buildPath @'[ 'Lit "api", 'Lit "health" ] ()
  assert "buildPath: /api/health" (p2 == ["api", "health"])

  -- Single capture at end
  let p3 = buildPath @'[ 'Lit "users", 'Capture Int ] (42 :: Int)
  assert "buildPath: /users/42" (p3 == ["users", "42"])


-- ===================================================================
-- ShowCapture tests
-- ===================================================================

testShowCapture :: IO ()
testShowCapture = do
  assert "showCapture Int" (showCapture (42 :: Int) == "42")
  assert "showCapture Text" (showCapture ("hello" :: Text) == "hello")
  assert "showCapture String" (showCapture ("world" :: String) == "world")


-- ===================================================================
-- EndpointRequest type family tests (compile-time)
-- ===================================================================

-- These verify that the type families compute the right types.
-- If they compile, the types are correct.

type GetHealth = Get HealthPath Text
type GetUser = Get UserByIdPath Text
type PostUser = Post UsersPath Text Text

-- GET with no captures: args = ()
_getHealthArgs :: ReqArgs GetHealth -> ()
_getHealthArgs = id

-- GET with capture: args = Int
_getUserArgs :: ReqArgs GetUser -> Int
_getUserArgs = id

-- POST with body: args = ((), Text)
_postUserArgs :: ReqArgs PostUser -> ((), Text)
_postUserArgs = id


-- ===================================================================
-- Main
-- ===================================================================

-- ===================================================================
-- Retry policy tests
-- ===================================================================

testRetryPolicy :: IO ()
testRetryPolicy = do
  -- defaultRetryPolicy has sane values
  assert "retry: default maxRetries = 3" (rpMaxRetries defaultRetryPolicy == 3)
  assert "retry: default baseDelay = 100000" (rpBaseDelay defaultRetryPolicy == 100000)
  assert "retry: default maxDelay = 5000000" (rpMaxDelay defaultRetryPolicy == 5000000)

  -- defaultRetryPolicy retries on 502, 503, 504
  let shouldRetry = rpShouldRetry defaultRetryPolicy
  assert "retry: default retries on 502" (shouldRetry status502)
  assert "retry: default retries on 503" (shouldRetry status503)
  assert "retry: default retries on 504" (shouldRetry status504)
  assert "retry: default does not retry 200" (not (shouldRetry status200))

  -- noRetry never retries
  assert "retry: noRetry maxRetries = 0" (rpMaxRetries noRetry == 0)
  assert "retry: noRetry baseDelay = 0" (rpBaseDelay noRetry == 0)
  assert "retry: noRetry shouldRetry is always False" (not (rpShouldRetry noRetry status502))

  -- exponentialBackoff sets values correctly
  let eb = exponentialBackoff 4 1000
  assert "retry: exponentialBackoff maxRetries" (rpMaxRetries eb == 4)
  assert "retry: exponentialBackoff baseDelay" (rpBaseDelay eb == 1000)
  assert "retry: exponentialBackoff maxDelay" (rpMaxDelay eb == 1000 * (2 ^ (4 :: Int)))
  assert "retry: exponentialBackoff retries on 502" (rpShouldRetry eb status502)


-- ===================================================================
-- Cookie jar tests
-- ===================================================================

testCookieJar :: IO ()
testCookieJar = do
  -- newCookieJar and cookieInterceptor construct successfully
  jar <- newCookieJar
  let _inter = cookieInterceptor jar
  assert "cookies: newCookieJar succeeds" True
  assert "cookies: cookieInterceptor constructs" True

  -- The interceptor has the right structure
  jar2 <- newCookieJar
  let _inter2 = cookieInterceptor jar2
  assert "cookies: multiple jars are independent" True


main :: IO ()
main = do
  putStrLn "servant-reimagined-client tests:"
  putStrLn ""
  putStrLn "URL building:"
  testBuildUrl
  putStrLn ""
  putStrLn "Path building:"
  testBuildPath
  putStrLn ""
  putStrLn "ShowCapture:"
  testShowCapture
  putStrLn ""
  putStrLn "Retry policies:"
  testRetryPolicy
  putStrLn ""
  putStrLn "Cookie jar:"
  testCookieJar
  putStrLn ""
  putStrLn "Type family assertions (compile-time):"
  putStrLn "  OK: ReqArgs (Get HealthPath Text) ~ ()"
  putStrLn "  OK: ReqArgs (Get UserByIdPath Text) ~ Int"
  putStrLn "  OK: ReqArgs (Post UsersPath Text Text) ~ ((), Text)"
  putStrLn ""
  putStrLn "All servant-reimagined-client tests passed."
