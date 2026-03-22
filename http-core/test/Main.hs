{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Http.Core
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


-- ===================================================================
-- Test helpers
-- ===================================================================

assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- 1. Extensions: typed heterogeneous map
-- ===================================================================

newtype UserId = UserId Int deriving (Eq, Show)
newtype UserName = UserName String deriving (Eq, Show)

testExtensions :: IO ()
testExtensions = do
  exts <- emptyExtensions

  -- Empty lookup returns Nothing
  r1 <- lookupExtension @UserId exts
  assert "empty lookup returns Nothing" (r1 == Nothing)

  -- Insert and lookup
  insertExtension (UserId 42) exts
  r2 <- lookupExtension @UserId exts
  assert "insert then lookup succeeds" (r2 == Just (UserId 42))

  -- Different type returns Nothing
  r3 <- lookupExtension @UserName exts
  assert "wrong type returns Nothing" (r3 == Nothing)

  -- Multiple types coexist
  insertExtension (UserName "alice") exts
  r4 <- lookupExtension @UserId exts
  r5 <- lookupExtension @UserName exts
  assert "multiple types: UserId still there" (r4 == Just (UserId 42))
  assert "multiple types: UserName found" (r5 == Just (UserName "alice"))

  -- Overwrite same type
  insertExtension (UserId 99) exts
  r6 <- lookupExtension @UserId exts
  assert "overwrite replaces value" (r6 == Just (UserId 99))

  -- hasExtension
  h1 <- hasExtension @UserId exts
  h2 <- hasExtension @Bool exts
  assert "hasExtension: present" h1
  assert "hasExtension: absent" (not h2)

  -- deleteExtension
  deleteExtension @UserId exts
  r7 <- lookupExtension @UserId exts
  assert "delete removes value" (r7 == Nothing)

  -- UserName still there after deleting UserId
  r8 <- lookupExtension @UserName exts
  assert "delete doesn't affect other types" (r8 == Just (UserName "alice"))


-- ===================================================================
-- 2. Request construction and splitting
-- ===================================================================

testRequest :: IO ()
testRequest = do
  req <- defaultRequest
  assert "default method is GET" (requestMethod req == methodGet)
  assert "default path is empty" (requestPath req == [])
  assert "default body is empty" (requestBody req == BS.empty)

  -- splitRequest preserves fields
  let (parts, body) = splitRequest req
  assert "split: method preserved" (rpMethod parts == methodGet)
  assert "split: body extracted" (body == BS.empty)

  -- Custom request
  exts <- emptyExtensions
  let req2 = Request
        { requestMethod     = methodPost
        , requestPathRaw    = "/users/42"
        , requestPath       = ["users", "42"]
        , requestQuery      = [("format", Just "json")]
        , requestHeaders    = [("Content-Type", "application/json")]
        , requestBody       = "{\"name\":\"alice\"}" :: ByteString
        , requestExtensions = exts
        }
  let (parts2, body2) = splitRequest req2
  assert "custom: method is POST" (rpMethod parts2 == methodPost)
  assert "custom: path segments" (rpPath parts2 == ["users", "42"])
  assert "custom: body content" (body2 == "{\"name\":\"alice\"}")


-- ===================================================================
-- 3. Response construction
-- ===================================================================

testResponse :: IO ()
testResponse = do
  let r1 = ok [("Content-Type", "text/plain")] "hello"
  assert "ok: status 200" (statusCode (responseStatus r1) == 200)
  assert "ok: body" (responseBody r1 == "hello")

  let r2 = notFound [] "not here"
  assert "notFound: status 404" (statusCode (responseStatus r2) == 404)

  assert "noContent: status 204" (statusCode (responseStatus noContent) == 204)
  assert "noContent: empty body" (responseBody noContent == BS.empty)

  let r3 = serverError [] "oops"
  assert "serverError: status 500" (statusCode (responseStatus r3) == 500)

  let r4 = created [("Location", "/users/1")] "{\"id\":1}"
  assert "created: status 201" (statusCode (responseStatus r4) == 201)

  assert "methodNotAllowed: status 405" (statusCode (responseStatus methodNotAllowed) == 405)

  let r5 = unprocessable [] "validation failed"
  assert "unprocessable: status 422" (statusCode (responseStatus r5) == 422)


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "http-core tests:"
  putStrLn ""
  putStrLn "Extensions:"
  testExtensions
  putStrLn ""
  putStrLn "Request:"
  testRequest
  putStrLn ""
  putStrLn "Response:"
  testResponse
  putStrLn ""
  putStrLn "All http-core tests passed."
