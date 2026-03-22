-- | Testing utilities for servant-reimagined APIs.
--
-- Test APIs by dispatching requests directly through the tower Service
-- — no network, no warp, no ports. Fast and deterministic.
--
-- @
-- import Servant.Reimagined.Test
--
-- test = do
--   let svc = mkServer @MyAPI handlers
--   resp <- request svc "GET" "/health" [] ""
--   resp `shouldHaveStatus` 200
--   resp `shouldHaveBody` "ok"
-- @
module Servant.Reimagined.Test
  ( -- * Making requests
    request
  , get
  , post
  , put_
  , delete_
  , patch
    -- * Response assertions
  , shouldHaveStatus
  , shouldHaveHeader
  , shouldHaveBody
  , shouldHaveJsonBody
  , shouldMatchJson
    -- * Utilities
  , responseBody'
  , responseStatus'
  , responseHeaders'
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Status, statusCode, RequestHeaders, Header, HeaderName)

import qualified Data.Aeson as Aeson

import Tower.Service (Service (..))
import Http.Core


-- ===================================================================
-- Making requests
-- ===================================================================

-- | Send a request to a tower Service (no network).
request
  :: Service IO (Request ByteString) (Response ByteString)
  -> ByteString     -- ^ HTTP method
  -> ByteString     -- ^ Path (e.g., "/users/42")
  -> RequestHeaders -- ^ Headers
  -> ByteString     -- ^ Body
  -> IO (Response ByteString)
request svc method path headers body = do
  exts <- emptyExtensions
  let segments = filter (/= "") $ T.splitOn "/" (TE.decodeUtf8 path)
  let req = Request
        { requestMethod     = method
        , requestPathRaw    = path
        , requestPath       = segments
        , requestQuery      = []
        , requestHeaders    = headers
        , requestBody       = body
        , requestExtensions = exts
        }
  runService svc req


-- | GET request with no body.
get :: Service IO (Request ByteString) (Response ByteString)
    -> ByteString -> IO (Response ByteString)
get svc path = request svc "GET" path [] ""


-- | POST request with JSON body.
post :: Aeson.ToJSON a
     => Service IO (Request ByteString) (Response ByteString)
     -> ByteString -> a -> IO (Response ByteString)
post svc path body = request svc "POST" path
  [("Content-Type", "application/json")]
  (LBS.toStrict (Aeson.encode body))


-- | PUT request with JSON body.
put_ :: Aeson.ToJSON a
     => Service IO (Request ByteString) (Response ByteString)
     -> ByteString -> a -> IO (Response ByteString)
put_ svc path body = request svc "PUT" path
  [("Content-Type", "application/json")]
  (LBS.toStrict (Aeson.encode body))


-- | DELETE request with no body.
delete_ :: Service IO (Request ByteString) (Response ByteString)
        -> ByteString -> IO (Response ByteString)
delete_ svc path = request svc "DELETE" path [] ""


-- | PATCH request with JSON body.
patch :: Aeson.ToJSON a
      => Service IO (Request ByteString) (Response ByteString)
      -> ByteString -> a -> IO (Response ByteString)
patch svc path body = request svc "PATCH" path
  [("Content-Type", "application/json")]
  (LBS.toStrict (Aeson.encode body))


-- ===================================================================
-- Response assertions
-- ===================================================================

-- | Assert response has the expected status code.
shouldHaveStatus :: Response ByteString -> Int -> IO ()
shouldHaveStatus resp expected
  | actual == expected = pure ()
  | otherwise = error $
      "Expected status " ++ show expected ++ " but got " ++ show actual
      ++ "\nBody: " ++ show (responseBody resp)
  where actual = statusCode (responseStatus resp)


-- | Assert response has a specific header with the expected value.
shouldHaveHeader :: Response ByteString -> HeaderName -> ByteString -> IO ()
shouldHaveHeader resp name expected =
  case lookup name (responseHeaders resp) of
    Just actual | actual == expected -> pure ()
    Just actual -> error $
      "Header " ++ show name ++ ": expected " ++ show expected
      ++ " but got " ++ show actual
    Nothing -> error $
      "Header " ++ show name ++ " not found in response"


-- | Assert response body equals the expected bytes.
shouldHaveBody :: Response ByteString -> ByteString -> IO ()
shouldHaveBody resp expected
  | responseBody resp == expected = pure ()
  | otherwise = error $
      "Expected body " ++ show expected
      ++ " but got " ++ show (responseBody resp)


-- | Assert response body decodes to the expected JSON value.
shouldHaveJsonBody :: (Aeson.FromJSON a, Eq a, Show a)
                   => Response ByteString -> a -> IO ()
shouldHaveJsonBody resp expected =
  case Aeson.eitherDecodeStrict' (responseBody resp) of
    Right actual | actual == expected -> pure ()
    Right actual -> error $
      "Expected JSON " ++ show expected ++ " but got " ++ show actual
    Left err -> error $
      "Failed to decode JSON: " ++ err ++ "\nBody: " ++ show (responseBody resp)


-- | Assert response body is valid JSON matching the expected Value.
shouldMatchJson :: Response ByteString -> Aeson.Value -> IO ()
shouldMatchJson resp expected =
  case Aeson.eitherDecodeStrict' (responseBody resp) of
    Right actual | actual == expected -> pure ()
    Right actual -> error $
      "JSON mismatch.\nExpected: " ++ show expected
      ++ "\nActual: " ++ show actual
    Left err -> error $ "Failed to decode JSON: " ++ err


-- ===================================================================
-- Utilities
-- ===================================================================

-- | Extract the status code as Int.
responseStatus' :: Response ByteString -> Int
responseStatus' = statusCode . responseStatus

-- | Extract the body.
responseBody' :: Response ByteString -> ByteString
responseBody' = responseBody

-- | Extract headers.
responseHeaders' :: Response ByteString -> [(HeaderName, ByteString)]
responseHeaders' = responseHeaders
