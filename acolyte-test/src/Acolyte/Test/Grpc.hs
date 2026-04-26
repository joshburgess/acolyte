{-# LANGUAGE OverloadedStrings #-}
-- | gRPC testing utilities for acolyte APIs.
--
-- Test gRPC services by dispatching requests directly through the tower
-- Service -- no network, no HTTP/2, no ports. Fast and deterministic.
--
-- @
-- import Acolyte.Test.Grpc
--
-- test = do
--   let svc = grpcServer myServiceMap
--   resp <- grpcCall svc "myapp.MySvc" "SayHello" reqBytes
--   resp `shouldHaveGrpcStatus` 0
--   grpcResponseBody resp `shouldBe` expectedBytes
-- @
module Acolyte.Test.Grpc
  ( -- * Making gRPC requests
    grpcCall
    -- * Response assertions
  , shouldHaveGrpcStatus
  , shouldHaveGrpcBody
  , grpcResponseBody
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Tower.Service (Service (..))
import Http.Core
import Http.Core.Body (Body, fromBytes, bodyToStrict)
import Tower.Grpc.Codec (encodeMessage, decodeMessage, GrpcMessage(..))


-- ===================================================================
-- Making gRPC requests
-- ===================================================================

-- | Send a gRPC request to a tower Service (no network).
--
-- Constructs a proper gRPC request with:
--
-- * Method: POST
-- * Content-Type: application\/grpc+proto
-- * Path: \/service\/method
-- * Body: payload wrapped in gRPC length-prefixed framing
--
-- Returns the raw HTTP Response (with Body).
grpcCall
  :: Service IO (Request Body) (Response Body)
  -> Text         -- ^ Service name (e.g., "myapp.Greeter")
  -> Text         -- ^ Method name (e.g., "SayHello")
  -> ByteString   -- ^ Request payload bytes (will be gRPC-framed)
  -> IO (Response Body)
grpcCall svc service method payload = do
  exts <- emptyExtensions
  let path = "/" <> TE.encodeUtf8 service <> "/" <> TE.encodeUtf8 method
      framedBody = encodeMessage payload
      segments = filter (/= "") $ T.splitOn "/" (TE.decodeUtf8 path)
      req = Request
        { requestMethod     = "POST"
        , requestPathRaw    = path
        , requestPath       = segments
        , requestQuery      = []
        , requestHeaders    = [("content-type", "application/grpc+proto")]
        , requestBody       = fromBytes framedBody
        , requestExtensions = exts
        }
  runService svc req


-- ===================================================================
-- Response assertions
-- ===================================================================

-- | Assert that the response has the expected gRPC status code.
--
-- Checks the @grpc-status@ header/trailer value. gRPC status 0 means OK.
shouldHaveGrpcStatus :: Response Body -> Int -> IO ()
shouldHaveGrpcStatus resp expected =
  case lookup "grpc-status" (responseHeaders resp) of
    Just actual ->
      let code = read (BS8.unpack actual) :: Int
      in if code == expected
         then pure ()
         else error $
           "Expected grpc-status " ++ show expected
           ++ " but got " ++ show code
    Nothing -> error
      "grpc-status header/trailer not found in response"


-- | Assert that the gRPC response body (after removing framing) equals
-- the expected bytes.
shouldHaveGrpcBody :: Response Body -> ByteString -> IO ()
shouldHaveGrpcBody resp expected = do
  body <- bodyToStrict (responseBody resp)
  let payload = stripGrpcFraming body
  if payload == expected
    then pure ()
    else error $
      "Expected gRPC body " ++ show expected
      ++ " but got " ++ show payload


-- | Extract the payload from a gRPC-framed response body.
--
-- Strips the 5-byte gRPC length-prefixed framing header and returns
-- the raw message bytes. Returns empty bytes if the body is empty
-- or cannot be decoded.
grpcResponseBody :: Response Body -> IO ByteString
grpcResponseBody resp = do
  body <- bodyToStrict (responseBody resp)
  pure (stripGrpcFraming body)


-- ===================================================================
-- Internal helpers
-- ===================================================================

-- | Strip gRPC length-prefixed framing from a ByteString.
stripGrpcFraming :: ByteString -> ByteString
stripGrpcFraming bs
  | BS.null bs = BS.empty
  | otherwise = case decodeMessage bs of
      Just (msg, _) -> gmPayload msg
      Nothing       -> bs  -- fallback: return raw bytes
