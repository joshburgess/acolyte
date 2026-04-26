-- | gRPC-Web protocol translation layer.
--
-- gRPC-Web allows browser clients (which cannot use HTTP/2 trailers) to
-- communicate with gRPC servers. The key differences from native gRPC:
--
-- * Content-Type: @application\/grpc-web@ or @application\/grpc-web+proto@
-- * Trailers are encoded in the response body (not HTTP/2 trailers) as a
--   1-byte flag (@0x80@) + length-prefixed trailer block appended after
--   the last message
-- * Works over HTTP/1.1 (no HTTP/2 required)
--
-- @
-- import Spire.Layer (applyLayer)
-- import Spire.Grpc.Web (grpcWebLayer)
--
-- -- Wrap your gRPC server to accept gRPC-Web clients:
-- grpcWebServer = applyLayer grpcWebLayer (grpcServer services)
-- @
module Spire.Grpc.Web
  ( grpcWebLayer
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as BuilderEx
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI

import Spire.Layer (Middleware, around)
import Http.Core (Request (..), Response (..), Body, fromBytes, bodyToStrict)


-- | A spire 'Middleware' that translates between gRPC-Web and native gRPC.
--
-- On request: if the content-type starts with @application\/grpc-web@,
-- rewrites it to @application\/grpc@ so the inner service sees a normal
-- gRPC request.
--
-- On response: encodes @grpc-status@ and @grpc-message@ trailers into the
-- response body as a trailer frame (flag @0x80@ + length-prefixed trailer
-- block), and sets the response content-type to @application\/grpc-web@.
--
-- Non-gRPC-Web requests pass through unmodified.
grpcWebLayer :: Middleware IO (Request Body) (Response Body)
grpcWebLayer = around $ \req callInner -> do
  let ct = lookup "content-type" (requestHeaders req)
  case ct of
    Just v | isGrpcWeb v -> do
      -- Rewrite content-type to native gRPC
      let req' = req { requestHeaders = rewriteContentType (requestHeaders req) }
      resp <- callInner req'
      -- Encode trailers into body for gRPC-Web
      encodeGrpcWebResponse resp
    _ -> callInner req


-- | Check if a content-type value indicates gRPC-Web.
isGrpcWeb :: ByteString -> Bool
isGrpcWeb = BS.isPrefixOf "application/grpc-web"


-- | Rewrite @application\/grpc-web*@ content-type to @application\/grpc+proto@.
rewriteContentType
  :: [(CI.CI ByteString, ByteString)]
  -> [(CI.CI ByteString, ByteString)]
rewriteContentType = map rewrite
  where
    rewrite (name, val)
      | name == "content-type" && isGrpcWeb val = (name, "application/grpc+proto")
      | otherwise = (name, val)


-- | Encode a native gRPC response into gRPC-Web format.
--
-- Extracts @grpc-status@ and @grpc-message@ from the response headers,
-- encodes them as a trailer frame appended to the response body, removes
-- them from headers, and sets the content-type to @application\/grpc-web@.
encodeGrpcWebResponse :: Response Body -> IO (Response Body)
encodeGrpcWebResponse resp = do
  bodyBytes <- bodyToStrict (responseBody resp)
  let hdrs = responseHeaders resp
      -- Extract trailer values
      grpcStatus  = lookup "grpc-status" hdrs
      grpcMessage = lookup "grpc-message" hdrs
      -- Build trailer block as "key: value\r\n" pairs
      trailerPairs = maybe [] (\s -> ["grpc-status: " <> s <> "\r\n"]) grpcStatus
                  ++ maybe [] (\m -> ["grpc-message: " <> m <> "\r\n"]) grpcMessage
      trailerBlock = BS.concat trailerPairs
      -- Encode as trailer frame: 0x80 flag + 4-byte big-endian length + data
      trailerFrame = encodeTrailerFrame trailerBlock
      -- Append trailer frame to body
      newBody = bodyBytes <> trailerFrame
      -- Remove trailer headers and set content-type
      newHeaders = setContentType "application/grpc-web+proto"
                 $ removeHeader "grpc-status"
                 $ removeHeader "grpc-message" hdrs
  pure $ resp
    { responseHeaders = newHeaders
    , responseBody    = fromBytes newBody
    }


-- | Encode a trailer block as a gRPC-Web trailer frame.
--
-- Format: 1-byte flag (0x80 = trailer) + 4-byte big-endian length + data.
encodeTrailerFrame :: ByteString -> ByteString
encodeTrailerFrame block =
  let len = BS.length block
  in LBS.toStrict
     $ BuilderEx.toLazyByteStringWith
         (BuilderEx.safeStrategy (5 + len) 64)
         LBS.empty
     $ Builder.word8 0x80
       <> Builder.word32BE (fromIntegral len)
       <> Builder.byteString block


-- | Remove a header by name (case-insensitive).
removeHeader :: CI.CI ByteString -> [(CI.CI ByteString, ByteString)] -> [(CI.CI ByteString, ByteString)]
removeHeader name = filter (\(k, _) -> k /= name)


-- | Set the content-type header, replacing any existing one.
setContentType :: ByteString -> [(CI.CI ByteString, ByteString)] -> [(CI.CI ByteString, ByteString)]
setContentType ct hdrs =
  ("content-type", ct) : removeHeader "content-type" hdrs
