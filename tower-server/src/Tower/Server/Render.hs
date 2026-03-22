-- | HTTP/1.1 response rendering.
--
-- Converts a 'Response Body' into bytes suitable for writing to a socket.
module Tower.Server.Render
  ( renderResponse
  , renderResponseHead
  , renderFull
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (Status, statusCode, statusMessage, ResponseHeaders, Header)

import Http.Core.Body (Body (..), BodyChunk (..), bodyToStrict)


-- | Render a complete HTTP/1.1 response to bytes.
--
-- For strict and empty bodies, renders the full response including
-- Content-Length. For streaming bodies, collects to strict first
-- (a full streaming implementation would use chunked encoding).
renderResponse :: Status -> ResponseHeaders -> Body -> IO ByteString
renderResponse status headers body = case body of
  BodyStrict bs -> pure $ renderFull status headers bs
  BodyEmpty     -> pure $ renderFull status headers BS.empty
  BodyStream _  -> do
    bs <- bodyToStrict body
    pure $ renderFull status headers bs
  BodyFile _ _  -> do
    bs <- bodyToStrict body
    pure $ renderFull status headers bs


-- | Render a complete response with Content-Length.
renderFull :: Status -> ResponseHeaders -> ByteString -> ByteString
renderFull status headers body =
  LBS.toStrict . Builder.toLazyByteString $
    renderStatusLine status
    <> renderHeaders (addContentLength (BS.length body) headers)
    <> Builder.byteString "\r\n"
    <> Builder.byteString body


-- | Render just the status line and headers (for streaming).
renderResponseHead :: Status -> ResponseHeaders -> ByteString
renderResponseHead status headers =
  LBS.toStrict . Builder.toLazyByteString $
    renderStatusLine status
    <> renderHeaders headers
    <> Builder.byteString "\r\n"


-- ===================================================================
-- Internal builders
-- ===================================================================

renderStatusLine :: Status -> Builder.Builder
renderStatusLine status =
  Builder.byteString "HTTP/1.1 "
  <> Builder.intDec (statusCode status)
  <> Builder.byteString " "
  <> Builder.byteString (statusMessage status)
  <> Builder.byteString "\r\n"

renderHeaders :: ResponseHeaders -> Builder.Builder
renderHeaders = foldMap renderHeader

renderHeader :: Header -> Builder.Builder
renderHeader (name, value) =
  Builder.byteString (CI.original name)
  <> Builder.byteString ": "
  <> Builder.byteString value
  <> Builder.byteString "\r\n"

addContentLength :: Int -> ResponseHeaders -> ResponseHeaders
addContentLength len headers
  | any (\(k, _) -> k == "content-length") headers = headers
  | otherwise = ("Content-Length", BS8.pack (show len)) : headers
