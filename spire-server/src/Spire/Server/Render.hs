-- | HTTP/1.1 response rendering.
module Spire.Server.Render
  ( -- * Full response (strict body)
    renderFull
    -- * Streaming response (chunked transfer encoding)
  , sendResponseStreaming
    -- * Response head
  , renderResponseHead
    -- * Combined (picks strategy based on Body type)
  , sendResponse
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as BuilderEx
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (Status, statusCode, statusMessage, ResponseHeaders, Header)
import Numeric (showHex)

import Http.Core.Body (Body (..), BodyChunk (..), bodyToStrict)


-- | Send a response over a socket, choosing the right strategy.
--
-- * Strict/Empty/File → Content-Length, single write
-- * Stream → chunked transfer encoding, multiple writes
sendResponse :: (ByteString -> IO ()) -> Status -> ResponseHeaders -> Body -> IO ()
sendResponse send status headers body = case body of
  BodyStrict bs -> send (renderFull status headers bs)
  BodyEmpty     -> send (renderFull status headers BS.empty)
  BodyFile _ _  -> do
    bs <- bodyToStrict body
    send (renderFull status headers bs)
  BodyStream pull ->
    sendResponseStreaming send status headers pull


-- | Render a complete response with Content-Length (strict bodies).
--
-- Uses a tuned buffer strategy: 4KB initial allocation covers most
-- HTTP responses in a single chunk, avoiding the double-copy of
-- @LBS.toStrict . Builder.toLazyByteString@.
renderFull :: Status -> ResponseHeaders -> ByteString -> ByteString
renderFull status headers body =
  toStrictBuilder $
    renderStatusLine status
    <> renderHeaders (addContentLength (BS.length body) headers)
    <> Builder.byteString "\r\n"
    <> Builder.byteString body


-- | Send a streaming response using chunked transfer encoding.
--
-- HTTP/1.1 chunked format:
--   {hex-length}\r\n{data}\r\n   (per chunk)
--   0\r\n\r\n                     (final chunk)
sendResponseStreaming
  :: (ByteString -> IO ())  -- ^ Socket send function
  -> Status
  -> ResponseHeaders
  -> IO (Maybe BodyChunk)   -- ^ Pull-based chunk producer
  -> IO ()
sendResponseStreaming send status headers pull = do
  -- Send headers with Transfer-Encoding: chunked
  let hdrs = ("Transfer-Encoding", "chunked") : headers
  send (renderResponseHead status hdrs)

  -- Send chunks
  let loop = do
        mChunk <- pull
        case mChunk of
          Nothing -> do
            -- Final chunk: 0\r\n\r\n
            send "0\r\n\r\n"
          Just (Chunk bs) | BS.null bs -> loop  -- skip empty chunks
          Just (Chunk bs) -> do
            -- {hex-length}\r\n{data}\r\n
            let len = BS8.pack (showHex (BS.length bs) "")
            send (len <> "\r\n" <> bs <> "\r\n")
            loop
          Just ChunkFlush -> loop  -- flush hint — we send immediately anyway
  loop


-- | Render status line + headers (no body, no trailing CRLF for streaming).
renderResponseHead :: Status -> ResponseHeaders -> ByteString
renderResponseHead status headers =
  toStrictBuilder $
    renderStatusLine status
    <> renderHeaders headers
    <> Builder.byteString "\r\n"


-- ===================================================================
-- Internal
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


-- | Convert a Builder to strict ByteString with a tuned buffer strategy.
--
-- Uses a 4KB initial buffer (covers most HTTP headers + small JSON bodies
-- in a single allocation) with small-chunks growth strategy. This avoids
-- the overhead of @LBS.toStrict . Builder.toLazyByteString@ which allocates
-- a lazy bytestring with default 32KB chunks then copies to strict.
toStrictBuilder :: Builder.Builder -> ByteString
toStrictBuilder =
  LBS.toStrict
  . BuilderEx.toLazyByteStringWith
      (BuilderEx.safeStrategy 4096 4096)
      LBS.empty
{-# INLINE toStrictBuilder #-}
