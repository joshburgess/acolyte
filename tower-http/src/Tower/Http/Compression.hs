-- | Response compression middleware.
--
-- Compresses response bodies with gzip when the client sends
-- @Accept-Encoding: gzip@. Skips already-compressed responses
-- and small bodies.
module Tower.Http.Compression
  ( -- * Config
    CompressionConfig (..)
  , defaultCompression
    -- * Layer
  , compressionLayer
  ) where

import Codec.Compression.GZip (compress)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS

import Tower (Middleware, middleware)
import Tower.Service (Service (..))
import Http.Core (Request (..), Response (..))


-- | Compression configuration.
data CompressionConfig = CompressionConfig
  { compMinSize :: !Int
    -- ^ Minimum body size to compress (bytes). Default: 860.
    -- Below this, compression overhead exceeds savings.
  } deriving (Show)

-- | Default compression config: minimum body size of 860 bytes.
defaultCompression :: CompressionConfig
defaultCompression = CompressionConfig
  { compMinSize = 860
  }


-- | Gzip compression middleware.
compressionLayer :: CompressionConfig -> Middleware IO (Request ByteString) (Response ByteString)
compressionLayer cfg = middleware $ \(Service inner) -> Service $ \req -> do
  resp <- inner req
  if shouldCompress cfg req resp
    then pure (compressResponse resp)
    else pure resp


shouldCompress :: CompressionConfig -> Request ByteString -> Response ByteString -> Bool
shouldCompress cfg req resp =
  clientAcceptsGzip req
  && BS.length (responseBody resp) >= compMinSize cfg
  && not (isAlreadyCompressed resp)


clientAcceptsGzip :: Request ByteString -> Bool
clientAcceptsGzip req =
  case lookup "accept-encoding" (requestHeaders req) of
    Just ae -> BS8.isInfixOf "gzip" ae
    Nothing -> False


isAlreadyCompressed :: Response ByteString -> Bool
isAlreadyCompressed resp =
  case lookup "Content-Encoding" (responseHeaders resp) of
    Just _ -> True
    Nothing -> False


compressResponse :: Response ByteString -> Response ByteString
compressResponse resp =
  let compressed = LBS.toStrict . compress . LBS.fromStrict $ responseBody resp
      headers = ("Content-Encoding", "gzip")
              : filter (\(k, _) -> k /= "Content-Length") (responseHeaders resp)
  in resp
    { responseBody    = compressed
    , responseHeaders = ("Content-Length", BS8.pack (show (BS.length compressed))) : headers
    }
