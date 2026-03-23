{-# LANGUAGE OverloadedStrings #-}
-- | gRPC message compression support.
--
-- Implements gzip compression/decompression for gRPC messages.
-- The gRPC wire format uses a 1-byte compression flag in the
-- 5-byte message header to indicate whether the payload is compressed.
--
-- When the compressed flag is 1, the payload is gzip-compressed.
-- The @grpc-encoding@ header indicates the compression algorithm.
module Tower.Grpc.Compression
  ( -- * Compression types
    GrpcCompression (..)
    -- * Compress / decompress payloads
  , compressMessage
  , decompressMessage
    -- * gRPC framing with compression
  , encodeMessageCompressed
    -- * Body-level helpers
  , decompressGrpcBody
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as BuilderEx
import qualified Codec.Compression.GZip as GZip

import Tower.Grpc.Codec (GrpcMessage (..), decodeMessages)


-- | Compression algorithm for gRPC messages.
data GrpcCompression = NoCompression | Gzip
  deriving (Show, Eq)


-- | Compress a payload with gzip.
compressMessage :: ByteString -> ByteString
compressMessage = LBS.toStrict . GZip.compress . LBS.fromStrict


-- | Decompress a gzip-compressed payload.
decompressMessage :: ByteString -> ByteString
decompressMessage = LBS.toStrict . GZip.decompress . LBS.fromStrict


-- | Encode a single message into gRPC wire format with gzip compression.
--
-- Sets the compressed flag to 1 and gzip-compresses the payload.
encodeMessageCompressed :: ByteString -> ByteString
encodeMessageCompressed payload =
  let compressed = compressMessage payload
      len = BS.length compressed
  in LBS.toStrict
     $ BuilderEx.toLazyByteStringWith
         (BuilderEx.safeStrategy (5 + len) 128)
         LBS.empty
     $ Builder.word8 0x01
       <> Builder.word32BE (fromIntegral len)
       <> Builder.byteString compressed


-- | Decompress all compressed messages in a gRPC body.
--
-- Decodes all length-prefixed messages, decompresses any that have
-- the compressed flag set, and re-encodes them as uncompressed messages.
-- This allows the rest of the pipeline to work with uncompressed data.
decompressGrpcBody :: ByteString -> ByteString
decompressGrpcBody body =
  let msgs = decodeMessages body
      decompress msg
        | gmCompressed msg = decompressMessage (gmPayload msg)
        | otherwise        = gmPayload msg
      -- Re-encode as uncompressed framed messages
      reEncode payload =
        let len = BS.length payload
        in LBS.toStrict
           $ BuilderEx.toLazyByteStringWith
               (BuilderEx.safeStrategy (5 + len) 128)
               LBS.empty
           $ Builder.word8 0x00
             <> Builder.word32BE (fromIntegral len)
             <> Builder.byteString payload
  in BS.concat (map (reEncode . decompress) msgs)
