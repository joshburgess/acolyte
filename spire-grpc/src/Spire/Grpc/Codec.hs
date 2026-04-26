{-# LANGUAGE OverloadedStrings #-}
-- | gRPC length-prefixed message framing.
--
-- The gRPC wire format wraps each message in a 5-byte header:
--
-- @
-- +----------+----------------+--------------------------------------+
-- | 1 byte   | 4 bytes        | Message-Length bytes                  |
-- | comp flag| msg len (BE)   | message payload                      |
-- +----------+----------------+--------------------------------------+
-- @
--
-- This module provides encoding and decoding of this framing format,
-- independent of any serialization library (protobuf, JSON, etc.).
module Spire.Grpc.Codec
  ( -- * Encoding
    encodeMessage
  , encodeMessages
    -- * Decoding
  , decodeMessage
  , decodeMessages
    -- * Types
  , GrpcMessage (..)
    -- * Internal (re-exported by Compression)
  , decodeWord32BE
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as BuilderEx
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32)
import Data.Bits (shiftL)


-- | A decoded gRPC message with its compression flag.
data GrpcMessage = GrpcMessage
  { gmCompressed :: !Bool
  , gmPayload    :: !ByteString
  } deriving (Show, Eq)


-- | Encode a single uncompressed message into gRPC wire format.
--
-- Prepends the 5-byte header: 0x00 (uncompressed) + big-endian length.
encodeMessage :: ByteString -> ByteString
encodeMessage payload =
  let len = BS.length payload
      -- 5-byte header + payload, single allocation for typical messages
  in LBS.toStrict
     $ BuilderEx.toLazyByteStringWith
         (BuilderEx.safeStrategy (5 + len) 128)
         LBS.empty
     $ Builder.word8 0x00
       <> Builder.word32BE (fromIntegral len)
       <> Builder.byteString payload


-- | Encode multiple messages into a single gRPC wire-format ByteString.
encodeMessages :: [ByteString] -> ByteString
encodeMessages = BS.concat . map encodeMessage


-- | Decode the first gRPC message from a ByteString.
--
-- Returns the decoded message and any remaining bytes, or 'Nothing'
-- if the input is incomplete.
decodeMessage :: ByteString -> Maybe (GrpcMessage, ByteString)
decodeMessage bs
  | BS.length bs < 5 = Nothing
  | otherwise =
    let compressed = BS.index bs 0 /= 0
        len = fromIntegral (decodeWord32BE (BS.take 4 (BS.drop 1 bs)))
        rest = BS.drop 5 bs
    in if BS.length rest < len
       then Nothing
       else Just
         ( GrpcMessage compressed (BS.take len rest)
         , BS.drop len rest
         )


-- | Decode all gRPC messages from a ByteString.
decodeMessages :: ByteString -> [GrpcMessage]
decodeMessages bs
  | BS.null bs = []
  | otherwise = case decodeMessage bs of
      Nothing        -> []
      Just (msg, rest) -> msg : decodeMessages rest


-- | Decode a big-endian Word32 from 4 bytes.
decodeWord32BE :: ByteString -> Word32
decodeWord32BE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word32
      b1 = fromIntegral (BS.index bs 1) :: Word32
      b2 = fromIntegral (BS.index bs 2) :: Word32
      b3 = fromIntegral (BS.index bs 3) :: Word32
  in (b0 `shiftL` 24) + (b1 `shiftL` 16) + (b2 `shiftL` 8) + b3
