-- | Protobuf wire format primitives.
--
-- The proto3 wire format uses 5 wire types. Each field on the wire is
-- a tag (field_number << 3 | wire_type) followed by the value.
--
-- This module provides low-level encoding and decoding of varints,
-- fixed-width values, and length-delimited fields.
module Tower.Protobuf.Wire
  ( -- * Wire types
    WireType (..)
    -- * Tags
  , encodeTag
  , decodeTag
    -- * Varint encoding/decoding
  , encodeVarint
  , decodeVarint
    -- * Zigzag encoding (for sint32/sint64)
  , zigzagEncode
  , zigzagDecode
    -- * Fixed-width encoding
  , encodeFixed32
  , encodeFixed64
  , decodeFixed32
  , decodeFixed64
    -- * Float/Double encoding
  , encodeProtoFloat
  , encodeProtoDouble
  , decodeProtoFloat
  , decodeProtoDouble
    -- * Length-delimited
  , encodeLengthDelimited
  , decodeLengthDelimited
    -- * Field skipping
  , skipField
  ) where

import Prelude hiding (encodeFloat, decodeFloat)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Int (Int64)
import Data.Word (Word8, Word32, Word64)
import GHC.Float (castWord32ToFloat, castFloatToWord32,
                  castWord64ToDouble, castDoubleToWord64)


-- | Proto3 wire types.
data WireType
  = WireVarint          -- ^ 0: int32, int64, uint32, uint64, sint32, sint64, bool, enum
  | WireFixed64         -- ^ 1: fixed64, sfixed64, double
  | WireLengthDelimited -- ^ 2: string, bytes, embedded messages, packed repeated
  | WireFixed32         -- ^ 5: fixed32, sfixed32, float
  deriving (Show, Eq)

wireTypeId :: WireType -> Word8
wireTypeId WireVarint          = 0
wireTypeId WireFixed64         = 1
wireTypeId WireLengthDelimited = 2
wireTypeId WireFixed32         = 5

wireTypeFromId :: Word8 -> Maybe WireType
wireTypeFromId 0 = Just WireVarint
wireTypeFromId 1 = Just WireFixed64
wireTypeFromId 2 = Just WireLengthDelimited
wireTypeFromId 5 = Just WireFixed32
wireTypeFromId _ = Nothing


-- ===================================================================
-- Tags
-- ===================================================================

-- | Encode a field tag (field_number << 3 | wire_type).
encodeTag :: Int -> WireType -> B.Builder
encodeTag fieldNum wt =
  encodeVarint (fromIntegral fieldNum `shiftL` 3 .|. fromIntegral (wireTypeId wt))
{-# INLINE encodeTag #-}

-- | Decode a field tag. Returns (field_number, wire_type, remaining bytes).
decodeTag :: ByteString -> Maybe (Int, WireType, ByteString)
decodeTag bs = do
  (val, rest) <- decodeVarint bs
  let wireId = fromIntegral (val .&. 0x07) :: Word8
      fieldNum = fromIntegral (val `shiftR` 3)
  wt <- wireTypeFromId wireId
  Just (fieldNum, wt, rest)
{-# INLINE decodeTag #-}


-- ===================================================================
-- Varint encoding/decoding
-- ===================================================================

-- | Encode a value as a varint (7 bits per byte, MSB continuation).
encodeVarint :: Word64 -> B.Builder
encodeVarint n
  | n < 0x80  = B.word8 (fromIntegral n)
  | otherwise = B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
                <> encodeVarint (n `shiftR` 7)
{-# INLINE encodeVarint #-}

-- | Decode a varint. Returns (value, remaining bytes).
-- Rejects overlong encodings (more than 10 bytes or invalid high bits
-- in the 10th byte).
decodeVarint :: ByteString -> Maybe (Word64, ByteString)
decodeVarint = go 0 0
  where
    go !acc !shft !bs = case BS.uncons bs of
      Nothing -> Nothing
      Just (b, rest) ->
        let val = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shft)
        in if b .&. 0x80 == 0
           then if shft == 63 && (b .&. 0x7E) /= 0
                then Nothing  -- reject overlong encoding at byte 10
                else Just (val, rest)
           else if shft >= 63
                then Nothing  -- overflow protection
                else go val (shft + 7) rest
{-# INLINE decodeVarint #-}


-- ===================================================================
-- Zigzag encoding (for sint32/sint64)
-- ===================================================================

-- | Zigzag-encode a signed integer: maps negatives to odds, positives to evens.
-- This makes small negative numbers small on the wire.
zigzagEncode :: Int64 -> Word64
zigzagEncode n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigzagEncode #-}

-- | Zigzag-decode.
zigzagDecode :: Word64 -> Int64
zigzagDecode n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE zigzagDecode #-}


-- ===================================================================
-- Fixed-width encoding
-- ===================================================================

-- | Encode a 32-bit value in little-endian.
encodeFixed32 :: Word32 -> B.Builder
encodeFixed32 = B.word32LE
{-# INLINE encodeFixed32 #-}

-- | Encode a 64-bit value in little-endian.
encodeFixed64 :: Word64 -> B.Builder
encodeFixed64 = B.word64LE
{-# INLINE encodeFixed64 #-}

-- | Decode a 32-bit little-endian value.
decodeFixed32 :: ByteString -> Maybe (Word32, ByteString)
decodeFixed32 bs
  | BS.length bs < 4 = Nothing
  | otherwise =
    let b0 = fromIntegral (BS.index bs 0) :: Word32
        b1 = fromIntegral (BS.index bs 1) :: Word32
        b2 = fromIntegral (BS.index bs 2) :: Word32
        b3 = fromIntegral (BS.index bs 3) :: Word32
    in Just (b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24),
             BS.drop 4 bs)
{-# INLINE decodeFixed32 #-}

-- | Decode a 64-bit little-endian value.
decodeFixed64 :: ByteString -> Maybe (Word64, ByteString)
decodeFixed64 bs
  | BS.length bs < 8 = Nothing
  | otherwise =
    let readByte i = fromIntegral (BS.index bs i) :: Word64
    in Just ( readByte 0 .|. (readByte 1 `shiftL` 8) .|. (readByte 2 `shiftL` 16)
              .|. (readByte 3 `shiftL` 24) .|. (readByte 4 `shiftL` 32)
              .|. (readByte 5 `shiftL` 40) .|. (readByte 6 `shiftL` 48)
              .|. (readByte 7 `shiftL` 56),
              BS.drop 8 bs)
{-# INLINE decodeFixed64 #-}


-- ===================================================================
-- Float/Double
-- ===================================================================

-- | Encode a Float (as fixed32, little-endian IEEE 754).
encodeProtoFloat :: Float -> B.Builder
encodeProtoFloat = encodeFixed32 . castFloatToWord32
{-# INLINE encodeProtoFloat #-}

-- | Encode a Double (as fixed64, little-endian IEEE 754).
encodeProtoDouble :: Double -> B.Builder
encodeProtoDouble = encodeFixed64 . castDoubleToWord64
{-# INLINE encodeProtoDouble #-}

-- | Decode a Float.
decodeProtoFloat :: ByteString -> Maybe (Float, ByteString)
decodeProtoFloat bs = do
  (w, rest) <- decodeFixed32 bs
  Just (castWord32ToFloat w, rest)
{-# INLINE decodeProtoFloat #-}

-- | Decode a Double.
decodeProtoDouble :: ByteString -> Maybe (Double, ByteString)
decodeProtoDouble bs = do
  (w, rest) <- decodeFixed64 bs
  Just (castWord64ToDouble w, rest)
{-# INLINE decodeProtoDouble #-}


-- ===================================================================
-- Length-delimited
-- ===================================================================

-- | Encode a length-delimited value: varint length + bytes.
encodeLengthDelimited :: ByteString -> B.Builder
encodeLengthDelimited bs =
  encodeVarint (fromIntegral (BS.length bs)) <> B.byteString bs
{-# INLINE encodeLengthDelimited #-}

-- | Decode a length-delimited value. Returns (bytes, remaining).
-- Zero-copy: returns a slice of the input ByteString.
decodeLengthDelimited :: ByteString -> Maybe (ByteString, ByteString)
decodeLengthDelimited bs = do
  (len, rest) <- decodeVarint bs
  let n = fromIntegral len
  if BS.length rest < n
    then Nothing
    else Just (BS.take n rest, BS.drop n rest)
{-# INLINE decodeLengthDelimited #-}


-- ===================================================================
-- Field skipping (for unknown fields)
-- ===================================================================

-- | Skip a field value based on its wire type. Returns remaining bytes.
skipField :: WireType -> ByteString -> Maybe ByteString
skipField WireVarint bs = do
  (_, rest) <- decodeVarint bs
  Just rest
skipField WireFixed64 bs
  | BS.length bs >= 8 = Just (BS.drop 8 bs)
  | otherwise = Nothing
skipField WireLengthDelimited bs = do
  (_, rest) <- decodeLengthDelimited bs
  Just rest
skipField WireFixed32 bs
  | BS.length bs >= 4 = Just (BS.drop 4 bs)
  | otherwise = Nothing
{-# INLINE skipField #-}
