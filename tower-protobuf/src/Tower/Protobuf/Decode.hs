-- | Protobuf decoding: single-pass wire format parser.
--
-- Provides 'ProtoDecode' for individual value types and 'DecodeError'
-- for structured error reporting. The decoder works directly on strict
-- 'ByteString' with zero-copy slicing for bytes/string fields.
module Tower.Protobuf.Decode
  ( -- * ProtoDecode class
    ProtoDecode (..)
    -- * Raw field parsing
  , RawField (..)
  , parseFields
    -- * Errors
  , DecodeError (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import Tower.Protobuf.Wire


-- ===================================================================
-- Errors
-- ===================================================================

-- | Errors that can occur during protobuf decoding.
data DecodeError
  = UnexpectedWireType !Int !WireType !WireType
    -- ^ field number, expected wire type, actual wire type
  | TruncatedInput
    -- ^ input ended before a complete value could be read
  | InvalidVarint
    -- ^ varint encoding was malformed or overflowed
  | InvalidUtf8
    -- ^ a string field contained invalid UTF-8
  deriving (Show, Eq)


-- ===================================================================
-- Raw field representation
-- ===================================================================

-- | A raw (unparsed) field value as it appears on the wire.
-- Used as an intermediate representation during decoding: parse all
-- tag-value pairs first, then interpret each field.
data RawField
  = RawVarint   !Word64
  | RawFixed64  !Word64
  | RawFixed32  !Word32
  | RawBytes    !ByteString   -- ^ zero-copy slice of the input
  deriving (Show, Eq)


-- | Parse all tag-value pairs from a protobuf message.
-- Returns a list of (field_number, raw_value) pairs in wire order.
-- Unknown wire types cause a 'Left' error; otherwise parsing is
-- lenient (unknown field numbers are preserved).
parseFields :: ByteString -> Either DecodeError [(Int, RawField)]
parseFields = go []
  where
    go !acc bs
      | BS.null bs = Right (reverse acc)
      | otherwise = case decodeTag bs of
          Nothing -> Left TruncatedInput
          Just (fieldNum, wt, rest) -> case parseRawField wt rest of
            Nothing  -> Left TruncatedInput
            Just (raw, rest') -> go ((fieldNum, raw) : acc) rest'

-- | Parse a raw field value given its wire type.
parseRawField :: WireType -> ByteString -> Maybe (RawField, ByteString)
parseRawField WireVarint bs = do
  (val, rest) <- decodeVarint bs
  Just (RawVarint val, rest)
parseRawField WireFixed64 bs = do
  (val, rest) <- decodeFixed64 bs
  Just (RawFixed64 val, rest)
parseRawField WireFixed32 bs = do
  (val, rest) <- decodeFixed32 bs
  Just (RawFixed32 val, rest)
parseRawField WireLengthDelimited bs = do
  (val, rest) <- decodeLengthDelimited bs
  Just (RawBytes val, rest)


-- ===================================================================
-- ProtoDecode class
-- ===================================================================

-- | Class for types that can be decoded from a raw protobuf field.
class ProtoDecode a where
  -- | Decode a value from a 'RawField'.
  protoDecodeValue :: RawField -> Either DecodeError a

-- Int32: varint
instance ProtoDecode Int32 where
  protoDecodeValue (RawVarint w) = Right (fromIntegral w :: Int32)
  protoDecodeValue _             = Left (UnexpectedWireType 0 WireVarint WireLengthDelimited)
  {-# INLINE protoDecodeValue #-}

-- Int64: varint
instance ProtoDecode Int64 where
  protoDecodeValue (RawVarint w) = Right (fromIntegral w :: Int64)
  protoDecodeValue _             = Left (UnexpectedWireType 0 WireVarint WireLengthDelimited)
  {-# INLINE protoDecodeValue #-}

-- Word32: varint
instance ProtoDecode Word32 where
  protoDecodeValue (RawVarint w) = Right (fromIntegral w :: Word32)
  protoDecodeValue _             = Left (UnexpectedWireType 0 WireVarint WireLengthDelimited)
  {-# INLINE protoDecodeValue #-}

-- Word64: varint
instance ProtoDecode Word64 where
  protoDecodeValue (RawVarint w) = Right w
  protoDecodeValue _             = Left (UnexpectedWireType 0 WireVarint WireLengthDelimited)
  {-# INLINE protoDecodeValue #-}

-- Bool: varint, 0 = False, nonzero = True
instance ProtoDecode Bool where
  protoDecodeValue (RawVarint w) = Right (w /= 0)
  protoDecodeValue _             = Left (UnexpectedWireType 0 WireVarint WireLengthDelimited)
  {-# INLINE protoDecodeValue #-}

-- Float: fixed32
instance ProtoDecode Float where
  protoDecodeValue (RawFixed32 w) =
    Right (castWord32ToFloat w)
  protoDecodeValue _ = Left (UnexpectedWireType 0 WireFixed32 WireVarint)
  {-# INLINE protoDecodeValue #-}

-- Double: fixed64
instance ProtoDecode Double where
  protoDecodeValue (RawFixed64 w) =
    Right (castWord64ToDouble w)
  protoDecodeValue _ = Left (UnexpectedWireType 0 WireFixed64 WireVarint)
  {-# INLINE protoDecodeValue #-}

-- Text: length-delimited, UTF-8 decoded
instance ProtoDecode Text where
  protoDecodeValue (RawBytes bs) =
    case TE.decodeUtf8' bs of
      Left _  -> Left InvalidUtf8
      Right t -> Right t
  protoDecodeValue _ = Left (UnexpectedWireType 0 WireLengthDelimited WireVarint)
  {-# INLINE protoDecodeValue #-}

-- ByteString: length-delimited, zero-copy slice
instance ProtoDecode ByteString where
  protoDecodeValue (RawBytes bs) = Right bs
  protoDecodeValue _ = Left (UnexpectedWireType 0 WireLengthDelimited WireVarint)
  {-# INLINE protoDecodeValue #-}
