{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
    -- * Packed repeated field decoding
  , decodePacked
    -- * Packed/unpacked repeated entry decoding (Generics support)
  , DecodeRepeatedEntry (..)
    -- * Errors
  , DecodeError (..)
  ) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import Tower.Protobuf.Wire
import Tower.Protobuf.Field (SInt32(..), SInt64(..))


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
  | InvalidWireType !Int
    -- ^ wire type id was not one of 0, 1, 2, 5
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
          Nothing ->
            -- Distinguish truncated input from invalid wire type
            case decodeVarint bs of
              Nothing -> Left TruncatedInput
              Just (val, _) ->
                let wireId = fromIntegral (val .&. 0x07) :: Int
                in if wireId `elem` [0, 1, 2, 5]
                   then Left TruncatedInput
                   else Left (InvalidWireType wireId)
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

-- SInt32: varint with zigzag decoding
instance ProtoDecode SInt32 where
  protoDecodeValue (RawVarint w) =
    Right (SInt32 (fromIntegral (zigzagDecode w)))
  protoDecodeValue _ = Left (UnexpectedWireType 0 WireVarint WireLengthDelimited)
  {-# INLINE protoDecodeValue #-}

-- SInt64: varint with zigzag decoding
instance ProtoDecode SInt64 where
  protoDecodeValue (RawVarint w) =
    Right (SInt64 (zigzagDecode w))
  protoDecodeValue _ = Left (UnexpectedWireType 0 WireVarint WireLengthDelimited)
  {-# INLINE protoDecodeValue #-}


-- ===================================================================
-- Packed repeated field decoding
-- ===================================================================

-- | Decode packed repeated scalar values from a 'RawBytes' blob.
-- In proto3, repeated scalar fields may be encoded as a single
-- length-delimited blob containing all values concatenated.
-- Returns 'Left' if the blob cannot be fully consumed.
decodePacked :: ProtoDecode a => (ByteString -> Maybe (a, ByteString)) -> RawField -> Either DecodeError [a]
decodePacked parser (RawBytes bs) = go bs []
  where
    go remaining acc
      | BS.null remaining = Right (reverse acc)
      | otherwise = case parser remaining of
          Just (val, rest) -> go rest (val : acc)
          Nothing -> Left TruncatedInput
decodePacked _ _ = Left (UnexpectedWireType 0 WireLengthDelimited WireVarint)


-- ===================================================================
-- DecodeRepeatedEntry: handles both packed and unpacked entries
-- ===================================================================

-- | Decode a single raw field entry as one or more values of type @a@.
--
-- For packable scalar types (Int32, Int64, Word32, Word64, Bool, Float,
-- Double, SInt32, SInt64), a 'RawBytes' entry is treated as a packed
-- blob containing multiple concatenated values. For non-packable types
-- (Text, ByteString, submessages), 'RawBytes' is decoded as a single
-- value.
--
-- This class is used by the Generics decoder for repeated fields to
-- transparently handle both packed and unpacked wire encodings.
class DecodeRepeatedEntry a where
  decodeRepeatedEntry :: RawField -> Either DecodeError [a]

-- | Default: non-packable types. Decode as a single value.
instance {-# OVERLAPPABLE #-} ProtoDecode a => DecodeRepeatedEntry a where
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

-- Varint-based packable scalars: Int32, Int64, Word32, Word64, Bool

instance DecodeRepeatedEntry Int32 where
  decodeRepeatedEntry (RawBytes bs) = unpackVarints bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

instance DecodeRepeatedEntry Int64 where
  decodeRepeatedEntry (RawBytes bs) = unpackVarints bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

instance DecodeRepeatedEntry Word32 where
  decodeRepeatedEntry (RawBytes bs) = unpackVarints bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

instance DecodeRepeatedEntry Word64 where
  decodeRepeatedEntry (RawBytes bs) = unpackVarints bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

instance DecodeRepeatedEntry Bool where
  decodeRepeatedEntry (RawBytes bs) = unpackVarints bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

-- Fixed32-based: Float
instance DecodeRepeatedEntry Float where
  decodeRepeatedEntry (RawBytes bs) = unpackFixed32s bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

-- Fixed64-based: Double
instance DecodeRepeatedEntry Double where
  decodeRepeatedEntry (RawBytes bs) = unpackFixed64s bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

-- Varint-based with zigzag: SInt32, SInt64
instance DecodeRepeatedEntry SInt32 where
  decodeRepeatedEntry (RawBytes bs) = unpackVarints bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}

instance DecodeRepeatedEntry SInt64 where
  decodeRepeatedEntry (RawBytes bs) = unpackVarints bs
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}


-- ===================================================================
-- Unpacking helpers
-- ===================================================================

-- | Unpack varints from a packed blob, decoding each via 'ProtoDecode'.
unpackVarints :: ProtoDecode a => ByteString -> Either DecodeError [a]
unpackVarints = go []
  where
    go !acc bs
      | BS.null bs = Right (reverse acc)
      | otherwise = case decodeVarint bs of
          Just (w, rest) -> case protoDecodeValue (RawVarint w) of
            Right val -> go (val : acc) rest
            Left err  -> Left err
          Nothing -> Left TruncatedInput
{-# INLINE unpackVarints #-}

-- | Unpack fixed32 values from a packed blob.
unpackFixed32s :: ProtoDecode a => ByteString -> Either DecodeError [a]
unpackFixed32s = go []
  where
    go !acc bs
      | BS.null bs = Right (reverse acc)
      | otherwise = case decodeFixed32 bs of
          Just (w, rest) -> case protoDecodeValue (RawFixed32 w) of
            Right val -> go (val : acc) rest
            Left err  -> Left err
          Nothing -> Left TruncatedInput
{-# INLINE unpackFixed32s #-}

-- | Unpack fixed64 values from a packed blob.
unpackFixed64s :: ProtoDecode a => ByteString -> Either DecodeError [a]
unpackFixed64s = go []
  where
    go !acc bs
      | BS.null bs = Right (reverse acc)
      | otherwise = case decodeFixed64 bs of
          Just (w, rest) -> case protoDecodeValue (RawFixed64 w) of
            Right val -> go (val : acc) rest
            Left err  -> Left err
          Nothing -> Left TruncatedInput
{-# INLINE unpackFixed64s #-}
