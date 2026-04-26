{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | GHC Generics integration for automatic protobuf encoding/decoding.
--
-- Define your message as a Haskell record with 'Field' wrappers,
-- derive 'Generic', and write an empty 'ProtoMessage' instance:
--
-- @
-- data User = User
--   { name  :: Field 1 Text
--   , age   :: Field 2 Int32
--   , email :: Field 3 (Maybe Text)
--   } deriving (Generic)
--
-- instance ProtoMessage User
-- @
--
-- Encoding and decoding are then available via 'encode' and 'decode'.
module Spire.Protobuf.Message
  ( ProtoMessage (..)
  , encode
  , decode
  , GProtoEncode (..)
  , GProtoDecode (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)
import Data.Proxy (Proxy(..))

import Spire.Protobuf.Wire
import Spire.Protobuf.Field
import Spire.Protobuf.Encode
import Spire.Protobuf.Decode


-- ===================================================================
-- ProtoMessage class
-- ===================================================================

-- | Class for protobuf messages that can be encoded and decoded.
--
-- The default implementations use GHC Generics to walk the record
-- structure. Each field must be wrapped in 'Field' with a unique
-- field number.
class ProtoMessage a where
  protoEncode :: a -> ProtoBuilder
  default protoEncode :: (Generic a, GProtoEncode (Rep a)) => a -> ProtoBuilder
  protoEncode = gProtoEncode . from

  protoDecode :: ByteString -> Either DecodeError a
  default protoDecode :: (Generic a, GProtoDecode (Rep a)) => ByteString -> Either DecodeError a
  protoDecode bs = do
    rawFields <- parseFields bs
    to <$> gProtoDecode rawFields

-- | Encode a message to a strict 'ByteString'.
encode :: ProtoMessage a => a -> ByteString
encode = runProtoBuilder . protoEncode
{-# INLINE encode #-}

-- | Decode a message from a strict 'ByteString'.
decode :: ProtoMessage a => ByteString -> Either DecodeError a
decode = protoDecode
{-# INLINE decode #-}

-- ProtoMessage instance for submessage encoding
instance {-# OVERLAPPABLE #-} ProtoMessage a => ProtoEncode a where
  protoWireType = WireLengthDelimited
  protoEncodeValue a = encodeSubmessage (protoEncode a)
  {-# INLINE protoEncodeValue #-}

-- ProtoMessage instance for submessage decoding
instance {-# OVERLAPPABLE #-} ProtoMessage a => ProtoDecode a where
  protoDecodeValue (RawBytes bs) = protoDecode bs
  protoDecodeValue _             = Left (UnexpectedWireType 0 WireLengthDelimited WireVarint)
  {-# INLINE protoDecodeValue #-}


-- ===================================================================
-- GProtoEncode: Generic encoding
-- ===================================================================

-- | Generic encoding class. Walks the 'Rep' of a type and encodes
-- each 'Field n a' it encounters.
class GProtoEncode f where
  gProtoEncode :: f p -> ProtoBuilder

-- M1: metadata wrappers (datatype, constructor, selector) — pass through
instance GProtoEncode f => GProtoEncode (M1 i c f) where
  gProtoEncode (M1 x) = gProtoEncode x
  {-# INLINE gProtoEncode #-}

-- :*: product — encode both sides
instance (GProtoEncode f, GProtoEncode g) => GProtoEncode (f :*: g) where
  gProtoEncode (f :*: g) = gProtoEncode f <> gProtoEncode g
  {-# INLINE gProtoEncode #-}

-- U1: unit (empty record)
instance GProtoEncode U1 where
  gProtoEncode U1 = mempty
  {-# INLINE gProtoEncode #-}

-- K1: a field of type Field n a — encode it
instance (KnownNat n, ProtoEncode a, Eq a, ProtoDefault a)
  => GProtoEncode (K1 i (Field n a)) where
  gProtoEncode (K1 fld) = encodeField fld
  {-# INLINE gProtoEncode #-}

-- K1: optional field — Field n (Maybe a)
instance {-# OVERLAPPING #-} (KnownNat n, ProtoEncode a)
  => GProtoEncode (K1 i (Field n (Maybe a))) where
  gProtoEncode (K1 fld) = encodeOptionalField fld
  {-# INLINE gProtoEncode #-}

-- K1: repeated field — Field n [a]
instance {-# OVERLAPPING #-} (KnownNat n, ProtoEncode a)
  => GProtoEncode (K1 i (Field n [a])) where
  gProtoEncode (K1 fld) = encodeRepeatedField fld
  {-# INLINE gProtoEncode #-}


-- ===================================================================
-- GProtoDecode: Generic decoding
-- ===================================================================

-- | Generic decoding class. Constructs the 'Rep' of a type from a
-- list of raw (field_number, value) pairs.
--
-- The approach: parse all wire fields into @[(Int, RawField)]@ up front,
-- then for each record field, look up its field number in the list.
-- Unknown fields are silently ignored (proto3 forward-compatibility).
-- Last-wins semantics for duplicate field numbers.
class GProtoDecode f where
  gProtoDecode :: [(Int, RawField)] -> Either DecodeError (f p)

-- M1: metadata wrappers — pass through
instance GProtoDecode f => GProtoDecode (M1 i c f) where
  gProtoDecode rfs = M1 <$> gProtoDecode rfs
  {-# INLINE gProtoDecode #-}

-- :*: product — decode both sides from the same field list
instance (GProtoDecode f, GProtoDecode g) => GProtoDecode (f :*: g) where
  gProtoDecode rfs = do
    l <- gProtoDecode rfs
    r <- gProtoDecode rfs
    Right (l :*: r)
  {-# INLINE gProtoDecode #-}

-- U1: unit
instance GProtoDecode U1 where
  gProtoDecode _ = Right U1
  {-# INLINE gProtoDecode #-}

-- K1: required field — Field n a. Look up field number n; if absent, use default.
instance (KnownNat n, ProtoDecode a, ProtoDefault a)
  => GProtoDecode (K1 i (Field n a)) where
  gProtoDecode rfs =
    let fn = fromIntegral (natVal (Proxy @n))
    in case lookupLast fn rfs of
         Nothing  -> Right (K1 (Field protoDefault))
         Just raw -> K1 . Field <$> protoDecodeValue raw
  {-# INLINE gProtoDecode #-}

-- K1: optional field — Field n (Maybe a). Absent means Nothing.
instance {-# OVERLAPPING #-} (KnownNat n, ProtoDecode a)
  => GProtoDecode (K1 i (Field n (Maybe a))) where
  gProtoDecode rfs =
    let fn = fromIntegral (natVal (Proxy @n))
    in case lookupLast fn rfs of
         Nothing  -> Right (K1 (Field Nothing))
         Just raw -> K1 . Field . Just <$> protoDecodeValue raw
  {-# INLINE gProtoDecode #-}

-- K1: repeated field — Field n [a]. Collect all occurrences.
-- Handles both unpacked (each element tagged separately) and packed
-- (all elements in a single length-delimited blob) wire encodings.
-- For packable scalar types (Int32, Int64, Word32, Word64, Bool, Float,
-- Double, SInt32, SInt64), a RawBytes entry is automatically unpacked
-- into multiple values. For non-packable types (Text, ByteString,
-- submessages), each RawBytes is decoded as a single value.
instance {-# OVERLAPPING #-} (KnownNat n, DecodeRepeatedEntry a)
  => GProtoDecode (K1 i (Field n [a])) where
  gProtoDecode rfs =
    let fn = fromIntegral (natVal (Proxy @n))
        raws = [raw | (n', raw) <- rfs, n' == fn]
    in K1 . Field . concat <$> traverse decodeRepeatedEntry raws
  {-# INLINE gProtoDecode #-}


-- ===================================================================
-- Helpers
-- ===================================================================

-- | Look up the last occurrence of a field number (last-wins proto3 semantics).
lookupLast :: Int -> [(Int, RawField)] -> Maybe RawField
lookupLast fn = go Nothing
  where
    go acc [] = acc
    go acc ((n, v) : rest)
      | n == fn   = go (Just v) rest
      | otherwise = go acc rest
{-# INLINE lookupLast #-}


-- ===================================================================
-- ProtoMap: Proto3 map field support
-- ===================================================================

-- | Proto3 default for ProtoMap: empty map.
instance ProtoDefault (ProtoMap k v) where
  protoDefault = ProtoMap Map.empty

-- | Encode a ProtoMap as repeated length-delimited entries.
-- Each map entry is a submessage with field 1 = key, field 2 = value.
instance (Ord k, ProtoEncode k, ProtoEncode v) => ProtoEncode (ProtoMap k v) where
  protoWireType = WireLengthDelimited
  protoEncodeValue (ProtoMap m) =
    let encodeEntry k v =
          let keyEnc   = pbTag 1 (protoWireType @k) <> protoEncodeValue k
              valEnc   = pbTag 2 (protoWireType @v) <> protoEncodeValue v
              entryBody = keyEnc <> valEnc
          in encodeSubmessage entryBody
    in Map.foldlWithKey' (\acc k v -> acc <> encodeEntry k v) mempty m
  {-# INLINE protoEncodeValue #-}

-- | Decode a ProtoMap entry from a single RawBytes (one key-value pair submessage).
-- The caller (repeated field decoder) will collect all entries.
instance (Ord k, ProtoDecode k, ProtoDefault k, ProtoDecode v, ProtoDefault v)
  => ProtoDecode (ProtoMap k v) where
  protoDecodeValue (RawBytes bs) = do
    -- Parse the entry submessage
    rfs <- parseFields bs
    -- Extract key (field 1) and value (field 2), using defaults if absent
    k <- case lookupLast 1 rfs of
           Nothing  -> Right protoDefault
           Just raw -> protoDecodeValue raw
    v <- case lookupLast 2 rfs of
           Nothing  -> Right protoDefault
           Just raw -> protoDecodeValue raw
    Right (ProtoMap (Map.singleton k v))
  protoDecodeValue _ = Left (UnexpectedWireType 0 WireLengthDelimited WireVarint)
  {-# INLINE protoDecodeValue #-}

-- | DecodeRepeatedEntry for ProtoMap: each RawBytes is a single entry.
instance (Ord k, ProtoDecode k, ProtoDefault k, ProtoDecode v, ProtoDefault v)
  => DecodeRepeatedEntry (ProtoMap k v) where
  decodeRepeatedEntry raw = (: []) <$> protoDecodeValue raw
  {-# INLINE decodeRepeatedEntry #-}


-- ===================================================================
-- ProtoMap Generics encoding/decoding
-- ===================================================================

-- K1: ProtoMap field — Field n (ProtoMap k v). Encode all entries as
-- repeated submessages with the same field number.
instance {-# OVERLAPPING #-}
  (KnownNat n, Ord k, ProtoEncode k, ProtoEncode v)
  => GProtoEncode (K1 i (Field n (ProtoMap k v))) where
  gProtoEncode (K1 (Field (ProtoMap m)))
    | Map.null m = mempty
    | otherwise =
      let fn = fromIntegral (natVal (Proxy @n)) :: Int
          tag = pbTag fn WireLengthDelimited
          encodeEntry k v =
            let keyEnc   = pbTag 1 (protoWireType @k) <> protoEncodeValue k
                valEnc   = pbTag 2 (protoWireType @v) <> protoEncodeValue v
                entryBody = keyEnc <> valEnc
            in tag <> encodeSubmessage entryBody
      in Map.foldlWithKey' (\acc k v -> acc <> encodeEntry k v) mempty m
  {-# INLINE gProtoEncode #-}

-- K1: ProtoMap field — Field n (ProtoMap k v). Decode by collecting all
-- repeated submessage entries for the field number and merging them.
instance {-# OVERLAPPING #-}
  (KnownNat n, Ord k, ProtoDecode k, ProtoDefault k, ProtoDecode v, ProtoDefault v)
  => GProtoDecode (K1 i (Field n (ProtoMap k v))) where
  gProtoDecode rfs =
    let fn = fromIntegral (natVal (Proxy @n))
        raws = [raw | (n', raw) <- rfs, n' == fn]
    in do
      entries <- traverse decodeMapEntry raws
      Right (K1 (Field (ProtoMap (Map.unions entries))))
  {-# INLINE gProtoDecode #-}

-- | Decode a single map entry submessage into a singleton Map.
decodeMapEntry :: (Ord k, ProtoDecode k, ProtoDefault k, ProtoDecode v, ProtoDefault v)
               => RawField -> Either DecodeError (Map.Map k v)
decodeMapEntry (RawBytes bs) = do
  rfs <- parseFields bs
  k <- case lookupLast 1 rfs of
         Nothing  -> Right protoDefault
         Just raw -> protoDecodeValue raw
  v <- case lookupLast 2 rfs of
         Nothing  -> Right protoDefault
         Just raw -> protoDecodeValue raw
  Right (Map.singleton k v)
decodeMapEntry _ = Left (UnexpectedWireType 0 WireLengthDelimited WireVarint)
