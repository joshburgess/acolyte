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
module Tower.Protobuf.Message
  ( ProtoMessage (..)
  , encode
  , decode
  , GProtoEncode (..)
  , GProtoDecode (..)
  ) where

import Data.ByteString (ByteString)
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)
import Data.Proxy (Proxy(..))

import Tower.Protobuf.Wire (WireType(..))
import Tower.Protobuf.Field
import Tower.Protobuf.Encode
import Tower.Protobuf.Decode


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
instance {-# OVERLAPPING #-} (KnownNat n, ProtoDecode a)
  => GProtoDecode (K1 i (Field n [a])) where
  gProtoDecode rfs =
    let fn = fromIntegral (natVal (Proxy @n))
        raws = [raw | (n', raw) <- rfs, n' == fn]
    in K1 . Field <$> traverse protoDecodeValue raws
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
