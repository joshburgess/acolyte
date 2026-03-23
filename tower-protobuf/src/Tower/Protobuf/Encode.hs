{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Length-memoizing builder and encoding for protobuf values.
--
-- The key insight: protobuf submessages need their byte length encoded
-- as a varint /before/ the content. A normal 'Builder' doesn't know its
-- length until materialized. 'ProtoBuilder' tracks the byte count
-- alongside the builder, so submessage encoding is a single pass.
module Tower.Protobuf.Encode
  ( -- * Length-memoizing Builder
    ProtoBuilder (..)
  , runProtoBuilder
  , pbEmpty
    -- * ProtoEncode class
  , ProtoEncode (..)
    -- * Field encoding
  , encodeField
  , encodeOptionalField
  , encodeRepeatedField
  , encodePackedField
    -- * Submessage encoding
  , encodeSubmessage
    -- * Proto3 default values
  , ProtoDefault (..)
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BEx
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import GHC.TypeLits (KnownNat, natVal)
import Data.Proxy (Proxy(..))

import Tower.Protobuf.Wire
import Tower.Protobuf.Field


-- ===================================================================
-- ProtoBuilder
-- ===================================================================

-- | A 'Builder' paired with its exact byte length.
--
-- This is needed for submessage encoding: protobuf length-delimited
-- fields require the byte count as a varint prefix. Tracking it
-- incrementally avoids a double-pass.
data ProtoBuilder = ProtoBuilder
  { pbLength  :: {-# UNPACK #-} !Int
  , pbBuilder :: !B.Builder
  }

instance Semigroup ProtoBuilder where
  ProtoBuilder l1 b1 <> ProtoBuilder l2 b2 =
    ProtoBuilder (l1 + l2) (b1 <> b2)
  {-# INLINE (<>) #-}

instance Monoid ProtoBuilder where
  mempty = ProtoBuilder 0 mempty
  {-# INLINE mempty #-}

-- | Empty builder.
pbEmpty :: ProtoBuilder
pbEmpty = mempty
{-# INLINE pbEmpty #-}

-- | Materialize a 'ProtoBuilder' into a strict 'ByteString'.
-- Uses 'safeStrategy' with the exact known length for a single allocation.
runProtoBuilder :: ProtoBuilder -> ByteString
runProtoBuilder (ProtoBuilder len bld) =
  LBS.toStrict (BEx.toLazyByteStringWith
    (BEx.safeStrategy len BEx.smallChunkSize)
    LBS.empty
    bld)
{-# INLINE runProtoBuilder #-}


-- ===================================================================
-- Helpers: wrap Wire primitives as ProtoBuilder
-- ===================================================================

-- | Encode a varint as a 'ProtoBuilder'.
pbVarint :: Word64 -> ProtoBuilder
pbVarint n = ProtoBuilder (varintSize n) (encodeVarint n)
{-# INLINE pbVarint #-}

-- | Number of bytes a varint takes on the wire.
varintSize :: Word64 -> Int
varintSize n
  | n < 0x80       = 1
  | n < 0x4000     = 2
  | n < 0x200000   = 3
  | n < 0x10000000 = 4
  | n < 0x0800000000 = 5
  | n < 0x040000000000 = 6
  | n < 0x02000000000000 = 7
  | n < 0x0100000000000000 = 8
  | n < 0x8000000000000000 = 9
  | otherwise = 10
{-# INLINE varintSize #-}

-- | Encode a tag as a 'ProtoBuilder'.
pbTag :: Int -> WireType -> ProtoBuilder
pbTag fieldNum wt =
  let tagVal = fromIntegral fieldNum `shiftL` 3 .|. wireTypeToWord wt
  in pbVarint tagVal
{-# INLINE pbTag #-}

wireTypeToWord :: WireType -> Word64
wireTypeToWord WireVarint          = 0
wireTypeToWord WireFixed64         = 1
wireTypeToWord WireLengthDelimited = 2
wireTypeToWord WireFixed32         = 5
{-# INLINE wireTypeToWord #-}

-- | Encode raw bytes as a 'ProtoBuilder'.
pbBytes :: ByteString -> ProtoBuilder
pbBytes bs = ProtoBuilder (BS.length bs) (B.byteString bs)
{-# INLINE pbBytes #-}


-- ===================================================================
-- ProtoEncode class
-- ===================================================================

-- | Class for types that can be encoded into protobuf wire format.
class ProtoEncode a where
  -- | The wire type used for this value.
  protoWireType :: WireType
  -- | Encode the value (without a tag prefix).
  protoEncodeValue :: a -> ProtoBuilder

-- Int32: varint encoding (cast to Word64 preserving bit pattern)
instance ProtoEncode Int32 where
  protoWireType = WireVarint
  protoEncodeValue n = pbVarint (fromIntegral n :: Word64)
  {-# INLINE protoEncodeValue #-}

-- Int64: varint encoding
instance ProtoEncode Int64 where
  protoWireType = WireVarint
  protoEncodeValue n = pbVarint (fromIntegral n :: Word64)
  {-# INLINE protoEncodeValue #-}

-- Word32: varint encoding
instance ProtoEncode Word32 where
  protoWireType = WireVarint
  protoEncodeValue n = pbVarint (fromIntegral n)
  {-# INLINE protoEncodeValue #-}

-- Word64: varint encoding
instance ProtoEncode Word64 where
  protoWireType = WireVarint
  protoEncodeValue = pbVarint
  {-# INLINE protoEncodeValue #-}

-- Bool: varint 0 or 1
instance ProtoEncode Bool where
  protoWireType = WireVarint
  protoEncodeValue b = pbVarint (if b then 1 else 0)
  {-# INLINE protoEncodeValue #-}

-- Float: fixed32
instance ProtoEncode Float where
  protoWireType = WireFixed32
  protoEncodeValue f = ProtoBuilder 4 (encodeProtoFloat f)
  {-# INLINE protoEncodeValue #-}

-- Double: fixed64
instance ProtoEncode Double where
  protoWireType = WireFixed64
  protoEncodeValue d = ProtoBuilder 8 (encodeProtoDouble d)
  {-# INLINE protoEncodeValue #-}

-- Text: length-delimited UTF-8
instance ProtoEncode Text where
  protoWireType = WireLengthDelimited
  protoEncodeValue t =
    let bs = TE.encodeUtf8 t
        len = BS.length bs
    in pbVarint (fromIntegral len) <> pbBytes bs
  {-# INLINE protoEncodeValue #-}

-- ByteString: length-delimited raw bytes
instance ProtoEncode ByteString where
  protoWireType = WireLengthDelimited
  protoEncodeValue bs =
    let len = BS.length bs
    in pbVarint (fromIntegral len) <> pbBytes bs
  {-# INLINE protoEncodeValue #-}


-- ===================================================================
-- Submessage encoding
-- ===================================================================

-- | Encode a pre-built submessage as a length-delimited value.
-- Prepends the byte length as a varint.
encodeSubmessage :: ProtoBuilder -> ProtoBuilder
encodeSubmessage (ProtoBuilder len bld) =
  let lenPb = pbVarint (fromIntegral len)
  in lenPb <> ProtoBuilder len bld
{-# INLINE encodeSubmessage #-}


-- ===================================================================
-- Field encoding
-- ===================================================================

-- | Encode a single field: tag + value.
-- Proto3 semantics: skips the field if the value is the default.
encodeField :: forall n a. (KnownNat n, ProtoEncode a, Eq a, ProtoDefault a)
            => Field n a -> ProtoBuilder
encodeField (Field val)
  | val == protoDefault = mempty
  | otherwise =
    let fn = fromIntegral (natVal (Proxy @n)) :: Int
        tag = pbTag fn (protoWireType @a)
    in tag <> protoEncodeValue val
{-# INLINE encodeField #-}

-- | Encode an optional field ('Maybe' wrapper).
-- 'Nothing' encodes to zero bytes. 'Just' encodes like a regular field.
encodeOptionalField :: forall n a. (KnownNat n, ProtoEncode a)
                    => Field n (Maybe a) -> ProtoBuilder
encodeOptionalField (Field Nothing)  = mempty
encodeOptionalField (Field (Just val)) =
  let fn = fromIntegral (natVal (Proxy @n)) :: Int
      tag = pbTag fn (protoWireType @a)
  in tag <> protoEncodeValue val
{-# INLINE encodeOptionalField #-}

-- | Encode a repeated (non-packed) field: each element gets its own tag.
encodeRepeatedField :: forall n a. (KnownNat n, ProtoEncode a)
                    => Field n [a] -> ProtoBuilder
encodeRepeatedField (Field xs) =
  let fn = fromIntegral (natVal (Proxy @n)) :: Int
      tag = pbTag fn (protoWireType @a)
  in foldMap (\v -> tag <> protoEncodeValue v) xs
{-# INLINE encodeRepeatedField #-}

-- | Encode a packed repeated field: one length-delimited blob containing
-- all elements concatenated (only valid for scalar numeric types).
encodePackedField :: forall n a. (KnownNat n, ProtoEncode a)
                  => Field n [a] -> ProtoBuilder
encodePackedField (Field []) = mempty
encodePackedField (Field xs) =
  let fn = fromIntegral (natVal (Proxy @n)) :: Int
      tag = pbTag fn WireLengthDelimited
      content = foldMap protoEncodeValue xs
  in tag <> encodeSubmessage content
{-# INLINE encodePackedField #-}


-- ===================================================================
-- Proto3 default values
-- ===================================================================

-- | Default values for proto3 field types.
-- In proto3, fields with the default value are omitted from the wire.
class ProtoDefault a where
  protoDefault :: a

instance ProtoDefault Int32  where protoDefault = 0
instance ProtoDefault Int64  where protoDefault = 0
instance ProtoDefault Word32 where protoDefault = 0
instance ProtoDefault Word64 where protoDefault = 0
instance ProtoDefault Bool   where protoDefault = False
instance ProtoDefault Float  where protoDefault = 0.0
instance ProtoDefault Double where protoDefault = 0.0
instance ProtoDefault Text   where protoDefault = ""
instance ProtoDefault ByteString where protoDefault = BS.empty
