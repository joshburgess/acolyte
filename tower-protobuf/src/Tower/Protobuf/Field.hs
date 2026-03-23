-- | Type-level field numbers for protobuf messages.
--
-- The 'Field' newtype embeds a compile-time field number (proto3 field tag)
-- into the type. At runtime it is a zero-cost wrapper (via 'coerce').
--
-- @
-- data User = User
--   { name :: Field 1 Text
--   , age  :: Field 2 Int32
--   }
-- @
module Tower.Protobuf.Field
  ( Field (..)
  , fieldVal
    -- * Signed integer wrappers (zigzag encoding)
  , SInt32 (..)
  , SInt64 (..)
  ) where

import Data.Int (Int32, Int64)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))

-- | A protobuf field with a compile-time field number.
--
-- The field number is used for wire encoding; at runtime, 'Field' is
-- just a newtype wrapper (zero overhead via coerce).
newtype Field (n :: Nat) a = Field { unField :: a }
  deriving (Show, Eq, Ord, Functor)

-- | Get the field number at runtime.
fieldVal :: forall n a. KnownNat n => Field n a -> Int
fieldVal _ = fromIntegral (natVal (Proxy @n))
{-# INLINE fieldVal #-}


-- | Signed 32-bit integer using zigzag encoding on the wire.
-- Use for fields declared as @sint32@ in .proto files.
-- Zigzag encoding maps small-magnitude values (positive or negative)
-- to small varints, unlike plain @int32@ which uses 10 bytes for negatives.
newtype SInt32 = SInt32 { unSInt32 :: Int32 }
  deriving (Show, Eq, Ord)

-- | Signed 64-bit integer using zigzag encoding on the wire.
-- Use for fields declared as @sint64@ in .proto files.
newtype SInt64 = SInt64 { unSInt64 :: Int64 }
  deriving (Show, Eq, Ord)
