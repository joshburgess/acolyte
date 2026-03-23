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
  ) where

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
