-- | High-performance Protocol Buffers for Haskell.
--
-- Define messages as Haskell records with 'Field' wrappers for field
-- numbers, derive 'Generic' and 'ProtoMessage', and get encode\/decode
-- for free:
--
-- @
-- data User = User
--   { name  :: Field 1 Text
--   , age   :: Field 2 Int32
--   , email :: Field 3 (Maybe Text)
--   } deriving (Generic)
--
-- instance ProtoMessage User
--
-- -- Encode:
-- let bs = encode (User (Field "Alice") (Field 30) (Field (Just "alice\@example.com")))
--
-- -- Decode:
-- case decode bs of
--   Right user -> print user
--   Left err   -> error (show err)
-- @
module Spire.Protobuf
  ( -- * Messages
    ProtoMessage (..)
  , encode
  , decode
    -- * Field wrapper
  , Field (..)
    -- * Signed integer wrappers (zigzag encoding)
  , SInt32 (..)
  , SInt64 (..)
    -- * Proto3 map fields
  , ProtoMap (..)
    -- * Errors
  , DecodeError (..)
    -- * Re-exports for advanced use
  , ProtoEncode (..)
  , ProtoDecode (..)
  , decodePacked
  , DecodeRepeatedEntry (..)
  , ProtoDefault (..)
  , ProtoBuilder
  , WireType (..)
  ) where

import Spire.Protobuf.Wire (WireType (..))
import Spire.Protobuf.Field (Field (..), SInt32(..), SInt64(..), ProtoMap(..))
import Spire.Protobuf.Encode (ProtoEncode (..), ProtoBuilder, ProtoDefault (..))
import Spire.Protobuf.Decode (ProtoDecode (..), DecodeError (..), decodePacked, DecodeRepeatedEntry(..))
import Spire.Protobuf.Message (ProtoMessage (..), encode, decode)
