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
module Tower.Protobuf
  ( -- * Messages
    ProtoMessage (..)
  , encode
  , decode
    -- * Field wrapper
  , Field (..)
    -- * Signed integer wrappers (zigzag encoding)
  , SInt32 (..)
  , SInt64 (..)
    -- * Errors
  , DecodeError (..)
    -- * Re-exports for advanced use
  , ProtoEncode (..)
  , ProtoDecode (..)
  , decodePacked
  , ProtoDefault (..)
  , ProtoBuilder
  , WireType (..)
  ) where

import Tower.Protobuf.Wire (WireType (..))
import Tower.Protobuf.Field (Field (..), SInt32(..), SInt64(..))
import Tower.Protobuf.Encode (ProtoEncode (..), ProtoBuilder, ProtoDefault (..))
import Tower.Protobuf.Decode (ProtoDecode (..), DecodeError (..), decodePacked)
import Tower.Protobuf.Message (ProtoMessage (..), encode, decode)
