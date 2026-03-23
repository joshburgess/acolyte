-- | Protobuf compatibility types and compile-time validation.
--
-- 'ToProtoType' declares that a Haskell type can be serialized to
-- protobuf wire format. 'GrpcReady' checks at compile time that
-- every request/response type in the API has this capability.
--
-- Serialization is pluggable — implement 'ToProtoType' with
-- proto-lens, binary, manual encoding, or any codec. The gRPC
-- layer only needs 'encode' and 'decode' on 'ByteString'.
module Servant.Reimagined.Grpc.Proto
  ( -- * Serialization class
    GrpcCodec (..)
    -- * Proto type metadata
  , ProtoMessageName (..)
    -- * Compile-time validation
  , GrpcReady
  , AllGrpcReady
    -- * Proto field metadata (for .proto generation)
  , ProtoField (..)
  , ProtoFieldType (..)
  , HasProtoFields (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type, Constraint)
import Data.Text (Text)
import GHC.TypeLits (TypeError, ErrorMessage (..))

import Servant.Reimagined.Core.Endpoint (Endpoint, NoBody)
import Servant.Reimagined.Core.Effect (Requires)
import Servant.Reimagined.Server.Response (Json)


-- | Encode and decode values to/from protobuf-compatible bytes.
--
-- This is deliberately minimal — no proto-lens dependency, no
-- code generation requirement. Implement it however you like:
--
-- @
-- instance GrpcCodec MyMessage where
--   grpcEncode msg = ... -- your serialization
--   grpcDecode bs  = ... -- your deserialization
--
-- -- With proto-lens:
-- instance GrpcCodec MyProtoMessage where
--   grpcEncode = encodeMessage
--   grpcDecode = decodeMessage
--
-- -- With aeson (for grpc+json):
-- instance GrpcCodec MyType where
--   grpcEncode = LBS.toStrict . Aeson.encode
--   grpcDecode = Aeson.decodeStrict'
-- @
class GrpcCodec a where
  grpcEncode :: a -> ByteString
  grpcDecode :: ByteString -> Maybe a


-- | The protobuf message name for .proto generation.
--
-- @
-- instance ProtoMessageName User where
--   protoMessageName = "User"
-- @
class ProtoMessageName a where
  protoMessageName :: Text


-- | A field in a proto message (for .proto file generation).
data ProtoField = ProtoField
  { pfName   :: !Text
  , pfNumber :: !Int
  , pfType   :: !ProtoFieldType
  } deriving (Show, Eq)


-- | Proto scalar types.
data ProtoFieldType
  = ProtoInt32
  | ProtoInt64
  | ProtoUInt32
  | ProtoUInt64
  | ProtoDouble
  | ProtoFloat
  | ProtoBool
  | ProtoString
  | ProtoBytes
  | ProtoMessage !Text   -- ^ nested message by name
  | ProtoRepeated !ProtoFieldType
  | ProtoOptional !ProtoFieldType
  deriving (Show, Eq)


-- | Declare the fields of a proto message (for .proto generation).
class HasProtoFields a where
  protoFields :: [ProtoField]


-- ===================================================================
-- Compile-time validation: GrpcReady
-- ===================================================================

-- | Check that all request and response types in an API support
-- gRPC serialization.
--
-- Missing a 'GrpcCodec' instance? The compiler will report:
--
-- @
-- No instance for 'GrpcCodec MyType'
--   arising from a use of 'mkGrpcServiceMap'
-- @
--
-- To fix, add: @instance GrpcCodec MyType where ...@
--
-- Usage:
--
-- @
-- type MyAPI = '[ Get HealthPath Text
--               , Post UsersPath (Json CreateUser) (Json User)
--               ]
--
-- -- This compiles only if Text, CreateUser, and User all have GrpcCodec.
-- server :: GrpcReady MyAPI => ...
-- @
type GrpcReady (api :: [Type]) = AllGrpcReady api


-- | Walk the API list and check each endpoint's types.
--
-- Each endpoint's request and response types are individually checked
-- for a 'GrpcCodec' instance. A missing instance produces:
--
-- @
-- No instance for 'GrpcCodec SomeType'
-- @
--
-- The fix is always the same: add an @instance GrpcCodec SomeType@.
type AllGrpcReady :: [Type] -> Constraint
type family AllGrpcReady (api :: [Type]) :: Constraint where
  AllGrpcReady '[] = ()

  AllGrpcReady (Endpoint m path NoBody resp ': rest) =
    ( AssertGrpcCodec resp
    , AllGrpcReady rest
    )

  AllGrpcReady (Endpoint m path req resp ': rest) =
    ( AssertGrpcCodec req
    , AssertGrpcCodec resp
    , AllGrpcReady rest
    )

  -- Requires: delegate to inner endpoint
  AllGrpcReady (Requires e inner ': rest) =
    ( AllGrpcReady '[inner]
    , AllGrpcReady rest
    )

  -- Fallback: skip unknown wrappers
  AllGrpcReady (_ ': rest) = AllGrpcReady rest


-- | Assert that a type has a GrpcCodec instance.
type AssertGrpcCodec :: Type -> Constraint
type family AssertGrpcCodec (a :: Type) :: Constraint where
  -- Json wrapper: check the inner type
  AssertGrpcCodec (Json a) = GrpcCodec a
  -- Bare types: check directly
  AssertGrpcCodec a = GrpcCodec a


-- Json imported from Servant.Reimagined.Server.Response
