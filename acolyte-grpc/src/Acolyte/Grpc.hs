-- | @acolyte-grpc@ — gRPC interpretation of API types.
--
-- The same API type that drives REST routing, OpenAPI generation,
-- and type-safe clients also drives gRPC serving and .proto
-- generation.
--
-- @
-- import Acolyte.Core
-- import Acolyte.Grpc
-- import Spire.Grpc (grpcServer)
-- import Spire.Server.H2 (runServerH2, defaultH2Config)
--
-- type API = '[ Get HealthPath Text
--             , Post UsersPath (Json CreateUser) (Json User)
--             ]
--
-- main = do
--   let services = mkGrpcServiceMap @API "myapp" "MySvc"
--         (healthHandler, createUserHandler)
--   runServerH2 (defaultH2Config 50051) (grpcServer services)
-- @
module Acolyte.Grpc
  ( -- * Serialization
    GrpcCodec (..)
  , ProtoMessageName (..)

    -- * Compile-time validation
  , GrpcReady
  , AllGrpcReady

    -- * Proto metadata
  , ProtoField (..)
  , ProtoFieldType (..)
  , HasProtoFields (..)

    -- * Server construction
  , mkGrpcServiceMap
  , GrpcHandlerFn
  , BuildGrpcHandlers (..)

    -- * .proto generation
  , generateProto
  , generateProtoFull
  , ApiToProtoMethods (..)
  , ProtoMethod (..)
  , MessageDef (..)
  , renderMessage
  , renderFieldType
  , CollectMessageDefs (..)
  , CollectTypeDef (..)
  ) where

import Acolyte.Grpc.Proto
import Acolyte.Grpc.Server
import Acolyte.Grpc.Generate
