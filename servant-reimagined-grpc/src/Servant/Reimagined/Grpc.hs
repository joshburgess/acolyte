-- | @servant-reimagined-grpc@ — gRPC interpretation of API types.
--
-- The same API type that drives REST routing, OpenAPI generation,
-- and type-safe clients also drives gRPC serving and .proto
-- generation.
--
-- @
-- import Servant.Reimagined.Core
-- import Servant.Reimagined.Grpc
-- import Tower.Grpc (grpcServer)
-- import Tower.Server.H2 (runServerH2, defaultH2Config)
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
module Servant.Reimagined.Grpc
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

import Servant.Reimagined.Grpc.Proto
import Servant.Reimagined.Grpc.Server
import Servant.Reimagined.Grpc.Generate
