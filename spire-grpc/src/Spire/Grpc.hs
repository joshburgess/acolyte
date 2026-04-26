-- | @spire-grpc@ — gRPC wire protocol for spire services.
--
-- Provides gRPC framing, status codes, and server dispatch built on
-- spire + http-core. Serialization is pluggable — use proto-lens,
-- binary, aeson, or any codec.
--
-- @
-- import Spire.Grpc
-- import Spire.Server.H2 (runServerH2, defaultH2Config)
--
-- main :: IO ()
-- main = do
--   let services = grpcServiceMap
--         [ ("greet.Greeter", "SayHello", unaryHandler sayHello)
--         ]
--   runServerH2 (defaultH2Config 50051) (grpcServer services)
-- @
module Spire.Grpc
  ( -- * Server
    grpcServer
  , GrpcServiceMap
  , grpcServiceMap
  , GrpcHandler (..)
  , GrpcRequest (..)
  , GrpcResponse (..)

    -- * Handler constructors
  , unaryHandler
  , serverStreamHandler
  , clientStreamHandler
  , bidiStreamHandler

    -- * Status codes
  , GrpcStatus (..)
  , grpcOk
  , grpcCancelled
  , grpcUnknown
  , grpcInvalidArgument
  , grpcDeadlineExceeded
  , grpcNotFound
  , grpcAlreadyExists
  , grpcPermissionDenied
  , grpcResourceExhausted
  , grpcFailedPrecondition
  , grpcAborted
  , grpcOutOfRange
  , grpcUnimplemented
  , grpcInternal
  , grpcUnavailable
  , grpcDataLoss
  , grpcUnauthenticated
  , grpcStatusCode
  , statusFromCode

    -- * Multiplexing (REST + gRPC on same port)
  , multiplex

    -- * Codec (low-level framing)
  , encodeMessage
  , encodeMessages
  , decodeMessage
  , decodeMessages
  , GrpcMessage (..)

    -- * Server reflection
  , reflectionService
  , withReflection
  , ServiceInfo (..)
  , MethodInfo (..)

    -- * Health check
  , healthService
  , withHealthCheck
  , HealthStatus (..)

    -- * Compression
  , compressMessage
  , decompressMessage
  , maxDecompressedSize
  , GrpcCompression (..)
  , encodeMessageCompressed

    -- * gRPC-Web
  , grpcWebLayer

    -- * Rich error details
  , RichGrpcStatus (..)
  , richError
  , ErrorDetail (..)
  , BadRequest (..)
  , FieldViolation (..)
  , DebugInfo (..)
  , RetryInfo (..)
  , Help (..)
  , HelpLink (..)
  , ResourceInfo (..)
  , richErrorResponse
  , encodeDetails
  ) where

import Spire.Grpc.Status
import Spire.Grpc.Codec
import Spire.Grpc.Server
import Spire.Grpc.Multiplex
import Spire.Grpc.Reflection
import Spire.Grpc.Health
import Spire.Grpc.Compression
import Spire.Grpc.Web
import Spire.Grpc.ErrorDetails
