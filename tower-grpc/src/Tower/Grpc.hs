-- | @tower-grpc@ — gRPC wire protocol for tower services.
--
-- Provides gRPC framing, status codes, and server dispatch built on
-- tower + http-core. Serialization is pluggable — use proto-lens,
-- binary, aeson, or any codec.
--
-- @
-- import Tower.Grpc
-- import Tower.Server.H2 (runServerH2, defaultH2Config)
--
-- main :: IO ()
-- main = do
--   let services = grpcServiceMap
--         [ ("greet.Greeter", "SayHello", unaryHandler sayHello)
--         ]
--   runServerH2 (defaultH2Config 50051) (grpcServer services)
-- @
module Tower.Grpc
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

import Tower.Grpc.Status
import Tower.Grpc.Codec
import Tower.Grpc.Server
import Tower.Grpc.Multiplex
import Tower.Grpc.Reflection
import Tower.Grpc.Health
import Tower.Grpc.Compression
import Tower.Grpc.Web
import Tower.Grpc.ErrorDetails
