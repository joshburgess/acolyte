-- | Convenience prelude — import everything you typically need.
--
-- @
-- import Acolyte.Prelude
-- @
--
-- This single import gives you:
--
-- * All API types and path helpers from @acolyte-core@
-- * All extractors, response types, and server wiring from @acolyte-server@
-- * Spire Service\/Layer\/Middleware composition
-- * HTTP middleware (CORS, security headers, tracing, etc.)
-- * Backend-agnostic HTTP Request\/Response types
-- * spire-server (zero-WAI HTTP server)
module Acolyte.Prelude
  ( -- * Core API types (methods, paths, endpoints, effects, wrappers)
    module Acolyte.Core

    -- * Server (extractors, responses, wiring, effects)
  , module Acolyte.Server

    -- * Spire (composition)
  , Service (..)
  , Layer (..)
  , Middleware
  , middleware
  , applyLayer
  , (|>)
  , (<|)
  , composeLayer
  , before
  , after
  , around
  , mapRequest
  , mapResponse
  , mapResponseM
  , contramapRequest
  , hoistService

    -- * HTTP types (backend-agnostic)
  , Http.Core.Request (..)
  , Http.Core.Response (..)
  , Extensions
  , emptyExtensions
  , newExtensions
  , insertExtension
  , lookupExtension
  , deleteExtension
  , hasExtension
  , splitRequest
  , RequestParts (..)
  , defaultRequest

    -- * HTTP response helpers
  , ok
  , created
  , noContent
  , badRequest
  , unauthorized
  , forbidden
  , notFound
  , methodNotAllowed
  , unprocessable
  , serverError

    -- * Body types
  , Body (..)
  , BodyChunk (..)
  , FileRange (..)
  , fromBytes
  , fromLazyBytes
  , fromBuilder
  , fromText
  , streamBody
  , streamFromList
  , fileBody
  , emptyBody
  , bodyToStrict
  , foldBodyChunks
  , isEmptyBody

    -- * HTTP status codes (re-exported from http-types)
  , Status
  , status200
  , status201
  , status204
  , status400
  , status401
  , status403
  , status404
  , status405
  , status422
  , status500
    -- | Extract the numeric status code from a 'Status' value.
  , statusCode

    -- * HTTP methods (re-exported from http-types)
  , methodGet
  , methodPost
  , methodPut
  , methodDelete
  , methodPatch
  , methodHead
  , methodOptions

    -- * HTTP headers (re-exported from http-types)
  , RequestHeaders
  , ResponseHeaders
  , Header
  , HeaderName
  , hContentType
  , hAccept
  , hAuthorization

    -- * HTTP middleware layers (spire-http)
  , SecureHeadersConfig (..)
  , defaultSecureHeaders
  , secureHeadersLayer
  , RequestId (..)
  , requestIdLayer
  , requestIdHeader
  , TraceEntry (..)
  , traceLayer
  , CorsConfig (..)
  , defaultCors
  , permissiveCors
  , corsLayer
  , timeoutLayer
  , CompressionConfig (..)
  , defaultCompression
  , compressionLayer

    -- * spire-server (zero-WAI HTTP server)
  , runServer
  , runServerBS
  , runServerConfig
  , runServerConfigBS
  , runServerWithShutdown
  , ServerConfig (..)
  , defaultConfig
  , adaptToBody

    -- * gRPC (spire-grpc)
  , grpcServer
  , GrpcServiceMap
  , grpcServiceMap
  , GrpcHandler (..)
  , GrpcRequest (..)
  , GrpcResponse (..)
  , unaryHandler
  , serverStreamHandler
  , clientStreamHandler
  , bidiStreamHandler
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
  , encodeMessage
  , encodeMessages
  , decodeMessage
  , decodeMessages
  , GrpcMessage (..)
  , multiplex

    -- * gRPC reflection
  , reflectionService
  , withReflection
  , ServiceInfo (..)
  , MethodInfo (..)

    -- * gRPC health check
  , healthService
  , withHealthCheck
  , HealthStatus (..)

    -- * gRPC compression
  , compressMessage
  , decompressMessage
  , GrpcCompression (..)
  , encodeMessageCompressed

    -- * gRPC interpretation (acolyte-grpc)
  , GrpcCodec (..)
  , GrpcReady
  , AllGrpcReady
  , mkGrpcServiceMap
  , generateProto
  , ProtoMessageName (..)

    -- * WebSocket session types (spire-websocket)
  , Session
  , WebSocketConn (..)
  , send
  , recv
  , offer
  , select1
  , select2
  , close
  , recurse
  , loop
  , withSession
  , Unfold
  ) where

import Acolyte.Core
import Acolyte.Server
import Spire
import Http.Core
import Spire.Http
import Spire.Server
import Spire.Grpc
import Acolyte.Grpc (GrpcCodec(..), GrpcReady, AllGrpcReady, mkGrpcServiceMap, generateProto, ProtoMessageName(..))
import Spire.WebSocket.Session
