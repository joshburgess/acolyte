{-# LANGUAGE OverloadedStrings #-}
-- | gRPC status codes per the gRPC specification.
--
-- These are sent in the @grpc-status@ trailer, NOT in the HTTP status
-- code (which is always 200 for gRPC).
module Tower.Grpc.Status
  ( -- * Status type
    GrpcStatus (..)
    -- * Status codes
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
    -- * Conversion
  , grpcStatusCode
  , statusFromCode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)


-- | A gRPC status with code and optional message.
data GrpcStatus = GrpcStatus
  { gsCode    :: !Int
  , gsMessage :: !Text
  } deriving (Show, Eq)


-- | Status code 0: success.
grpcOk :: Text -> GrpcStatus
grpcOk                 = GrpcStatus 0

-- | Status code 1: the operation was cancelled.
grpcCancelled :: Text -> GrpcStatus
grpcCancelled          = GrpcStatus 1

-- | Status code 2: unknown error.
grpcUnknown :: Text -> GrpcStatus
grpcUnknown            = GrpcStatus 2

-- | Status code 3: the client specified an invalid argument.
grpcInvalidArgument :: Text -> GrpcStatus
grpcInvalidArgument    = GrpcStatus 3

-- | Status code 4: deadline expired before the operation completed.
grpcDeadlineExceeded :: Text -> GrpcStatus
grpcDeadlineExceeded   = GrpcStatus 4

-- | Status code 5: requested entity was not found.
grpcNotFound :: Text -> GrpcStatus
grpcNotFound           = GrpcStatus 5

-- | Status code 6: the entity already exists.
grpcAlreadyExists :: Text -> GrpcStatus
grpcAlreadyExists      = GrpcStatus 6

-- | Status code 7: the caller does not have permission.
grpcPermissionDenied :: Text -> GrpcStatus
grpcPermissionDenied   = GrpcStatus 7

-- | Status code 8: some resource has been exhausted.
grpcResourceExhausted :: Text -> GrpcStatus
grpcResourceExhausted  = GrpcStatus 8

-- | Status code 9: the system is not in the required state.
grpcFailedPrecondition :: Text -> GrpcStatus
grpcFailedPrecondition = GrpcStatus 9

-- | Status code 10: the operation was aborted.
grpcAborted :: Text -> GrpcStatus
grpcAborted            = GrpcStatus 10

-- | Status code 11: the operation was attempted past the valid range.
grpcOutOfRange :: Text -> GrpcStatus
grpcOutOfRange         = GrpcStatus 11

-- | Status code 12: the operation is not implemented.
grpcUnimplemented :: Text -> GrpcStatus
grpcUnimplemented      = GrpcStatus 12

-- | Status code 13: internal error.
grpcInternal :: Text -> GrpcStatus
grpcInternal           = GrpcStatus 13

-- | Status code 14: the service is currently unavailable.
grpcUnavailable :: Text -> GrpcStatus
grpcUnavailable        = GrpcStatus 14

-- | Status code 15: unrecoverable data loss or corruption.
grpcDataLoss :: Text -> GrpcStatus
grpcDataLoss           = GrpcStatus 15

-- | Status code 16: the request does not have valid authentication credentials.
grpcUnauthenticated :: Text -> GrpcStatus
grpcUnauthenticated    = GrpcStatus 16


-- | Convert a gRPC status to its numeric code as a ByteString.
grpcStatusCode :: GrpcStatus -> ByteString
grpcStatusCode = BS8.pack . show . gsCode


-- | Parse a numeric code into a GrpcStatus (with empty message).
statusFromCode :: Int -> GrpcStatus
statusFromCode n = GrpcStatus n ""
