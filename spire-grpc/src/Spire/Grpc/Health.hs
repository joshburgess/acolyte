{-# LANGUAGE OverloadedStrings #-}
-- | gRPC health check service.
--
-- Implements the @grpc.health.v1.Health/Check@ protocol.
-- See <https://github.com/grpc/grpc/blob/master/doc/health-checking.md>.
--
-- For simplicity, the health status is encoded as a single byte:
--
--   * 0 = UNKNOWN
--   * 1 = SERVING
--   * 2 = NOT_SERVING
--
-- @
-- import Spire.Grpc
-- import Spire.Grpc.Health
--
-- main :: IO ()
-- main = do
--   let services = withHealthCheck (pure Serving) $ grpcServiceMap
--         [ ("myapp.Greeter", "SayHello", unaryHandler sayHello)
--         ]
--   runServerH2 (defaultH2Config 50051) (grpcServer services)
-- @
module Spire.Grpc.Health
  ( -- * Health status
    HealthStatus (..)
    -- * Service construction
  , healthService
  , withHealthCheck
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Spire.Grpc.Status
import Spire.Grpc.Server (GrpcHandler (..), GrpcResponse (..), GrpcServiceMap)


-- | Health status of a service.
data HealthStatus = Serving | NotServing | Unknown
  deriving (Show, Eq)


-- | Encode a 'HealthStatus' as a single byte.
encodeStatus :: HealthStatus -> ByteString
encodeStatus Unknown    = BS.singleton 0
encodeStatus Serving    = BS.singleton 1
encodeStatus NotServing = BS.singleton 2


-- | Build a health check service map.
--
-- Implements @grpc.health.v1.Health/Check@. The provided action is
-- called on each health check request to get the current status.
healthService :: IO HealthStatus -> GrpcServiceMap
healthService getStatus = Map.fromList
  [ (("grpc.health.v1.Health", "Check"), healthCheckHandler getStatus)
  ]


-- | Add health check to an existing service map.
--
-- Merges the health check service into the given map.
-- If the map already contains a @grpc.health.v1.Health/Check@ entry,
-- it will be overwritten.
withHealthCheck :: IO HealthStatus -> GrpcServiceMap -> GrpcServiceMap
withHealthCheck getStatus existing =
  Map.union (healthService getStatus) existing


-- | Handler for @grpc.health.v1.Health/Check@.
healthCheckHandler :: IO HealthStatus -> GrpcHandler
healthCheckHandler getStatus = GrpcHandler $ \_req -> do
  status <- getStatus
  pure $ GrpcUnary (grpcOk "") (encodeStatus status)
