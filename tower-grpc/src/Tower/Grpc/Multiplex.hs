{-# LANGUAGE OverloadedStrings #-}
-- | REST+gRPC multiplexing — serve both on the same port.
--
-- Dispatches based on the @Content-Type@ header:
--
-- * @application\/grpc*@ → gRPC service
-- * everything else → REST service
--
-- @
-- main = do
--   let rest = mkServer @API restHandlers
--   let grpc = grpcServer (mkGrpcServiceMap @API "pkg" "Svc" grpcHandlers)
--   let combined = multiplex (adaptToBody rest) grpc
--   runServerH2 (defaultH2Config 8080) combined
-- @
module Tower.Grpc.Multiplex
  ( multiplex
  ) where

import qualified Data.ByteString as BS

import Tower.Service (Service (..))
import Http.Core (Request (..), Response, Body)


-- | Combine a REST service and a gRPC service into one.
--
-- Dispatches based on the @Content-Type@ header:
--
-- * @application\/grpc*@ → gRPC service
-- * everything else → REST service
multiplex
  :: Service IO (Request Body) (Response Body)  -- ^ REST service
  -> Service IO (Request Body) (Response Body)  -- ^ gRPC service
  -> Service IO (Request Body) (Response Body)  -- ^ Combined service
multiplex (Service restHandler) (Service grpcHandler) = Service $ \req ->
  let ct = lookup "content-type" (requestHeaders req)
  in case ct of
    Just v | "application/grpc" `BS.isPrefixOf` v -> grpcHandler req
    _ -> restHandler req
