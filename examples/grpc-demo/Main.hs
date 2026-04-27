{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | One type, four interpretations: REST + gRPC + OpenAPI + Proto3.
--
-- Demonstrates the core thesis of acolyte: a single API type
-- drives server routing, gRPC multiplexing, OpenAPI spec generation,
-- and .proto definition output.
--
-- Run:  cabal run grpc-demo
--
-- The server speaks HTTP/2 cleartext (h2c) only, so curl needs
-- --http2-prior-knowledge to skip the HTTP/1.1 upgrade dance.
--
-- REST: curl --http2-prior-knowledge http://localhost:8080/health
--       curl --http2-prior-knowledge -X POST http://localhost:8080/greet \
--            -d '{"name":"Alice"}' -H 'Content-Type: application/json'
-- gRPC: grpcurl -plaintext localhost:8080 greeter.GreeterService/Health
--       grpcurl -plaintext localhost:8080 greeter.GreeterService/Greet
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import Acolyte.Prelude
import Acolyte.OpenApi (generateSpec, ToSchema)
import Acolyte.Grpc
  ( HasProtoFields(..)
  , ProtoField(..), ProtoFieldType(..)
  , CollectTypeDef(..), MessageDef(..)
  , GrpcHandlerFn
  )
import Spire.Server.H2 (runServerH2, defaultH2Config)


-- ===================================================================
-- 1. Types
-- ===================================================================

data GreetRequest = GreetRequest { name :: !Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GreetReply = GreetReply { message :: !Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data HealthReply = HealthReply { status :: !Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- GrpcCodec: simple UTF-8 encoding for the demo
instance GrpcCodec GreetRequest where
  grpcEncode (GreetRequest t) = encodeUtf8 t
  grpcDecode bs = Just (GreetRequest (decodeUtf8 bs))

instance GrpcCodec GreetReply where
  grpcEncode (GreetReply t) = encodeUtf8 t
  grpcDecode bs = Just (GreetReply (decodeUtf8 bs))

instance GrpcCodec HealthReply where
  grpcEncode (HealthReply t) = encodeUtf8 t
  grpcDecode bs = Just (HealthReply (decodeUtf8 bs))

-- ProtoMessageName: names used in .proto generation
instance ProtoMessageName GreetRequest where protoMessageName = "GreetRequest"
instance ProtoMessageName GreetReply   where protoMessageName = "GreetReply"
instance ProtoMessageName HealthReply  where protoMessageName = "HealthReply"

-- HasProtoFields: field definitions for .proto message blocks
instance HasProtoFields GreetRequest where
  protoFields = [ProtoField "name" 1 ProtoString]

instance HasProtoFields HealthReply where
  protoFields = [ProtoField "status" 1 ProtoString]

instance HasProtoFields GreetReply where
  protoFields = [ProtoField "message" 1 ProtoString]

-- CollectTypeDef: enable automatic message collection
instance {-# OVERLAPPING #-} CollectTypeDef GreetRequest where
  collectTypeDef = [MessageDef (protoMessageName @GreetRequest) (protoFields @GreetRequest)]
instance {-# OVERLAPPING #-} CollectTypeDef GreetReply where
  collectTypeDef = [MessageDef (protoMessageName @GreetReply) (protoFields @GreetReply)]
instance {-# OVERLAPPING #-} CollectTypeDef HealthReply where
  collectTypeDef = [MessageDef (protoMessageName @HealthReply) (protoFields @HealthReply)]


-- ===================================================================
-- 2. API type — one type, four interpretations
-- ===================================================================

type GreeterAPI =
  '[ Describe "Health check endpoint"
       (Get (At "health") (Json HealthReply))
   , Describe "Greet a user by name"
       (WithParams '[QP "formal" Bool]
         (PostCreated (At "greet") (Json GreetRequest) (Json GreetReply)))
   ]


-- ===================================================================
-- 3. Handlers
-- ===================================================================

healthHandler :: IO (Json HealthReply)
healthHandler = pure (Json (HealthReply "serving"))

greetHandler :: JsonBody GreetRequest -> IO (Json GreetReply)
greetHandler (JsonBody (GreetRequest n)) =
  pure (Json (GreetReply ("Hello, " <> n <> "!")))


-- ===================================================================
-- 4. Build REST + gRPC, multiplex, run on HTTP/2
-- ===================================================================

main :: IO ()
main = do
  putStrLn "=== One type, four interpretations ===\n"

  -- 1. REST service (ByteString-based, then adapted to Body)
  let restSvc = mkApi @GreeterAPI (healthHandler, greetHandler)
      restBody = adaptToBody restSvc

  -- 2. gRPC service map + reflection + health check
  let proto = generateProto @GreeterAPI "greeter" "GreeterService"
      grpcSvcMap = withReflection
          [ ServiceInfo "greeter.GreeterService"
              [ MethodInfo "Health" "Empty" "HealthReply"
              , MethodInfo "Greet"  "GreetRequest" "GreetReply"
              ] proto
          ]
        $ withHealthCheck (pure Serving)
        $ mkGrpcServiceMap @GreeterAPI "greeter" "GreeterService"
          ( grpcHealthHandler, grpcGreetHandler )
      grpcSvc = grpcServer grpcSvcMap

  -- 3. OpenAPI 3.1 spec
  let spec = generateSpec @GreeterAPI "Greeter API" "1.0"
  putStrLn "3. OpenAPI 3.1 spec:"
  LBS.putStr (Aeson.encode spec)
  putStrLn "\n"

  -- 4. Proto3 definition
  putStrLn "4. Proto3 definition:"
  TIO.putStrLn proto
  putStrLn ""

  -- Multiplex: Content-Type routing on a single port
  let combined = multiplex restBody grpcSvc

  putStrLn "1. REST server running on :8080"
  putStrLn "2. gRPC server running on :8080 (multiplexed)"
  runServerH2 (defaultH2Config 8080) combined

  where
    grpcHealthHandler :: GrpcHandlerFn
    grpcHealthHandler _req = pure (Right (grpcEncode (HealthReply "serving")))

    grpcGreetHandler :: GrpcHandlerFn
    grpcGreetHandler req = case grpcDecode req of
      Just (GreetRequest n) ->
        pure (Right (grpcEncode (GreetReply ("Hello, " <> n <> "!"))))
      Nothing ->
        pure (Left "invalid GreetRequest")


-- ===================================================================
-- Helpers
-- ===================================================================

encodeUtf8 :: Text -> ByteString
encodeUtf8 = TE.encodeUtf8

decodeUtf8 :: ByteString -> Text
decodeUtf8 = TE.decodeUtf8
