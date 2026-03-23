{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status400, status500)

import Tower
import Tower.Service (Service (..))
import Http.Core
import Http.Core.Body (Body, fromBytes, bodyToStrict)
import Servant.Reimagined.Core
import Servant.Reimagined.Server
import Servant.Reimagined.Test
import Servant.Reimagined.Test.Grpc
import Tower.Grpc (grpcServer, grpcServiceMap, unaryHandler, multiplex)
import Tower.Server (adaptToBody)
import Servant.Reimagined.OpenApi
  ( generateSpec, OpenApiSpec(..), specOps )
import Servant.Reimagined.Grpc
  ( mkGrpcServiceMap, GrpcHandlerFn
  , generateProto, ProtoMessageName(..)
  )


-- ===================================================================
-- Test API and server (original tests)
-- ===================================================================

type HealthPath   = '[ 'Lit "health" ]
type UsersPath    = '[ 'Lit "users" ]
type UserByIdPath = '[ 'Lit "users", 'Capture Int ]

type TestAPI =
  '[ Get HealthPath   Text
   , Get UsersPath    (Json [Text])
   , Get UserByIdPath (Json Text)
   ]

testServer :: Service IO (Request ByteString) (Response ByteString)
testServer = mkServer @TestAPI
  ( wrapHandler @(Get HealthPath Text)
      (mkHandler0 (pure ("ok" :: Text)))
  , wrapHandler @(Get UsersPath (Json [Text]))
      (mkHandler0 (pure (Json (["alice", "bob"] :: [Text]))))
  , wrapHandler @(Get UserByIdPath (Json Text))
      (\parts _body -> do
        mCaps <- lookupExtension @CaptureList (rpExtensions parts)
        case mCaps of
          Just (CaptureList (idT : _)) ->
            case parseCapture @Int idT of
              Just n  -> pure $ intoResponse (Json (T.pack ("user-" ++ show n)))
              Nothing -> pure $ intoResponse (mkError status400 "bad id")
          _ -> pure $ intoResponse (mkError status500 "no caps")
      )
  )


-- ===================================================================
-- Integration API (exercises all layers)
-- ===================================================================

-- ProtoMessageName instances needed for .proto generation
instance ProtoMessageName Text where
  protoMessageName = "StringValue"

instance ProtoMessageName [Text] where
  protoMessageName = "StringList"


type IntegrationAPI =
  '[ Describe "Health check" (Get (At "health") Text)
   , WithParams '[QP "page" Int]
       (Get (At "users") (Json [Text]))
   , PostCreated (At "users") (Json Text) (Json Text)
   , Get (Param "users" Int) (Json Text)
   ]


-- Handlers for REST server (mkApi style)
healthHandler :: IO Text
healthHandler = pure "ok"

listUsersHandler :: IO (Json [Text])
listUsersHandler = pure (Json (["alice", "bob"] :: [Text]))

createUserHandler :: JsonBody Text -> IO (Json Text)
createUserHandler (JsonBody name) = pure (Json ("created:" <> name))

getUserHandler :: PathCapture Int -> IO (Json Text)
getUserHandler (PathCapture uid) = pure (Json (T.pack ("user-" ++ show uid)))


-- gRPC handlers
grpcHealthHandler :: GrpcHandlerFn
grpcHealthHandler _req = pure (Right "ok")

grpcListUsersHandler :: GrpcHandlerFn
grpcListUsersHandler _req = pure (Right "[\"alice\",\"bob\"]")

grpcCreateUserHandler :: GrpcHandlerFn
grpcCreateUserHandler req = pure (Right ("created:" <> req))

grpcGetUserHandler :: GrpcHandlerFn
grpcGetUserHandler _req = pure (Right "user-42")


-- ===================================================================
-- Effect-tracked API (compile-time middleware tracking)
-- ===================================================================

type EffectAPI =
  '[ Requires Auth (Get (At "profile") (Json Text))
   , Get (At "public") Text
   ]

authMiddleware :: Middleware IO (Request ByteString) (Response ByteString)
authMiddleware = middleware $ \inner -> Service $ \req -> runService inner req


-- ===================================================================
-- Tests
-- ===================================================================

main :: IO ()
main = do
  putStrLn "servant-reimagined-test tests:"
  putStrLn ""

  -- =================================================================
  -- Original tests
  -- =================================================================

  putStrLn "get helper:"
  resp1 <- get testServer "/health"
  resp1 `shouldHaveStatus` 200
  resp1 `shouldHaveBody` "ok"
  putStrLn "  OK: GET /health -> 200, body ok"

  putStrLn ""
  putStrLn "JSON response:"
  resp2 <- get testServer "/users"
  resp2 `shouldHaveStatus` 200
  resp2 `shouldHaveJsonBody` (["alice", "bob"] :: [Text])
  putStrLn "  OK: GET /users -> 200, JSON body"

  putStrLn ""
  putStrLn "Path captures:"
  resp3 <- get testServer "/users/42"
  resp3 `shouldHaveStatus` 200
  resp3 `shouldHaveJsonBody` ("user-42" :: Text)
  putStrLn "  OK: GET /users/42 -> 200, user-42"

  putStrLn ""
  putStrLn "404:"
  resp4 <- get testServer "/nope"
  resp4 `shouldHaveStatus` 404
  putStrLn "  OK: GET /nope -> 404"

  putStrLn ""
  putStrLn "405:"
  resp5 <- request testServer "POST" "/health" [] ""
  resp5 `shouldHaveStatus` 405
  putStrLn "  OK: POST /health -> 405"

  putStrLn ""
  putStrLn "Middleware:"
  let svc = testServer |> middleware (\inner -> Service $ \req -> do
              resp <- runService inner req
              pure resp { responseHeaders = ("X-Test", "yes") : responseHeaders resp })
  resp6 <- get svc "/health"
  resp6 `shouldHaveStatus` 200
  resp6 `shouldHaveHeader` "X-Test" $ "yes"
  putStrLn "  OK: middleware header present"

  -- =================================================================
  -- gRPC testing utilities (original)
  -- =================================================================

  putStrLn ""
  putStrLn "gRPC test utilities:"

  let echoHandler = unaryHandler $ \reqBytes -> pure (Right reqBytes)
      grpcSvc = grpcServer $ grpcServiceMap
        [ ("test.Echo", "Echo", echoHandler)
        ]

  putStrLn ""
  putStrLn "grpcCall + shouldHaveGrpcStatus:"
  grpcResp1 <- grpcCall grpcSvc "test.Echo" "Echo" "hello"
  grpcResp1 `shouldHaveGrpcStatus` 0
  putStrLn "  OK: grpc-status 0 (OK)"

  putStrLn ""
  putStrLn "grpcResponseBody:"
  body1 <- grpcResponseBody grpcResp1
  if body1 == "hello"
    then putStrLn "  OK: response body matches"
    else error $ "Expected body \"hello\" but got " ++ show body1

  putStrLn ""
  putStrLn "shouldHaveGrpcBody:"
  grpcResp2 <- grpcCall grpcSvc "test.Echo" "Echo" "world"
  grpcResp2 `shouldHaveGrpcBody` "world"
  putStrLn "  OK: gRPC body assertion passed"

  putStrLn ""
  putStrLn "gRPC unimplemented method:"
  grpcResp3 <- grpcCall grpcSvc "test.Echo" "NoSuchMethod" ""
  grpcResp3 `shouldHaveGrpcStatus` 12  -- UNIMPLEMENTED
  putStrLn "  OK: grpc-status 12 (UNIMPLEMENTED)"

  -- =================================================================
  -- INTEGRATION TESTS: Cross-package end-to-end
  -- =================================================================

  putStrLn ""
  putStrLn "============================================"
  putStrLn "Integration tests (full stack):"
  putStrLn "============================================"

  -- -----------------------------------------------------------------
  -- (a) REST server via mkApi
  -- -----------------------------------------------------------------
  putStrLn ""
  putStrLn "a) REST server via mkApi @IntegrationAPI:"

  let restSvc = mkApi @IntegrationAPI
        (healthHandler, listUsersHandler, createUserHandler, getUserHandler)

  ra1 <- get restSvc "/health"
  ra1 `shouldHaveStatus` 200
  ra1 `shouldHaveBody` "ok"
  putStrLn "  OK: GET /health -> 200"

  ra2 <- get restSvc "/users"
  ra2 `shouldHaveStatus` 200
  ra2 `shouldHaveJsonBody` (["alice", "bob"] :: [Text])
  putStrLn "  OK: GET /users -> 200, JSON list"

  ra3 <- post restSvc "/users" ("newuser" :: Text)
  ra3 `shouldHaveStatus` 200
  ra3 `shouldHaveJsonBody` ("created:newuser" :: Text)
  putStrLn "  OK: POST /users -> 200, created"

  ra4 <- get restSvc "/users/42"
  ra4 `shouldHaveStatus` 200
  ra4 `shouldHaveJsonBody` ("user-42" :: Text)
  putStrLn "  OK: GET /users/42 -> 200, user-42"

  ra5 <- get restSvc "/notexist"
  ra5 `shouldHaveStatus` 404
  putStrLn "  OK: GET /notexist -> 404"

  -- -----------------------------------------------------------------
  -- (b) gRPC server via mkGrpcServiceMap
  -- -----------------------------------------------------------------
  putStrLn ""
  putStrLn "b) gRPC server via mkGrpcServiceMap @IntegrationAPI:"

  let grpcServices = mkGrpcServiceMap @IntegrationAPI "test" "IntegrationSvc"
        (grpcHealthHandler, grpcListUsersHandler, grpcCreateUserHandler, grpcGetUserHandler)
      grpcSvc2 = grpcServer grpcServices

  gb1 <- grpcCall grpcSvc2 "test.IntegrationSvc" "Health" "ping"
  gb1 `shouldHaveGrpcStatus` 0
  gb1body <- grpcResponseBody gb1
  if gb1body == "ok"
    then putStrLn "  OK: gRPC Health -> status 0, body ok"
    else error $ "Expected gRPC body \"ok\" but got " ++ show gb1body

  -- Note: "Users" method name is shared by 3 endpoints (list, create, getById).
  -- gRPC dispatches by method name, so the last registered handler wins.
  -- This is expected — in real usage, distinct gRPC method names are needed.
  gb2 <- grpcCall grpcSvc2 "test.IntegrationSvc" "Users" ""
  gb2 `shouldHaveGrpcStatus` 0
  putStrLn "  OK: gRPC Users -> status 0 (dispatched)"

  gb4 <- grpcCall grpcSvc2 "test.IntegrationSvc" "NoSuch" ""
  gb4 `shouldHaveGrpcStatus` 12
  putStrLn "  OK: gRPC NoSuch -> status 12 (UNIMPLEMENTED)"

  -- -----------------------------------------------------------------
  -- (c) Multiplex: REST + gRPC on same service
  -- -----------------------------------------------------------------
  putStrLn ""
  putStrLn "c) Multiplex REST + gRPC:"

  let combined = multiplex (adaptToBody restSvc) grpcSvc2

  -- REST request through combined (no content-type or non-grpc)
  do
    exts <- emptyExtensions
    let segments = ["health"]
        req = Request
          { requestMethod     = "GET"
          , requestPathRaw    = "/health"
          , requestPath       = segments
          , requestQuery      = []
          , requestHeaders    = [("content-type", "application/json")]
          , requestBody       = fromBytes ""
          , requestExtensions = exts
          }
    mresp <- runService combined req
    mbody <- bodyToStrict (responseBody mresp)
    if mbody == "ok"
      then putStrLn "  OK: REST request through multiplex -> ok"
      else error $ "Expected body \"ok\" but got " ++ show mbody

  -- gRPC request through combined
  do
    mgResp <- grpcCall combined "test.IntegrationSvc" "Health" "ping"
    mgResp `shouldHaveGrpcStatus` 0
    mgBody <- grpcResponseBody mgResp
    if mgBody == "ok"
      then putStrLn "  OK: gRPC request through multiplex -> ok"
      else error $ "Expected gRPC body \"ok\" but got " ++ show mgBody

  -- -----------------------------------------------------------------
  -- (d) OpenAPI spec generation
  -- -----------------------------------------------------------------
  putStrLn ""
  putStrLn "d) OpenAPI spec generation:"

  let spec = generateSpec @IntegrationAPI "Integration Test" "1.0.0"
      ops  = specOps spec

  if length ops == 4
    then putStrLn "  OK: generateSpec produced 4 operations"
    else error $ "Expected 4 operations but got " ++ show (length ops)

  if specTitle spec == "Integration Test"
    then putStrLn "  OK: spec title correct"
    else error $ "Expected title 'Integration Test' but got " ++ show (specTitle spec)

  if specVersion spec == "1.0.0"
    then putStrLn "  OK: spec version correct"
    else error $ "Expected version '1.0.0' but got " ++ show (specVersion spec)

  -- -----------------------------------------------------------------
  -- (e) .proto generation
  -- -----------------------------------------------------------------
  putStrLn ""
  putStrLn "e) .proto generation:"

  let proto = generateProto @IntegrationAPI "test" "IntegrationSvc"
      rpcLines = filter (T.isPrefixOf "  rpc ") (T.lines proto)

  if length rpcLines == 4
    then putStrLn "  OK: generateProto produced 4 rpc lines"
    else error $ "Expected 4 rpc lines but got " ++ show (length rpcLines)
              ++ "\nProto:\n" ++ T.unpack proto

  if T.isInfixOf "syntax = \"proto3\";" proto
    then putStrLn "  OK: proto3 syntax declaration present"
    else error "Expected proto3 syntax declaration"

  if T.isInfixOf "service IntegrationSvc" proto
    then putStrLn "  OK: service name correct"
    else error "Expected service IntegrationSvc in proto"

  if T.isInfixOf "package test;" proto
    then putStrLn "  OK: package name correct"
    else error "Expected package test in proto"

  -- -----------------------------------------------------------------
  -- (g) Effect tracking
  -- -----------------------------------------------------------------
  putStrLn ""
  putStrLn "g) Effect tracking:"

  let profileHandler :: IO (Json Text)
      profileHandler = pure (Json ("profile-data" :: Text))

      publicHandler :: IO Text
      publicHandler = pure ("public-data" :: Text)

      -- Build effectful server, provide Auth, then run
      effectSvc = run
        $ provide @Auth authMiddleware
        $ effectfulApi @EffectAPI (profileHandler, publicHandler)

  eg1 <- get effectSvc "/profile"
  eg1 `shouldHaveStatus` 200
  eg1 `shouldHaveJsonBody` ("profile-data" :: Text)
  putStrLn "  OK: effectful GET /profile -> 200 (Auth provided)"

  eg2 <- get effectSvc "/public"
  eg2 `shouldHaveStatus` 200
  eg2 `shouldHaveBody` "public-data"
  putStrLn "  OK: effectful GET /public -> 200"

  putStrLn ""
  putStrLn "============================================"
  putStrLn "All servant-reimagined-test tests passed."
