{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)

import Acolyte.Core
import Acolyte.OpenApi


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- API types
-- ===================================================================

type HealthPath   = '[ 'Lit "health" ]
type UsersPath    = '[ 'Lit "users" ]
type UserByIdPath = '[ 'Lit "users", 'Capture Int ]

type TestAPI =
  '[ Get  HealthPath   Text
   , Get  UsersPath    Text
   , Get  UserByIdPath Text
   , Post UsersPath    Text Text
   ]


-- ===================================================================
-- Tests
-- ===================================================================

testGenerateSpec :: IO ()
testGenerateSpec = do
  let spec = generateSpec @TestAPI "Test API" "1.0.0"
  assert "spec title" (specTitle spec == "Test API")
  assert "spec version" (specVersion spec == "1.0.0")
  assert "spec has 4 operations" (length (specOps spec) == 4)

testOperations :: IO ()
testOperations = do
  let ops = toOperations @TestAPI
  assert "4 operations" (length ops == 4)

  let op1 = ops !! 0
  assert "op1 method GET" (opMethod op1 == "GET")
  assert "op1 path /health" (opPath op1 == "/health")
  assert "op1 no params" (null (opParameters op1))

  let op3 = ops !! 2
  assert "op3 method GET" (opMethod op3 == "GET")
  assert "op3 path /users/{int}" (opPath op3 == "/users/{int}")
  assert "op3 has path param" (length (opParameters op3) == 1)

  let param = head (opParameters op3)
  assert "param name 'int'" (paramName param == "int")
  assert "param in 'path'" (paramIn param == "path")
  assert "param required" (paramRequired param)

testJsonOutput :: IO ()
testJsonOutput = do
  let spec = generateSpec @TestAPI "Test API" "1.0.0"
      json = Aeson.toJSON spec
  -- Should produce valid JSON
  case json of
    Aeson.Object obj -> do
      assert "has openapi field" (KM.member "openapi" obj)
      assert "has info field" (KM.member "info" obj)
      assert "has paths field" (KM.member "paths" obj)
    _ -> error "FAIL: spec is not a JSON object"

testEndpointToOperation :: IO ()
testEndpointToOperation = do
  let op = toOperation @(Get UserByIdPath Text)
  assert "endpoint op: GET" (opMethod op == "GET")
  assert "endpoint op: /users/{int}" (opPath op == "/users/{int}")
  assert "endpoint op: 1 param" (length (opParameters op) == 1)
  assert "endpoint op: status 200" (opStatusCode op == 200)

  -- POST gets 201
  let op2 = toOperation @(Post UsersPath Text Text)
  assert "POST status 201" (opStatusCode op2 == 201)

testRequiresDelegate :: IO ()
testRequiresDelegate = do
  let op = toOperation @(Requires Auth (Get UserByIdPath Text))
  assert "Requires delegates: GET" (opMethod op == "GET")
  assert "Requires delegates: /users/{int}" (opPath op == "/users/{int}")

testSchema :: IO ()
testSchema = do
  assert "Int schema" (schemaType (toSchema @Int) == "integer")
  assert "Text schema" (schemaType (toSchema @Text) == "string")
  assert "Bool schema" (schemaType (toSchema @Bool) == "boolean")
  assert "[Int] schema" (schemaType (toSchema @[Int]) == "array")


-- ===================================================================
-- Swagger 2.0 tests
-- ===================================================================

testSwagger :: IO ()
testSwagger = do
  let spec = generateSwagger @TestAPI "Test API" "1.0.0" "localhost:3000" "/api"
      json = Aeson.toJSON spec
  case json of
    Aeson.Object obj -> do
      assert "swagger: has swagger field" (KM.member "swagger" obj)
      assert "swagger: has host field" (KM.member "host" obj)
      assert "swagger: has basePath field" (KM.member "basePath" obj)
      assert "swagger: has paths field" (KM.member "paths" obj)
      assert "swagger: has consumes" (KM.member "consumes" obj)
      -- Verify swagger version
      case KM.lookup "swagger" obj of
        Just (Aeson.String v) -> assert "swagger: version 2.0" (v == "2.0")
        _ -> error "FAIL: swagger field not a string"
    _ -> error "FAIL: swagger spec is not a JSON object"

-- ===================================================================
-- OpenAPI 3.1 explicit tests
-- ===================================================================

testOpenApi3 :: IO ()
testOpenApi3 = do
  let spec = generateOpenApi3 @TestAPI "Test API" "1.0.0"
      json = Aeson.toJSON spec
  case json of
    Aeson.Object obj -> do
      assert "oa3: has openapi field" (KM.member "openapi" obj)
      assert "oa3: has paths field" (KM.member "paths" obj)
      case KM.lookup "openapi" obj of
        Just (Aeson.String v) -> assert "oa3: version 3.1.0" (v == "3.1.0")
        _ -> error "FAIL: openapi field not a string"
    _ -> error "FAIL: openapi3 spec is not a JSON object"


-- ===================================================================
-- Named endpoint operationId tests
-- ===================================================================

testNamedOperationId :: IO ()
testNamedOperationId = do
  -- Named endpoint produces operationId
  let op = toOperation @(Named "getUser" (Get UserByIdPath Text))
  assert "Named: operationId set" (opOperationId op == Just "getUser")
  assert "Named: method delegated" (opMethod op == "GET")
  assert "Named: path delegated" (opPath op == "/users/{int}")

  -- Unnamed endpoint has no operationId
  let op2 = toOperation @(Get HealthPath Text)
  assert "Unnamed: no operationId" (opOperationId op2 == Nothing)

  -- Named + Describe stacking
  let op3 = toOperation @(Named "health" (Describe "Health check" (Get HealthPath Text)))
  assert "Named+Describe: operationId set" (opOperationId op3 == Just "health")
  assert "Named+Describe: method GET" (opMethod op3 == "GET")

  -- Named + Requires stacking
  let op4 = toOperation @(Named "secureGet" (Requires Auth (Get HealthPath Text)))
  assert "Named+Requires: operationId set" (opOperationId op4 == Just "secureGet")

testNamedApiSpec :: IO ()
testNamedApiSpec = do
  -- Full API with Named endpoints generates operationIds
  let ops = toOperations @'[ Named "listUsers" (Get UsersPath Text)
                            , Named "getUser" (Get UserByIdPath Text)
                            , Named "createUser" (Post UsersPath Text Text)
                            ]
  assert "named API: 3 operations" (length ops == 3)
  assert "named API: op1 id" (opOperationId (ops !! 0) == Just "listUsers")
  assert "named API: op2 id" (opOperationId (ops !! 1) == Just "getUser")
  assert "named API: op3 id" (opOperationId (ops !! 2) == Just "createUser")

  -- JSON output includes operationId
  let spec = generateSpec @'[ Named "health" (Get HealthPath Text) ] "Test" "1.0"
      json = Aeson.toJSON spec
      bs = LBS.toStrict (Aeson.encode json)
  assert "named spec JSON contains operationId" (T.isInfixOf "operationId" (T.pack (show bs)))


-- ===================================================================
-- Describe test
-- ===================================================================

testDescribe :: IO ()
testDescribe = do
  let op = toOperation @(Describe "Health check" (Get HealthPath Text))
  assert "Describe: summary set" (opSummary op == Just "Health check")
  assert "Describe: method delegated" (opMethod op == "GET")
  assert "Describe: path delegated" (opPath op == "/health")


-- ===================================================================
-- RespondsWith test
-- ===================================================================

testRespondsWith :: IO ()
testRespondsWith = do
  let op = toOperation @(RespondsWith 204 (Post (At "items") Text Text))
  assert "RespondsWith: status 204" (opStatusCode op == 204)
  assert "RespondsWith: method POST" (opMethod op == "POST")
  assert "RespondsWith: path /items" (opPath op == "/items")


-- ===================================================================
-- WithParams test
-- ===================================================================

testWithParams :: IO ()
testWithParams = do
  let op = toOperation @(WithParams '[QP "page" Int, QP "limit" Int] (Get (At "users") Text))
  assert "WithParams: 2 query params" (length (opParameters op) == 2)
  assert "WithParams: first param 'page'" (paramName (opParameters op !! 0) == "page")
  assert "WithParams: second param 'limit'" (paramName (opParameters op !! 1) == "limit")
  assert "WithParams: params are query" (all (\p -> paramIn p == "query") (opParameters op))


-- ===================================================================
-- WithHeaders test
-- ===================================================================

testWithHeaders :: IO ()
testWithHeaders = do
  let op = toOperation @(WithHeaders '[HH "Authorization" Text] (Get (At "users") Text))
  assert "WithHeaders: 1 header param" (length (opParameters op) == 1)
  assert "WithHeaders: header name 'Authorization'" (paramName (head (opParameters op)) == "Authorization")
  assert "WithHeaders: param in 'header'" (paramIn (head (opParameters op)) == "header")


-- ===================================================================
-- Versioned test
-- ===================================================================

data V1
instance ApiVersion V1 where versionPrefix = "v1"

testVersionedOpenApi :: IO ()
testVersionedOpenApi = do
  let op = toOperation @(Versioned V1 (Get HealthPath Text))
  assert "Versioned: path starts with /v1" (T.isPrefixOf "/v1" (opPath op))
  assert "Versioned: full path /v1/health" (opPath op == "/v1/health")
  assert "Versioned: method delegated" (opMethod op == "GET")


-- ===================================================================
-- Stacking wrappers test
-- ===================================================================

testStackedWrappers :: IO ()
testStackedWrappers = do
  let op = toOperation @(Named "x" (Describe "y" (WithParams '[QP "p" Int] (Get (At "z") Text))))
  assert "Stacked: operationId set" (opOperationId op == Just "x")
  assert "Stacked: summary set" (opSummary op == Just "y")
  assert "Stacked: has query param" (length (opParameters op) == 1)
  assert "Stacked: param name 'p'" (paramName (head (opParameters op)) == "p")
  assert "Stacked: method GET" (opMethod op == "GET")
  assert "Stacked: path /z" (opPath op == "/z")


-- ===================================================================
-- Generic ToSchema derivation test
-- ===================================================================

data TestUser = TestUser { userName :: Text, userAge :: Int }
  deriving (Generic)

instance ToSchema TestUser

testGenericSchema :: IO ()
testGenericSchema = do
  let schema = toSchema @TestUser
  assert "Generic: type is object" (schemaType schema == "object")
  assert "Generic: has 2 properties" (length (schemaProperties schema) == 2)
  let props = schemaProperties schema
  assert "Generic: has userName" (any (\(k, _) -> k == "userName") props)
  assert "Generic: has userAge" (any (\(k, _) -> k == "userAge") props)
  case lookup "userName" props of
    Just s  -> assert "Generic: userName is string" (schemaType s == "string")
    Nothing -> error "FAIL: userName property missing"
  case lookup "userAge" props of
    Just s  -> assert "Generic: userAge is integer" (schemaType s == "integer")
    Nothing -> error "FAIL: userAge property missing"


-- ===================================================================
-- Body schema tests
-- ===================================================================

testBodySchemas :: IO ()
testBodySchemas = do
  -- GET response schema
  let op1 = toOperation @(Get HealthPath Text)
  assert "body: GET has response schema" (opResponseSchema op1 /= Nothing)
  case opResponseSchema op1 of
    Just s  -> assert "body: GET response is string" (schemaType s == "string")
    Nothing -> error "FAIL: no response schema"
  assert "body: GET has no request body" (opRequestBody op1 == Nothing)

  -- POST request + response schemas
  let op2 = toOperation @(Post (At "users") Text Text)
  assert "body: POST has response schema" (opResponseSchema op2 /= Nothing)
  assert "body: POST has request body" (opRequestBody op2 /= Nothing)

  -- Json unwrapping
  let op3 = toOperation @(Get HealthPath (Json [Int]))
  case opResponseSchema op3 of
    Just s  -> assert "body: Json [Int] is array" (schemaType s == "array")
    Nothing -> error "FAIL: no response schema for Json"

  -- Generic type in endpoint
  let op4 = toOperation @(Post (At "users") (Json TestUser) (Json TestUser))
  case opRequestBody op4 of
    Just s  -> assert "body: Generic type in request" (schemaType s == "object")
    Nothing -> error "FAIL: no request body for Generic type"
  case opResponseSchema op4 of
    Just s  -> assert "body: Generic type in response" (length (schemaProperties s) == 2)
    Nothing -> error "FAIL: no response schema for Generic type"


-- ===================================================================
-- CaptureNamed test
-- ===================================================================

testCaptureNamed :: IO ()
testCaptureNamed = do
  let op = toOperation @(Get (ParamNamed "userId" "users" Int) Text)
  assert "CaptureNamed: path has named param" (opPath op == "/users/{userId}")
  assert "CaptureNamed: param name is userId" (paramName (head (opParameters op)) == "userId")


-- ===================================================================
-- Description test
-- ===================================================================

testDescription :: IO ()
testDescription = do
  let op = toOperation @(Description "Detailed docs here" (Get HealthPath Text))
  assert "Description: opDescription set" (opDescription op == Just "Detailed docs here")
  assert "Description: summary unset" (opSummary op == Nothing)
  -- Describe + Description stacking
  let op2 = toOperation @(Describe "Short" (Description "Long" (Get HealthPath Text)))
  assert "Describe+Description: summary" (opSummary op2 == Just "Short")
  assert "Describe+Description: description" (opDescription op2 == Just "Long")


-- ===================================================================
-- Sum type / enum schema test
-- ===================================================================

data Color = Red | Green | Blue deriving (Generic)
instance ToSchema Color

data Shape = Circle { radius :: Double } | Rectangle { width :: Double, height :: Double }
  deriving (Generic)
instance ToSchema Shape

testSumTypeSchema :: IO ()
testSumTypeSchema = do
  -- Sum type with constructors produces oneOf
  let schema = toSchema @Shape
  assert "Sum type: has oneOf" (schemaOneOf schema /= Nothing)
  case schemaOneOf schema of
    Just variants -> assert "Sum type: 2 variants" (length variants == 2)
    Nothing -> error "FAIL: expected oneOf"

  -- Either produces oneOf
  let eitherSchema = toSchema @(Either Text Int)
  assert "Either: has oneOf" (schemaOneOf eitherSchema /= Nothing)


-- ===================================================================
-- CaptureNamed body schema test
-- ===================================================================

testCaptureNamedBodySchema :: IO ()
testCaptureNamedBodySchema = do
  let op = toOperation @(Post (ParamNamed "userId" "users" Int) (Json Text) (Json Text))
  assert "CaptureNamed POST: has request body" (opRequestBody op /= Nothing)
  assert "CaptureNamed POST: has response schema" (opResponseSchema op /= Nothing)


main :: IO ()
main = do
  putStrLn "acolyte-openapi tests:"
  putStrLn ""
  putStrLn "Spec generation (default = 3.1):"
  testGenerateSpec
  putStrLn ""
  putStrLn "Operations:"
  testOperations
  putStrLn ""
  putStrLn "JSON output:"
  testJsonOutput
  putStrLn ""
  putStrLn "EndpointToOperation:"
  testEndpointToOperation
  putStrLn ""
  putStrLn "Requires delegation:"
  testRequiresDelegate
  putStrLn ""
  putStrLn "Schema:"
  testSchema
  putStrLn ""
  putStrLn "Swagger 2.0:"
  testSwagger
  putStrLn ""
  putStrLn "OpenAPI 3.1 (explicit):"
  testOpenApi3
  putStrLn ""
  putStrLn "Named endpoint operationId:"
  testNamedOperationId
  putStrLn ""
  putStrLn "Named API spec generation:"
  testNamedApiSpec
  putStrLn ""
  putStrLn "Describe wrapper:"
  testDescribe
  putStrLn ""
  putStrLn "RespondsWith wrapper:"
  testRespondsWith
  putStrLn ""
  putStrLn "WithParams wrapper:"
  testWithParams
  putStrLn ""
  putStrLn "WithHeaders wrapper:"
  testWithHeaders
  putStrLn ""
  putStrLn "Versioned wrapper (OpenAPI):"
  testVersionedOpenApi
  putStrLn ""
  putStrLn "Stacked wrappers:"
  testStackedWrappers
  putStrLn ""
  putStrLn "Generic ToSchema derivation:"
  testGenericSchema
  putStrLn ""
  putStrLn "Body schemas:"
  testBodySchemas
  putStrLn ""
  putStrLn "CaptureNamed:"
  testCaptureNamed
  putStrLn ""
  putStrLn "Description wrapper:"
  testDescription
  putStrLn ""
  putStrLn "Sum type / enum schema:"
  testSumTypeSchema
  putStrLn ""
  putStrLn "CaptureNamed body schema:"
  testCaptureNamedBodySchema
  putStrLn ""
  putStrLn "All acolyte-openapi tests passed."
