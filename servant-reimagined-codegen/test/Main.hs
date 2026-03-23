{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Servant.Reimagined.Codegen
import Servant.Reimagined.Codegen.Proto


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- OpenAPI 3.x test spec
-- ===================================================================

openapi3Spec :: Aeson.Value
openapi3Spec = Aeson.object
  [ "openapi" Aeson..= ("3.0.3" :: Text)
  , "info" Aeson..= Aeson.object
      [ "title" Aeson..= ("Pet Store" :: Text)
      , "version" Aeson..= ("1.0.0" :: Text)
      ]
  , "paths" Aeson..= Aeson.object
      [ "/pets" Aeson..= Aeson.object
          [ "get" Aeson..= Aeson.object
              [ "operationId" Aeson..= ("listPets" :: Text)
              , "summary" Aeson..= ("List all pets" :: Text)
              , "responses" Aeson..= Aeson.object
                  [ "200" Aeson..= Aeson.object
                      [ "description" Aeson..= ("A list of pets" :: Text)
                      , "content" Aeson..= Aeson.object
                          [ "application/json" Aeson..= Aeson.object
                              [ "schema" Aeson..= Aeson.object
                                  [ "type" Aeson..= ("array" :: Text)
                                  , "items" Aeson..= Aeson.object
                                      [ "$ref" Aeson..= ("#/components/schemas/Pet" :: Text) ]
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
          , "post" Aeson..= Aeson.object
              [ "operationId" Aeson..= ("createPet" :: Text)
              , "security" Aeson..= [Aeson.object ["bearerAuth" Aeson..= ([] :: [Text])]]
              , "requestBody" Aeson..= Aeson.object
                  [ "content" Aeson..= Aeson.object
                      [ "application/json" Aeson..= Aeson.object
                          [ "schema" Aeson..= Aeson.object
                              [ "$ref" Aeson..= ("#/components/schemas/Pet" :: Text) ]
                          ]
                      ]
                  ]
              , "responses" Aeson..= Aeson.object
                  [ "201" Aeson..= Aeson.object
                      [ "description" Aeson..= ("Pet created" :: Text) ]
                  ]
              ]
          ]
      , "/pets/{petId}" Aeson..= Aeson.object
          [ "get" Aeson..= Aeson.object
              [ "operationId" Aeson..= ("getPet" :: Text)
              , "responses" Aeson..= Aeson.object
                  [ "200" Aeson..= Aeson.object
                      [ "description" Aeson..= ("A pet" :: Text) ]
                  ]
              ]
          ]
      ]
  , "components" Aeson..= Aeson.object
      [ "schemas" Aeson..= Aeson.object
          [ "Pet" Aeson..= Aeson.object
              [ "type" Aeson..= ("object" :: Text)
              , "properties" Aeson..= Aeson.object
                  [ "id" Aeson..= Aeson.object ["type" Aeson..= ("integer" :: Text)]
                  , "name" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
                  , "tag" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
                  ]
              ]
          ]
      ]
  ]


-- ===================================================================
-- Swagger 2.0 test spec
-- ===================================================================

swagger2Spec :: Aeson.Value
swagger2Spec = Aeson.object
  [ "swagger" Aeson..= ("2.0" :: Text)
  , "info" Aeson..= Aeson.object
      [ "title" Aeson..= ("User Service" :: Text)
      , "version" Aeson..= ("2.0.0" :: Text)
      ]
  , "host" Aeson..= ("localhost:3000" :: Text)
  , "basePath" Aeson..= ("/api" :: Text)
  , "paths" Aeson..= Aeson.object
      [ "/users" Aeson..= Aeson.object
          [ "get" Aeson..= Aeson.object
              [ "operationId" Aeson..= ("listUsers" :: Text)
              , "responses" Aeson..= Aeson.object
                  [ "200" Aeson..= Aeson.object
                      [ "description" Aeson..= ("User list" :: Text)
                      , "schema" Aeson..= Aeson.object
                          [ "type" Aeson..= ("array" :: Text)
                          , "items" Aeson..= Aeson.object
                              [ "$ref" Aeson..= ("#/definitions/User" :: Text) ]
                          ]
                      ]
                  ]
              ]
          ]
      , "/users/{id}" Aeson..= Aeson.object
          [ "get" Aeson..= Aeson.object
              [ "operationId" Aeson..= ("getUser" :: Text)
              , "responses" Aeson..= Aeson.object
                  [ "200" Aeson..= Aeson.object
                      [ "description" Aeson..= ("A user" :: Text) ]
                  ]
              ]
          , "delete" Aeson..= Aeson.object
              [ "operationId" Aeson..= ("deleteUser" :: Text)
              , "security" Aeson..= [Aeson.object ["apiKey" Aeson..= ([] :: [Text])]]
              , "responses" Aeson..= Aeson.object
                  [ "200" Aeson..= Aeson.object
                      [ "description" Aeson..= ("Deleted" :: Text) ]
                  ]
              ]
          ]
      ]
  , "definitions" Aeson..= Aeson.object
      [ "User" Aeson..= Aeson.object
          [ "type" Aeson..= ("object" :: Text)
          , "properties" Aeson..= Aeson.object
              [ "id" Aeson..= Aeson.object ["type" Aeson..= ("integer" :: Text)]
              , "name" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
              , "email" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
              ]
          ]
      ]
  ]


-- ===================================================================
-- Tests
-- ===================================================================

testOpenApi3Parse :: IO ()
testOpenApi3Parse = do
  case parseSpec openapi3Spec of
    Left err -> error $ "FAIL: parse error: " ++ show err
    Right api -> do
      assert "oa3: title" (apiTitle api == "Pet Store")
      assert "oa3: version" (apiVersion api == "1.0.0")
      assert "oa3: 3 endpoints" (length (apiEndpoints api) == 3)
      assert "oa3: 1 schema" (length (apiSchemas api) == 1)

      let eps = apiEndpoints api
      assert "oa3: GET /pets" (epMethod (eps !! 0) == GET)
      assert "oa3: POST /pets has auth" (epRequiresAuth (eps !! 1))
      assert "oa3: POST /pets has request body" (isJust (epRequestBody (eps !! 1)))
      assert "oa3: GET /pets/{petId}" (epMethod (eps !! 2) == GET)
      assert "oa3: capture in path" $
        any isCapture (epPath (eps !! 2))

testSwagger2Parse :: IO ()
testSwagger2Parse = do
  case parseSpec swagger2Spec of
    Left err -> error $ "FAIL: parse error: " ++ show err
    Right api -> do
      assert "sw2: title" (apiTitle api == "User Service")
      assert "sw2: version" (apiVersion api == "2.0.0")
      assert "sw2: 3 endpoints" (length (apiEndpoints api) == 3)
      assert "sw2: 1 schema" (length (apiSchemas api) == 1)

      let eps = apiEndpoints api
      assert "sw2: GET /users" (epMethod (eps !! 0) == GET)
      assert "sw2: GET /users/{id}" (epMethod (eps !! 1) == GET)
      assert "sw2: DELETE /users/{id}" (epMethod (eps !! 2) == DELETE)
      assert "sw2: DELETE has auth" (epRequiresAuth (eps !! 2))

testCodeGen :: IO ()
testCodeGen = do
  case parseSpec openapi3Spec of
    Left err -> error $ "FAIL: parse error: " ++ show err
    Right api -> do
      let code = emitModule defaultEmitConfig api
      assert "codegen: has module declaration" (T.isInfixOf "module Generated.API" code)
      assert "codegen: has DataKinds" (T.isInfixOf "DataKinds" code)
      assert "codegen: has path type" (T.isInfixOf "type Pets" code)
      assert "codegen: has API type" (T.isInfixOf "type API =" code)
      assert "codegen: has Get" (T.isInfixOf "Get " code)
      assert "codegen: has Post" (T.isInfixOf "Post " code)
      assert "codegen: has Requires Auth" (T.isInfixOf "Requires Auth" code)
      assert "codegen: has Pet data type" (T.isInfixOf "data Pet" code)
      assert "codegen: has handler stubs" (T.isInfixOf "TODO: implement" code)

testSwagger2CodeGen :: IO ()
testSwagger2CodeGen = do
  case parseSpec swagger2Spec of
    Left err -> error $ "FAIL: parse error: " ++ show err
    Right api -> do
      let code = emitModule (defaultEmitConfig { emitModuleName = "MyApp.API" }) api
      assert "sw2 codegen: custom module name" (T.isInfixOf "module MyApp.API" code)
      assert "sw2 codegen: has User type" (T.isInfixOf "data User" code)
      assert "sw2 codegen: has Delete" (T.isInfixOf "Delete " code)
      assert "sw2 codegen: has Requires Auth for delete" (T.isInfixOf "Requires Auth" code)


isCapture :: PathSegmentIR -> Bool
isCapture (CaptureSegment _ _) = True
isCapture _ = False


-- ===================================================================
-- Proto3 test
-- ===================================================================

testProtoText :: Text
testProtoText = T.unlines
  [ "syntax = \"proto3\";"
  , "package test;"
  , ""
  , "import \"google/protobuf/empty.proto\";"
  , ""
  , "// A greeting service"
  , "service Greeter {"
  , "  rpc SayHello (HelloRequest) returns (HelloReply);"
  , "  rpc SayGoodbye (HelloRequest) returns (google.protobuf.Empty);"
  , "}"
  , ""
  , "/* Request message */"
  , "message HelloRequest {"
  , "  string name = 1;"
  , "  int32 age = 2;"
  , "  repeated string tags = 3;"
  , "  optional string nickname = 4;"
  , "}"
  , ""
  , "message HelloReply {"
  , "  string message = 1;"
  , "}"
  , ""
  , "enum Status {"
  , "  UNKNOWN = 0;"
  , "  ACTIVE = 1;"
  , "  INACTIVE = 2;"
  , "}"
  ]

testProtoParse :: IO ()
testProtoParse = do
  case parseProtoText testProtoText of
    Left err -> error $ "FAIL: proto parse error: " ++ show err
    Right pf -> do
      assert "proto: syntax is proto3" (protoSyntax pf == "proto3")
      assert "proto: package is test" (protoPackage pf == "test")
      assert "proto: 1 import" (length (protoImports pf) == 1)
      assert "proto: 1 service" (length (protoServices pf) == 1)
      assert "proto: 2 messages" (length (protoMessages pf) == 2)
      assert "proto: 1 enum" (length (protoEnums pf) == 1)

      let svc = head (protoServices pf)
      assert "proto: service name is Greeter" (psName svc == "Greeter")
      assert "proto: 2 rpcs" (length (psRpcs svc) == 2)

      let rpc1 = head (psRpcs svc)
      assert "proto: rpc name is SayHello" (prName rpc1 == "SayHello")
      assert "proto: rpc input is HelloRequest" (prInputType rpc1 == "HelloRequest")
      assert "proto: rpc output is HelloReply" (prOutputType rpc1 == "HelloReply")
      assert "proto: no client streaming" (not (prClientStream rpc1))
      assert "proto: no server streaming" (not (prServerStream rpc1))

      let msg1 = head (protoMessages pf)
      assert "proto: message name is HelloRequest" (pmName msg1 == "HelloRequest")
      assert "proto: 4 fields" (length (pmFields msg1) == 4)

      let f1 = head (pmFields msg1)
      assert "proto: field type is string" (pfFieldType f1 == "string")
      assert "proto: field name is name" (pfFieldName f1 == "name")
      assert "proto: field number is 1" (pfFieldNumber f1 == 1)

      let f3 = pmFields msg1 !! 2
      assert "proto: repeated field" (pfRepeated f3)

      let f4 = pmFields msg1 !! 3
      assert "proto: optional field" (pfOptional f4)

      let enum1 = head (protoEnums pf)
      assert "proto: enum name is Status" (peName enum1 == "Status")
      assert "proto: 3 enum values" (length (peValues enum1) == 3)

testProtoToIR :: IO ()
testProtoToIR = do
  case parseProtoText testProtoText of
    Left err -> error $ "FAIL: proto parse error: " ++ show err
    Right pf -> do
      let ir = protoToIR pf
      assert "proto IR: title is package name" (apiTitle ir == "test")
      assert "proto IR: 2 endpoints" (length (apiEndpoints ir) == 2)
      assert "proto IR: 3 schemas (2 messages + 1 enum)" (length (apiSchemas ir) == 3)

      let ep1 = head (apiEndpoints ir)
      assert "proto IR: method is POST" (epMethod ep1 == POST)
      assert "proto IR: path is /Greeter/SayHello"
        (epPath ep1 == [LitSegment "Greeter", LitSegment "SayHello"])
      assert "proto IR: has request body" (isJust (epRequestBody ep1))
      assert "proto IR: has response type" (isJust (epResponseType ep1))

      let ep2 = apiEndpoints ir !! 1
      assert "proto IR: SayGoodbye has no response (Empty)"
        (isNothing (epResponseType ep2))

testProtoCodeGen :: IO ()
testProtoCodeGen = do
  case parseProtoText testProtoText of
    Left err -> error $ "FAIL: proto parse error: " ++ show err
    Right pf -> do
      let ir = protoToIR pf
          code = emitModule defaultEmitConfig ir
      assert "proto codegen: has module declaration" (T.isInfixOf "module Generated.API" code)
      assert "proto codegen: has Post" (T.isInfixOf "Post " code)
      assert "proto codegen: has HelloRequest data type" (T.isInfixOf "data HelloRequest" code)
      assert "proto codegen: has HelloReply data type" (T.isInfixOf "data HelloReply" code)
      assert "proto codegen: has path type with Greeter" (T.isInfixOf "Greeter" code)
      assert "proto codegen: has handler stubs" (T.isInfixOf "TODO: implement" code)


main :: IO ()
main = do
  putStrLn "servant-reimagined-codegen tests:"
  putStrLn ""
  putStrLn "OpenAPI 3.x parsing:"
  testOpenApi3Parse
  putStrLn ""
  putStrLn "Swagger 2.0 parsing:"
  testSwagger2Parse
  putStrLn ""
  putStrLn "Code generation (OpenAPI 3.x):"
  testCodeGen
  putStrLn ""
  putStrLn "Code generation (Swagger 2.0):"
  testSwagger2CodeGen
  putStrLn ""
  putStrLn "Proto3 parsing:"
  testProtoParse
  putStrLn ""
  putStrLn "Proto3 -> IR conversion:"
  testProtoToIR
  putStrLn ""
  putStrLn "Proto3 code generation:"
  testProtoCodeGen
  putStrLn ""
  putStrLn "All servant-reimagined-codegen tests passed."
