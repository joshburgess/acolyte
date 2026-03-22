{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Servant.Reimagined.Codegen


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
  putStrLn "All servant-reimagined-codegen tests passed."
