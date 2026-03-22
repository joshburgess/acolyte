{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS

import Servant.Reimagined.Core
import Servant.Reimagined.OpenApi


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
  assert "op3 path /users/{id}" (opPath op3 == "/users/{id}")
  assert "op3 has path param" (length (opParameters op3) == 1)

  let param = head (opParameters op3)
  assert "param name 'id'" (paramName param == "id")
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
  assert "endpoint op: /users/{id}" (opPath op == "/users/{id}")
  assert "endpoint op: 1 param" (length (opParameters op) == 1)

testRequiresDelegate :: IO ()
testRequiresDelegate = do
  let op = toOperation @(Requires Auth (Get UserByIdPath Text))
  assert "Requires delegates: GET" (opMethod op == "GET")
  assert "Requires delegates: /users/{id}" (opPath op == "/users/{id}")

testSchema :: IO ()
testSchema = do
  assert "Int schema" (schemaType (toSchema @Int) == "integer")
  assert "Text schema" (schemaType (toSchema @Text) == "string")
  assert "Bool schema" (schemaType (toSchema @Bool) == "boolean")
  assert "[Int] schema" (schemaType (toSchema @[Int]) == "array")


main :: IO ()
main = do
  putStrLn "servant-reimagined-openapi tests:"
  putStrLn ""
  putStrLn "Spec generation:"
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
  putStrLn "All servant-reimagined-openapi tests passed."
