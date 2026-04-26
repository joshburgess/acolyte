-- | Parse OpenAPI 3.x and Swagger 2.0 specs into the shared IR.
--
-- Auto-detects the version from the "openapi" or "swagger" field.
module Acolyte.Codegen.Parse
  ( parseSpec
  , ParseError (..)
  ) where

import Data.Aeson (Value (..), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Acolyte.Codegen.IR


-- | Errors that can occur when parsing an OpenAPI or Swagger spec.
data ParseError
  = UnknownVersion !Text
  | MissingField !Text
  | InvalidJSON !Text
  deriving (Show)


-- | Parse a JSON Value into ApiIR. Auto-detects Swagger 2.0 vs OpenAPI 3.x.
parseSpec :: Value -> Either ParseError ApiIR
parseSpec (Object obj)
  | Just (String v) <- KM.lookup "openapi" obj, "3" `T.isPrefixOf` v =
      parseOpenApi3 obj
  | Just (String v) <- KM.lookup "swagger" obj, "2" `T.isPrefixOf` v =
      parseSwagger2 obj
  | Just (String v) <- KM.lookup "openapi" obj =
      Left (UnknownVersion v)
  | Just (String v) <- KM.lookup "swagger" obj =
      Left (UnknownVersion v)
  | otherwise =
      Left (MissingField "No 'openapi' or 'swagger' version field found")
parseSpec _ = Left (InvalidJSON "Top-level value must be a JSON object")


-- ===================================================================
-- OpenAPI 3.x parser
-- ===================================================================

parseOpenApi3 :: Object -> Either ParseError ApiIR
parseOpenApi3 obj = do
  let info = getObject "info" obj
      title = fromMaybe "API" (info >>= getString "title")
      version = fromMaybe "0.0.0" (info >>= getString "version")
      paths = fromMaybe KM.empty (getObject "paths" obj >>= Just . id)
      schemasObj = getObject "components" obj
                   >>= getObjectKM "schemas"
      schemas = maybe [] parseSchemas schemasObj
  let endpoints = concatMap (parsePathItem3 (hasSecuritySchemes obj)) (KM.toList paths)
  Right ApiIR
    { apiTitle     = title
    , apiVersion   = version
    , apiEndpoints = endpoints
    , apiSchemas   = schemas
    }


parsePathItem3 :: Bool -> (Aeson.Key, Value) -> [EndpointIR]
parsePathItem3 hasSecurity (pathKey, Object pathObj) =
  let path = Key.toText pathKey
      segments = parsePath path
  in mapMaybe (\(method, mVal) -> case mVal of
      Just (Object opObj) -> Just (parseOperation3 hasSecurity method segments opObj)
      _ -> Nothing
    )
    [ (GET,     KM.lookup "get" pathObj)
    , (POST,    KM.lookup "post" pathObj)
    , (PUT,     KM.lookup "put" pathObj)
    , (DELETE,  KM.lookup "delete" pathObj)
    , (PATCH,   KM.lookup "patch" pathObj)
    ]
parsePathItem3 _ _ = []


parseOperation3 :: Bool -> HttpMethod -> [PathSegmentIR] -> Object -> EndpointIR
parseOperation3 hasSecurity method segments opObj =
  let opId = getString "operationId" opObj
      summary = getString "summary" opObj
      reqBody = getObject "requestBody" opObj >>= parseRequestBody3
      respSchema = parseResponseSchema3 opObj
      statusCode = case method of
        POST -> 201
        _    -> 200
      auth = case KM.lookup "security" opObj of
        Just (Array arr) -> not (null arr)
        _ -> hasSecurity  -- inherit global security if no local override
  in EndpointIR method segments opId summary reqBody respSchema statusCode auth


parseRequestBody3 :: Object -> Maybe SchemaIR
parseRequestBody3 obj = do
  content <- getObjectKM "content" obj
  jsonContent <- getObjectFromKM "application/json" content
  schema <- getObjectKM "schema" jsonContent
  Just (parseSchemaValue (Object (schema)))


parseResponseSchema3 :: Object -> Maybe SchemaIR
parseResponseSchema3 opObj = do
  responses <- getObjectKM "responses" opObj
  -- Try 200, 201, then default
  resp <- getObjectFromKM "200" responses
      `orElse` getObjectFromKM "201" responses
      `orElse` getObjectFromKM "default" responses
  content <- getObjectKM "content" resp
  jsonContent <- getObjectFromKM "application/json" content
  schema <- getObjectKM "schema" jsonContent
  Just (parseSchemaValue (Object (schema)))


-- ===================================================================
-- Swagger 2.0 parser
-- ===================================================================

parseSwagger2 :: Object -> Either ParseError ApiIR
parseSwagger2 obj = do
  let info = getObject "info" obj
      title = fromMaybe "API" (info >>= getString "title")
      version = fromMaybe "0.0.0" (info >>= getString "version")
      paths = fromMaybe KM.empty (getObject "paths" obj >>= Just . id)
      defsObj = getObjectKM "definitions" obj
      schemas = maybe [] parseSchemas defsObj
  let hasSec = case KM.lookup "security" obj of
        Just (Array arr) -> not (null arr)
        _ -> False
  let endpoints = concatMap (parsePathItem2 hasSec) (KM.toList paths)
  Right ApiIR
    { apiTitle     = title
    , apiVersion   = version
    , apiEndpoints = endpoints
    , apiSchemas   = schemas
    }


parsePathItem2 :: Bool -> (Aeson.Key, Value) -> [EndpointIR]
parsePathItem2 hasSec (pathKey, Object pathObj) =
  let path = Key.toText pathKey
      segments = parsePath path
  in mapMaybe (\(method, mVal) -> case mVal of
      Just (Object opObj) -> Just (parseOperation2 hasSec method segments opObj)
      _ -> Nothing
    )
    [ (GET,     KM.lookup "get" pathObj)
    , (POST,    KM.lookup "post" pathObj)
    , (PUT,     KM.lookup "put" pathObj)
    , (DELETE,  KM.lookup "delete" pathObj)
    , (PATCH,   KM.lookup "patch" pathObj)
    ]
parsePathItem2 _ _ = []


parseOperation2 :: Bool -> HttpMethod -> [PathSegmentIR] -> Object -> EndpointIR
parseOperation2 hasSec method segments opObj =
  let opId = getString "operationId" opObj
      summary = getString "summary" opObj
      reqBody = parseBodyParam2 opObj
      respSchema = parseResponseSchema2 opObj
      statusCode = case method of
        POST -> 201
        _    -> 200
      auth = case KM.lookup "security" opObj of
        Just (Array arr) -> not (null arr)
        _ -> hasSec
  in EndpointIR method segments opId summary reqBody respSchema statusCode auth


-- In Swagger 2.0, body params are in the parameters array with "in": "body"
parseBodyParam2 :: Object -> Maybe SchemaIR
parseBodyParam2 opObj = case KM.lookup "parameters" opObj of
  Just (Array params) ->
    let bodyParams = [o | Object o <- foldMap (:[]) params
                        , getString "in" o == Just "body"
                        , Just _ <- [KM.lookup "schema" o]]
    in case bodyParams of
      (bp:_) -> case KM.lookup "schema" bp of
        Just schemaVal -> Just (parseSchemaValue schemaVal)
        _ -> Nothing
      _ -> Nothing
  _ -> Nothing


parseResponseSchema2 :: Object -> Maybe SchemaIR
parseResponseSchema2 opObj = do
  responses <- getObjectKM "responses" opObj
  resp <- getObjectFromKM "200" responses
      `orElse` getObjectFromKM "201" responses
      `orElse` getObjectFromKM "default" responses
  schema <- getObjectKM "schema" resp
  Just (parseSchemaValue (Object (schema)))


-- ===================================================================
-- Shared: path parsing
-- ===================================================================

-- | Parse "/users/{id}/posts" into [LitSegment "users", CaptureSegment "id" "string", LitSegment "posts"]
parsePath :: Text -> [PathSegmentIR]
parsePath path =
  let segments = filter (not . T.null) (T.splitOn "/" path)
  in map parseSegment segments

parseSegment :: Text -> PathSegmentIR
parseSegment seg
  | Just ('{', inner) <- T.uncons seg
  , not (T.null inner)
  , T.last inner == '}' =
      let name = T.init inner
      in CaptureSegment name "string"
  | otherwise = LitSegment seg


-- ===================================================================
-- Shared: schema parsing
-- ===================================================================

parseSchemaValue :: Value -> SchemaIR
parseSchemaValue (Object obj)
  | Just ref <- getString "$ref" obj =
      SchemaRef (extractRef ref)
  | Just (String "array") <- KM.lookup "type" obj =
      let items = case KM.lookup "items" obj of
            Just v  -> parseSchemaValue v
            Nothing -> SchemaString
      in SchemaArray items
  | Just (String "integer") <- KM.lookup "type" obj = SchemaInteger
  | Just (String "number")  <- KM.lookup "type" obj = SchemaNumber
  | Just (String "boolean") <- KM.lookup "type" obj = SchemaBoolean
  | Just (String "string")  <- KM.lookup "type" obj = SchemaString
  | Just (String "object") <- KM.lookup "type" obj =
      let props = case KM.lookup "properties" obj of
            Just (Object ps) -> map (\(k, v) -> FieldIR (Key.toText k) (parseSchemaValue v) True) (KM.toList ps)
            _ -> []
      in SchemaObject "Object" props
  | otherwise = SchemaString  -- fallback
parseSchemaValue _ = SchemaString


parseSchemas :: KM.KeyMap Value -> [(Text, SchemaIR)]
parseSchemas km =
  [ (Key.toText k, parseSchemaValue v) | (k, v) <- KM.toList km ]


-- | Extract "User" from "#/definitions/User" or "#/components/schemas/User"
extractRef :: Text -> Text
extractRef ref = case reverse (T.splitOn "/" ref) of
  (x:_) -> x
  _     -> ref


-- ===================================================================
-- Helpers
-- ===================================================================

getObject :: Text -> Object -> Maybe Object
getObject key obj = case KM.lookup (Key.fromText key) obj of
  Just (Object o) -> Just o
  _ -> Nothing

getObjectKM :: Text -> Object -> Maybe (KM.KeyMap Value)
getObjectKM key obj = case KM.lookup (Key.fromText key) obj of
  Just (Object o) -> Just o
  _ -> Nothing

getObjectFromKM :: Text -> KM.KeyMap Value -> Maybe Object
getObjectFromKM key km = case KM.lookup (Key.fromText key) km of
  Just (Object o) -> Just o
  _ -> Nothing

getString :: Text -> Object -> Maybe Text
getString key obj = case KM.lookup (Key.fromText key) obj of
  Just (String s) -> Just s
  _ -> Nothing

hasSecuritySchemes :: Object -> Bool
hasSecuritySchemes obj = case KM.lookup "security" obj of
  Just (Array arr) -> not (null arr)
  _ -> False

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing  y = y
