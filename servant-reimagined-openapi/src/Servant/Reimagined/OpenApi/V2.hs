-- | Swagger / OpenAPI 2.0 spec rendering.
--
-- Generates a Swagger 2.0 JSON document from the same internal
-- 'Operation' representation that OpenAPI 3.x uses. The type-level
-- API walking is shared; only the JSON format differs.
--
-- @
-- import Servant.Reimagined.OpenApi.V2 (generateSwagger)
--
-- spec = generateSwagger @MyAPI "My API" "1.0.0" "localhost" "/api"
-- @
module Servant.Reimagined.OpenApi.V2
  ( -- * Swagger 2.0 spec
    SwaggerSpec (..)
  , generateSwagger
  ) where

import Data.Aeson (Value (..), object, (.=), ToJSON (..))
import qualified Data.Aeson.Key as Key
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T

import Servant.Reimagined.OpenApi.Spec
  ( Operation (..), Parameter (..), ApiToOperations (..)
  , opMethodLower
  )
import Servant.Reimagined.OpenApi.Schema (Schema (..), schemaToJson)


-- | A Swagger 2.0 spec.
data SwaggerSpec = SwaggerSpec
  { swaggerTitle    :: !Text
  , swaggerVersion  :: !Text
  , swaggerHost     :: !Text
  , swaggerBasePath :: !Text
  , swaggerOps      :: ![Operation]
  } deriving (Show)

instance ToJSON SwaggerSpec where
  toJSON spec = object
    [ "swagger" .= ("2.0" :: Text)
    , "info" .= object
        [ "title"   .= swaggerTitle spec
        , "version" .= swaggerVersion spec
        ]
    , "host"     .= swaggerHost spec
    , "basePath" .= swaggerBasePath spec
    , "schemes"  .= (["https", "http"] :: [Text])
    , "consumes" .= (["application/json"] :: [Text])
    , "produces" .= (["application/json"] :: [Text])
    , "paths"    .= swaggerPaths (swaggerOps spec)
    ]


-- | Group operations by path, Swagger 2.0 format.
swaggerPaths :: [Operation] -> Value
swaggerPaths ops = object
  [ Key.fromText (opPath op) .= object
      [ Key.fromText (opMethodLower op) .= swaggerOperation op ]
  | op <- ops
  ]


-- | Render a single operation in Swagger 2.0 format.
--
-- Swagger 2.0 differences from OpenAPI 3.x:
-- * Parameters include body params (not a separate requestBody)
-- * Responses use a simpler schema format
-- * No content negotiation per-operation
swaggerOperation :: Operation -> Value
swaggerOperation op = object $ concat
  [ case opOperationId op of
      Nothing  -> []
      Just oid -> ["operationId" .= oid]
  , case opSummary op of
      Nothing -> []
      Just s  -> ["summary" .= s]
  , [ "responses" .= object
        [ Key.fromText (T.pack (show (opStatusCode op))) .= responseObj ]
    ]
  , let allParams = map swaggerParam (opParameters op)
          ++ bodyParam (opRequestBody op)
    in if null allParams then []
       else ["parameters" .= allParams]
  ]
  where
    responseObj = case opResponseSchema op of
      Nothing -> object ["description" .= ("Success" :: Text)]
      Just schema -> object
        [ "description" .= ("Success" :: Text)
        , "schema" .= schemaToJson schema
        ]

    bodyParam Nothing = []
    bodyParam (Just schema) =
      [ object
          [ "in"       .= ("body" :: Text)
          , "name"     .= ("body" :: Text)
          , "required" .= True
          , "schema"   .= schemaToJson schema
          ]
      ]


-- | Render a path/query parameter in Swagger 2.0 format.
swaggerParam :: Parameter -> Value
swaggerParam p = object
  [ "name"     .= paramName p
  , "in"       .= paramIn p
  , "required" .= paramRequired p
  , "type"     .= schemaType (paramSchema p)
  ]


-- | Generate a Swagger 2.0 spec from an API type.
--
-- @
-- spec = generateSwagger @MyAPI "My API" "1.0.0" "localhost:3000" "/api"
-- @
generateSwagger
  :: forall api. ApiToOperations api
  => Text    -- ^ Title
  -> Text    -- ^ Version
  -> Text    -- ^ Host (e.g., "localhost:3000")
  -> Text    -- ^ Base path (e.g., "/api" or "/")
  -> SwaggerSpec
generateSwagger title version host basePath = SwaggerSpec
  { swaggerTitle    = title
  , swaggerVersion  = version
  , swaggerHost     = host
  , swaggerBasePath = basePath
  , swaggerOps      = toOperations @api
  }
