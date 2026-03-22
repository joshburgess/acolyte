-- | OpenAPI 3.1 spec rendering.
--
-- Generates an OpenAPI 3.1 JSON document from the shared internal
-- 'Operation' representation.
--
-- @
-- import Servant.Reimagined.OpenApi.V3 (generateOpenApi3)
--
-- spec = generateOpenApi3 @MyAPI "My API" "1.0.0"
-- @
module Servant.Reimagined.OpenApi.V3
  ( -- * OpenAPI 3.1 spec
    OpenApi3Spec (..)
  , generateOpenApi3
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


-- | An OpenAPI 3.1 spec.
data OpenApi3Spec = OpenApi3Spec
  { oa3Title   :: !Text
  , oa3Version :: !Text
  , oa3Ops     :: ![Operation]
  } deriving (Show)

instance ToJSON OpenApi3Spec where
  toJSON spec = object
    [ "openapi" .= ("3.1.0" :: Text)
    , "info" .= object
        [ "title"   .= oa3Title spec
        , "version" .= oa3Version spec
        ]
    , "paths" .= oa3Paths (oa3Ops spec)
    ]


-- | Group operations by path, OpenAPI 3.x format.
oa3Paths :: [Operation] -> Value
oa3Paths ops = object
  [ Key.fromText (opPath op) .= object
      [ Key.fromText (opMethodLower op) .= oa3Operation op ]
  | op <- ops
  ]


-- | Render a single operation in OpenAPI 3.x format.
--
-- OpenAPI 3.x differences from Swagger 2.0:
-- * requestBody is a separate field (not a body parameter)
-- * Response content uses media type keys
-- * Richer content negotiation support
oa3Operation :: Operation -> Value
oa3Operation op = object $ concat
  [ [ "responses" .= object
        [ Key.fromText (T.pack (show (opStatusCode op))) .= responseObj ]
    ]
  , if null (opParameters op) then []
    else ["parameters" .= map oa3Param (opParameters op)]
  , case opRequestBody op of
      Nothing -> []
      Just schema ->
        [ "requestBody" .= object
            [ "required" .= True
            , "content" .= object
                [ "application/json" .= object
                    [ "schema" .= schemaToJson schema ]
                ]
            ]
        ]
  ]
  where
    responseObj = case opResponseSchema op of
      Nothing -> object ["description" .= ("Success" :: Text)]
      Just schema -> object
        [ "description" .= ("Success" :: Text)
        , "content" .= object
            [ "application/json" .= object
                [ "schema" .= schemaToJson schema ]
            ]
        ]


-- | Render a parameter in OpenAPI 3.x format.
oa3Param :: Parameter -> Value
oa3Param p = object
  [ "name"     .= paramName p
  , "in"       .= paramIn p
  , "required" .= paramRequired p
  , "schema"   .= schemaToJson (paramSchema p)
  ]


-- | Generate an OpenAPI 3.1 spec from an API type.
generateOpenApi3
  :: forall api. ApiToOperations api
  => Text -> Text -> OpenApi3Spec
generateOpenApi3 title version = OpenApi3Spec
  { oa3Title   = title
  , oa3Version = version
  , oa3Ops     = toOperations @api
  }
