-- | @servant-reimagined-openapi@ — OpenAPI 3.1 spec generation.
--
-- @
-- spec = generateSpec @MyAPI "My API" "1.0.0"
-- encodePretty spec  -- produces JSON
-- @
module Servant.Reimagined.OpenApi
  ( -- * Spec generation
    OpenApiSpec (..)
  , generateSpec
    -- * Per-endpoint
  , EndpointToOperation (..)
  , Operation (..)
  , Parameter (..)
    -- * API walking
  , ApiToOperations (..)
    -- * Schema
  , Schema (..)
  , schemaToJson
  , ToSchema (..)
  ) where

import Servant.Reimagined.OpenApi.Spec
import Servant.Reimagined.OpenApi.Schema
