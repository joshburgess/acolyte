-- | @acolyte-openapi@ — OpenAPI spec generation.
--
-- Supports both Swagger 2.0 and OpenAPI 3.1 from the same API type.
--
-- @
-- -- OpenAPI 3.1 (default)
-- spec3 = generateSpec @MyAPI "My API" "1.0.0"
--
-- -- Swagger 2.0
-- spec2 = generateSwagger @MyAPI "My API" "1.0.0" "localhost:3000" "/"
-- @
module Acolyte.OpenApi
  ( -- * OpenAPI 3.1 (default)
    OpenApiSpec (..)
  , generateSpec

    -- * OpenAPI 3.1 (explicit)
  , OpenApi3Spec (..)
  , generateOpenApi3

    -- * Swagger / OpenAPI 2.0
  , SwaggerSpec (..)
  , generateSwagger

    -- * Shared: per-endpoint
  , EndpointToOperation (..)
  , Operation (..)
  , Parameter (..)

    -- * Shared: API walking
  , ApiToOperations (..)

    -- * Shared: schema
  , Schema (..)
  , schemaToJson
  , ToSchema (..)
  ) where

import Acolyte.OpenApi.Spec
import Acolyte.OpenApi.Schema
import Acolyte.OpenApi.V2
import Acolyte.OpenApi.V3
