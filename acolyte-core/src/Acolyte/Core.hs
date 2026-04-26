-- | @acolyte-core@ — type-level API specification.
--
-- This module re-exports all core types for defining HTTP APIs.
-- No IO, no runtime beyond base.
module Acolyte.Core
  ( -- * Methods
    Method (..)
  , SMethod (..)
  , KnownMethod (..)
  , methodVal

    -- * Path segments
  , PathSegment (..)
  , Captures
  , CapturesTuple
  , CountCaptures
  , At
  , Param
  , At2
  , Param2
  , ParamNamed

    -- * Endpoints
  , Endpoint
  , Get
  , Post
  , Put
  , Delete
  , Patch
  , NoBody
  , Json (..)

    -- * API specification
  , type API
  , Serves
  , Length
  , type (++)

    -- * Effects (Layer 1: generic middleware tracking)
  , Auth
  , Cors
  , RateLimit
  , Tracing
  , Requires
  , HasEffect
  , RequiredEffects
  , AllEffectsProvided

    -- * Endpoint wrappers (Layer 2: per-endpoint typed dispatch)
  , Protected
  , Validated
  , Validate (..)
  , Versioned
  , ApiVersion (..)
  , Describe
  , Description
  , Named
  , AllNamed
  , EndpointNames
  , NoDuplicateNames
  , LookupNamed
  , WithParams
  , QP
  , WithHeaders
  , HH

    -- * Streaming RPC markers
  , ServerStream
  , ClientStream
  , BidiStream

    -- * Response status code annotations
  , RespondsWith
  , PostCreated
  , DeleteNoContent

    -- * Session types (WebSocket protocols)
  , SessionType (..)
  , Dual

    -- * API versioning
  , Change (..)
  , VersionedApi
  , ApplyChanges
  , IsBackwardCompatible
  , BackwardCompatible

    -- * Content negotiation
  , Negotiate
  , ContentFormat (..)
  , JsonFormat
  , XmlFormat
  , TextFormat
  , HtmlFormat
  , CsvFormat
  ) where

import Acolyte.Core.Method
import Acolyte.Core.Path
import Acolyte.Core.Endpoint
import Acolyte.Core.API
import Acolyte.Core.Effect
import Acolyte.Core.Wrapper
import Acolyte.Core.Session
import Acolyte.Core.Versioning
import Acolyte.Core.Negotiate
