-- | @servant-reimagined-core@ — type-level API specification.
--
-- This module re-exports all core types for defining HTTP APIs.
-- No IO, no runtime beyond base.
module Servant.Reimagined.Core
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

    -- * Endpoints
  , Endpoint
  , Get
  , Post
  , Put
  , Delete
  , Patch
  , NoBody

    -- * API specification
  , type API
  , Serves (..)
  , Length

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

import Servant.Reimagined.Core.Method
import Servant.Reimagined.Core.Path
import Servant.Reimagined.Core.Endpoint
import Servant.Reimagined.Core.API
import Servant.Reimagined.Core.Effect
import Servant.Reimagined.Core.Wrapper
import Servant.Reimagined.Core.Session
import Servant.Reimagined.Core.Versioning
import Servant.Reimagined.Core.Negotiate
