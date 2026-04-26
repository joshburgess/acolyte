-- | @spire-http@ — HTTP-specific middleware layers.
--
-- Built on 'spire' and 'http-core'. Backend-agnostic — works with
-- any server that speaks @spire Service IO (Request ByteString) (Response ByteString)@.
module Spire.Http
  ( -- * Security headers
    SecureHeadersConfig (..)
  , defaultSecureHeaders
  , secureHeadersLayer

    -- * Request ID
  , RequestId (..)
  , requestIdLayer
  , requestIdHeader

    -- * Tracing
  , TraceEntry (..)
  , traceLayer

    -- * CORS
  , CorsConfig (..)
  , defaultCors
  , permissiveCors
  , corsLayer

    -- * Timeout
  , timeoutLayer

    -- * Compression
  , CompressionConfig (..)
  , defaultCompression
  , compressionLayer

    -- * Static files
  , StaticConfig (..)
  , defaultStaticConfig
  , staticFilesLayer

    -- * SPA fallback
  , spaFallbackLayer
  ) where

import Spire.Http.SecureHeaders
import Spire.Http.RequestId
import Spire.Http.Trace
import Spire.Http.Cors
import Spire.Http.Timeout
import Spire.Http.Compression
import Spire.Http.Static
