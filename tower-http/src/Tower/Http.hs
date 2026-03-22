-- | @tower-http@ — HTTP-specific middleware layers.
--
-- Built on 'tower' and 'http-core'. Backend-agnostic — works with
-- any server that speaks @tower Service IO (Request ByteString) (Response ByteString)@.
module Tower.Http
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
  ) where

import Tower.Http.SecureHeaders
import Tower.Http.RequestId
import Tower.Http.Trace
import Tower.Http.Cors
import Tower.Http.Timeout
import Tower.Http.Compression
