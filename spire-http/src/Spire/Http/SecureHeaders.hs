-- | OWASP-recommended security headers middleware.
--
-- Adds security headers to every response. Configurable via record
-- update on 'SecureHeadersConfig'.
--
-- Default headers:
--
-- * @X-Content-Type-Options: nosniff@
-- * @X-Frame-Options: DENY@
-- * @X-XSS-Protection: 0@ (modern browsers don't need it; can cause issues)
-- * @Referrer-Policy: strict-origin-when-cross-origin@
-- * @Content-Security-Policy: default-src 'self'@
-- * @Permissions-Policy: camera=(), microphone=(), geolocation=()@
--
-- Optional: @Strict-Transport-Security@ (HSTS) — off by default since
-- it requires HTTPS.
module Spire.Http.SecureHeaders
  ( -- * Config
    SecureHeadersConfig (..)
  , defaultSecureHeaders
    -- * Layer
  , secureHeadersLayer
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (ResponseHeaders, Header)

import Spire (Middleware, middleware)
import Spire.Service (Service (..), mapResponse)
import Http.Core (Request, Response (..))


-- | Configuration for security headers.
data SecureHeadersConfig = SecureHeadersConfig
  { xContentTypeOptions :: !ByteString
  , xFrameOptions       :: !ByteString
  , xXssProtection      :: !ByteString
  , referrerPolicy      :: !ByteString
  , contentSecurityPolicy :: !ByteString
  , permissionsPolicy   :: !ByteString
  , strictTransportSecurity :: !(Maybe ByteString)
    -- ^ @Nothing@ = don't add HSTS. @Just "max-age=63072000"@ for 2 years.
  }


-- | Sensible defaults matching OWASP recommendations.
defaultSecureHeaders :: SecureHeadersConfig
defaultSecureHeaders = SecureHeadersConfig
  { xContentTypeOptions     = "nosniff"
  , xFrameOptions           = "DENY"
  , xXssProtection          = "0"
  , referrerPolicy          = "strict-origin-when-cross-origin"
  , contentSecurityPolicy   = "default-src 'self'"
  , permissionsPolicy       = "camera=(), microphone=(), geolocation=()"
  , strictTransportSecurity = Nothing
  }


-- | Build the header list from config.
configHeaders :: SecureHeadersConfig -> ResponseHeaders
configHeaders cfg = base ++ hsts
  where
    base =
      [ ("X-Content-Type-Options",  xContentTypeOptions cfg)
      , ("X-Frame-Options",         xFrameOptions cfg)
      , ("X-XSS-Protection",        xXssProtection cfg)
      , ("Referrer-Policy",         referrerPolicy cfg)
      , ("Content-Security-Policy", contentSecurityPolicy cfg)
      , ("Permissions-Policy",      permissionsPolicy cfg)
      ]
    hsts = case strictTransportSecurity cfg of
      Nothing  -> []
      Just val -> [("Strict-Transport-Security", val)]


-- | Middleware that adds security headers to every response.
secureHeadersLayer
  :: SecureHeadersConfig
  -> Middleware IO (Request ByteString) (Response ByteString)
secureHeadersLayer cfg = middleware $ \inner ->
  mapResponse addHeaders inner
  where
    hdrs = configHeaders cfg
    addHeaders resp = resp
      { responseHeaders = responseHeaders resp ++ hdrs }
