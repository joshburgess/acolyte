-- | CORS (Cross-Origin Resource Sharing) middleware.
--
-- Handles preflight OPTIONS requests automatically and adds
-- Access-Control-* headers to all responses.
module Tower.Http.Cors
  ( -- * Config
    CorsConfig (..)
  , defaultCors
  , permissiveCors
    -- * Layer
  , corsLayer
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types (status200, status204, Header, ResponseHeaders)

import Tower (Middleware, middleware)
import Tower.Service (Service (..))
import Http.Core (Request (..), Response (..))


-- | CORS configuration.
data CorsConfig = CorsConfig
  { corsAllowOrigins :: ![ByteString]
    -- ^ Allowed origins. @["*"]@ allows all.
  , corsAllowMethods :: ![ByteString]
    -- ^ Allowed methods for preflight.
  , corsAllowHeaders :: ![ByteString]
    -- ^ Allowed request headers for preflight.
  , corsExposeHeaders :: ![ByteString]
    -- ^ Response headers the browser can access.
  , corsMaxAge :: !(Maybe Int)
    -- ^ Preflight cache duration in seconds.
  , corsAllowCredentials :: !Bool
    -- ^ Allow cookies/auth headers.
  }

-- | Restrictive defaults: no origins allowed.
defaultCors :: CorsConfig
defaultCors = CorsConfig
  { corsAllowOrigins     = []
  , corsAllowMethods     = ["GET", "POST", "PUT", "DELETE", "PATCH"]
  , corsAllowHeaders     = ["Content-Type", "Authorization", "Accept"]
  , corsExposeHeaders    = []
  , corsMaxAge           = Just 86400
  , corsAllowCredentials = False
  }

-- | Allow everything. Good for development.
permissiveCors :: CorsConfig
permissiveCors = defaultCors
  { corsAllowOrigins     = ["*"]
  , corsAllowHeaders     = ["*"]
  , corsAllowCredentials = False
  }


-- | CORS middleware.
corsLayer :: CorsConfig -> Middleware IO (Request ByteString) (Response ByteString)
corsLayer cfg = middleware $ \(Service inner) -> Service $ \req ->
  if requestMethod req == "OPTIONS"
    then pure (preflightResponse cfg req)
    else do
      resp <- inner req
      pure (addCorsHeaders cfg req resp)


-- | Build preflight response.
preflightResponse :: CorsConfig -> Request ByteString -> Response ByteString
preflightResponse cfg req = Response status204 hdrs ""
  where
    hdrs = corsHeaders cfg req ++
      maybe [] (\age -> [("Access-Control-Max-Age", BS8.pack (show age))]) (corsMaxAge cfg)


-- | Add CORS headers to a normal response.
addCorsHeaders :: CorsConfig -> Request ByteString -> Response ByteString -> Response ByteString
addCorsHeaders cfg req resp =
  resp { responseHeaders = responseHeaders resp ++ corsHeaders cfg req }


-- | Common CORS headers.
corsHeaders :: CorsConfig -> Request ByteString -> ResponseHeaders
corsHeaders cfg req =
  [ ("Access-Control-Allow-Origin", origin)
  , ("Access-Control-Allow-Methods", BS8.intercalate ", " (corsAllowMethods cfg))
  , ("Access-Control-Allow-Headers", BS8.intercalate ", " (corsAllowHeaders cfg))
  ] ++
  [ ("Access-Control-Expose-Headers", BS8.intercalate ", " (corsExposeHeaders cfg))
  | not (null (corsExposeHeaders cfg)) ] ++
  [ ("Access-Control-Allow-Credentials", "true")
  | corsAllowCredentials cfg ]
  where
    origin
      | corsAllowOrigins cfg == ["*"] = "*"
      | otherwise = case lookup "origin" (requestHeaders req) of
          Just o | o `elem` corsAllowOrigins cfg -> o
          _ -> ""
