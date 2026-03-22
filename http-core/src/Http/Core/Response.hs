-- | Backend-agnostic HTTP response types.
--
-- 'Response' is parameterized by body type, enabling both strict
-- (@ByteString@) and streaming response bodies. Backend adapters
-- convert from this to their native response types.
module Http.Core.Response
  ( -- * Response type
    Response (..)
    -- * Convenience constructors
  , ok
  , created
  , noContent
  , badRequest
  , unauthorized
  , forbidden
  , notFound
  , methodNotAllowed
  , unprocessable
  , serverError
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HTTP.Types
  ( Status, ResponseHeaders
  , status200, status201, status204
  , status400, status401, status403, status404, status405, status422
  , status500
  )


-- | An HTTP response, parameterized by body type.
data Response body = Response
  { responseStatus  :: !Status
  , responseHeaders :: !ResponseHeaders
  , responseBody    :: !body
  }


-- ===================================================================
-- Convenience constructors (strict ByteString bodies)
-- ===================================================================

-- | 200 OK with a body.
ok :: ResponseHeaders -> ByteString -> Response ByteString
ok hdrs body = Response status200 hdrs body

-- | 201 Created with a body.
created :: ResponseHeaders -> ByteString -> Response ByteString
created hdrs body = Response status201 hdrs body

-- | 204 No Content (empty body).
noContent :: Response ByteString
noContent = Response status204 [] BS.empty

-- | 400 Bad Request with a body.
badRequest :: ResponseHeaders -> ByteString -> Response ByteString
badRequest hdrs body = Response status400 hdrs body

-- | 401 Unauthorized with a body.
unauthorized :: ResponseHeaders -> ByteString -> Response ByteString
unauthorized hdrs body = Response status401 hdrs body

-- | 403 Forbidden with a body.
forbidden :: ResponseHeaders -> ByteString -> Response ByteString
forbidden hdrs body = Response status403 hdrs body

-- | 404 Not Found with a body.
notFound :: ResponseHeaders -> ByteString -> Response ByteString
notFound hdrs body = Response status404 hdrs body

-- | 405 Method Not Allowed (empty body).
methodNotAllowed :: Response ByteString
methodNotAllowed = Response status405 [] BS.empty

-- | 422 Unprocessable Entity with a body.
unprocessable :: ResponseHeaders -> ByteString -> Response ByteString
unprocessable hdrs body = Response status422 hdrs body

-- | 500 Internal Server Error with a body.
serverError :: ResponseHeaders -> ByteString -> Response ByteString
serverError hdrs body = Response status500 hdrs body
