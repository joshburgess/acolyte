-- | @http-core@ — backend-agnostic HTTP types.
--
-- Shared vocabulary types for HTTP requests and responses, independent
-- of any server backend (warp, snap, etc.) or middleware framework.
--
-- Everything above the backend adapter layer uses these types.
-- WAI, warp, and other backends are confined to adapter packages.
module Http.Core
  ( -- * Request
    Request (..)
  , RequestParts (..)
  , splitRequest
  , defaultRequest

    -- * Response
  , Response (..)
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

    -- * Extensions
  , Extensions
  , emptyExtensions
  , newExtensions
  , insertExtension
  , lookupExtension
  , deleteExtension
  , hasExtension

    -- * Re-exports from http-types
  , module Network.HTTP.Types.Method
  , module Network.HTTP.Types.Status
  , module Network.HTTP.Types.Header
  ) where

import Http.Core.Request
import Http.Core.Response
import Http.Core.Extensions

import Network.HTTP.Types.Method (Method, methodGet, methodPost, methodPut, methodDelete, methodPatch, methodHead, methodOptions)
import Network.HTTP.Types.Status (Status, status200, status201, status204, status400, status401, status403, status404, status405, status422, status500, statusCode)
import Network.HTTP.Types.Header (RequestHeaders, ResponseHeaders, Header, HeaderName, hContentType, hAccept, hAuthorization)
