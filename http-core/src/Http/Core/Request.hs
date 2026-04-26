-- | Backend-agnostic HTTP request types.
--
-- 'Request' is parameterized by body type, enabling both strict
-- (@ByteString@) and streaming request bodies. Backend adapters
-- (spire-wai, etc.) convert from their native request types to this.
module Http.Core.Request
  ( -- * Request type
    Request (..)
    -- * Request parts (for extractors)
  , RequestParts (..)
  , splitRequest
    -- * Convenience constructors
  , defaultRequest
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Network.HTTP.Types (Method, Query, RequestHeaders, methodGet)

import Http.Core.Extensions (Extensions, emptyExtensions)


-- | An HTTP request, parameterized by body type.
--
-- The body type parameter enables both strict (@ByteString@) and
-- streaming bodies. For the default server path, the body is strict
-- bytes (collected by the backend before dispatch).
data Request body = Request
  { requestMethod     :: !Method
  , requestPathRaw    :: !ByteString
  , requestPath       :: ![Text]
  , requestQuery      :: !Query
  , requestHeaders    :: !RequestHeaders
  , requestBody       :: !body
  , requestExtensions :: !Extensions
  }


-- | The non-body parts of a request, used by 'FromRequestParts' extractors.
--
-- Extractors that don't need the body (path captures, query params,
-- headers, state, auth) work on 'RequestParts'. This allows multiple
-- extractors to read from the same request without consuming the body.
--
-- The body is extracted separately by 'FromRequest' (at most once per
-- handler).
data RequestParts = RequestParts
  { rpMethod     :: !Method
  , rpPathRaw    :: !ByteString
  , rpPath       :: ![Text]
  , rpQuery      :: !Query
  , rpHeaders    :: !RequestHeaders
  , rpExtensions :: !Extensions
  }


-- | Split a strict-body request into parts + body.
--
-- This is the boundary between the router (which has the full request)
-- and the extractor system (which separates parts from body).
splitRequest :: Request ByteString -> (RequestParts, ByteString)
splitRequest req =
  ( RequestParts
      { rpMethod     = requestMethod req
      , rpPathRaw    = requestPathRaw req
      , rpPath       = requestPath req
      , rpQuery      = requestQuery req
      , rpHeaders    = requestHeaders req
      , rpExtensions = requestExtensions req
      }
  , requestBody req
  )


-- | A minimal default request for testing.
--
-- Method: GET, path: "/", empty query/headers/body.
defaultRequest :: IO (Request ByteString)
defaultRequest = do
  exts <- emptyExtensions
  pure Request
    { requestMethod     = methodGet
    , requestPathRaw    = "/"
    , requestPath       = []
    , requestQuery      = []
    , requestHeaders    = []
    , requestBody       = BS.empty
    , requestExtensions = exts
    }
