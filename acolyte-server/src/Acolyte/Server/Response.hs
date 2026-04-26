-- | Response encoding: converting handler return values to HTTP responses.
module Acolyte.Server.Response
  ( -- * Response class
    IntoResponse (..)
    -- * JSON response wrapper
  , Json (..)
    -- * Structured error
  , JsonError (..)
  , jsonError
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
  ( Status, status200, status400, status401, status403, status404
  , status422, status500, statusCode
  )

import qualified Data.Aeson as Aeson

import Http.Core (Response (..))
import Acolyte.Core.Endpoint (Json (..))
import Acolyte.Server.Extract (ServerError (..))


-- | Convert a value into an HTTP 'Response'.
--
-- Handlers can return any type with an 'IntoResponse' instance.
-- The endpoint's declared response type (for OpenAPI/clients) is
-- separate — see 'CompatibleWith' (future).
class IntoResponse a where
  intoResponse :: a -> Response ByteString


-- | Passthrough: Response is already a Response.
instance IntoResponse (Response ByteString) where
  intoResponse = id

-- | Plain text from ByteString.
instance IntoResponse ByteString where
  intoResponse bs = Response status200 [("Content-Type", "text/plain")] bs

-- | Plain text from Text.
instance IntoResponse Text where
  intoResponse t = Response status200 [("Content-Type", "text/plain; charset=utf-8")] (TE.encodeUtf8 t)

-- | Status code only (empty body).
instance IntoResponse Status where
  intoResponse s = Response s [] ""

-- | Status + body tuple.
instance IntoResponse a => IntoResponse (Status, a) where
  intoResponse (s, a) = (intoResponse a) { responseStatus = s }

-- | Either: Left is error response, Right is success response.
instance (IntoResponse e, IntoResponse a) => IntoResponse (Either e a) where
  intoResponse (Left e)  = intoResponse e
  intoResponse (Right a) = intoResponse a

-- | ServerError as a JSON error response.
instance IntoResponse ServerError where
  intoResponse (ServerError s msg) =
    let body = Aeson.encode $ Aeson.object
          [ "error" Aeson..= Aeson.object
            [ "status"  Aeson..= statusCode s
            , "message" Aeson..= msg
            ]
          ]
    in Response s [("Content-Type", "application/json")] (LBS.toStrict body)


-- ===================================================================
-- Json: JSON response wrapper
-- ===================================================================

-- | A JSON-serialized response with @application/json@ content type.
--
-- @
-- handler :: IO (Json User)
-- handler = pure (Json myUser)
-- @

instance Aeson.ToJSON a => IntoResponse (Json a) where
  intoResponse (Json val) =
    let body = LBS.toStrict
             $ Builder.toLazyByteStringWith
                 (Builder.safeStrategy 256 256)
                 LBS.empty
             $ Aeson.fromEncoding (Aeson.toEncoding val)
    in Response status200 [("Content-Type", "application/json")] body


-- ===================================================================
-- JsonError: structured JSON error responses
-- ===================================================================

-- | A structured JSON error response.
data JsonError = JsonError
  { jeStatus  :: !Status
  , jeMessage :: !Text
  } deriving (Show)

-- | Construct a 'JsonError' from a status code and message.
jsonError :: Status -> Text -> JsonError
jsonError = JsonError

instance IntoResponse JsonError where
  intoResponse (JsonError s msg) =
    let body = Aeson.encode $ Aeson.object
          [ "error" Aeson..= Aeson.object
            [ "status"  Aeson..= statusCode s
            , "message" Aeson..= msg
            ]
          ]
    in Response s [("Content-Type", "application/json")] (LBS.toStrict body)
