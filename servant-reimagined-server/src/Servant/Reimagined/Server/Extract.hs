-- | Request extraction: turning raw requests into typed handler arguments.
--
-- Two extraction protocols mirror the typeway/axum pattern:
--
-- * 'FromRequestParts' — extracts from method, path, query, headers,
--   extensions (can be called multiple times per request)
-- * 'FromRequest' — extracts from the body (consumed once)
module Servant.Reimagined.Server.Extract
  ( -- * Extraction protocols
    FromRequestParts (..)
  , FromRequest (..)
    -- * Built-in extractors
  , PathCapture (..)
  , JsonBody (..)
  , AppState (..)
  , RawBody (..)
    -- * Server error
  , ServerError (..)
  , mkError
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Typeable (Typeable)
import Network.HTTP.Types (Status, status400, status422, status500)

import qualified Data.Aeson as Aeson

import Http.Core (RequestParts (..), Extensions, lookupExtension)


-- | A structured server error with status and message.
data ServerError = ServerError
  { seStatus  :: !Status
  , seMessage :: !Text
  } deriving (Show)

mkError :: Status -> Text -> ServerError
mkError = ServerError


-- | Extract a value from the non-body parts of the request.
-- Can be called multiple times (parts are not consumed).
class FromRequestParts a where
  fromRequestParts :: RequestParts -> IO (Either ServerError a)


-- | Extract a value from the request body.
-- Called at most once (body is consumed).
class FromRequest a where
  fromRequest :: RequestParts -> ByteString -> IO (Either ServerError a)


-- ===================================================================
-- PathCapture: extract typed captures from the URL path
-- ===================================================================

-- | Captures extracted from URL path segments.
--
-- The type parameter corresponds to the 'CapturesTuple' of the
-- endpoint's path. For a path like @'[Lit "users", Capture Int]@,
-- the handler receives @PathCapture Int@.
--
-- Path matching and extraction happens in the Router, which stores
-- the extracted captures in Extensions. This extractor retrieves them.
newtype PathCapture a = PathCapture { unPathCapture :: a }
  deriving (Show, Eq)

instance Typeable a => FromRequestParts (PathCapture a) where
  fromRequestParts parts = do
    mVal <- lookupExtension @(PathCapture a) (rpExtensions parts)
    pure $ case mVal of
      Just pc -> Right pc
      Nothing -> Left (mkError status500 "PathCapture not found in extensions (router bug)")


-- ===================================================================
-- JsonBody: parse JSON request body
-- ===================================================================

-- | A JSON-decoded request body.
newtype JsonBody a = JsonBody { unJsonBody :: a }
  deriving (Show, Eq)

instance Aeson.FromJSON a => FromRequest (JsonBody a) where
  fromRequest _parts body =
    case Aeson.eitherDecodeStrict' body of
      Right val -> pure (Right (JsonBody val))
      Left err  -> pure (Left (mkError status422 (T.pack ("JSON parse error: " ++ err))))


-- ===================================================================
-- AppState: shared application state from Extensions
-- ===================================================================

-- | Shared application state injected via 'withState'.
--
-- @
-- handler :: AppState DbPool -> PathCapture Int -> IO (Json User)
-- @
newtype AppState a = AppState { unAppState :: a }
  deriving (Show, Eq)

instance Typeable a => FromRequestParts (AppState a) where
  fromRequestParts parts = do
    mVal <- lookupExtension @(AppState a) (rpExtensions parts)
    pure $ case mVal of
      Just st -> Right st
      Nothing -> Left (mkError status500 "AppState not found in extensions (forgot withState?)")


-- ===================================================================
-- RawBody: raw request body bytes
-- ===================================================================

-- | The raw request body as a ByteString.
newtype RawBody = RawBody { unRawBody :: ByteString }
  deriving (Show, Eq)

instance FromRequest RawBody where
  fromRequest _parts body = pure (Right (RawBody body))
