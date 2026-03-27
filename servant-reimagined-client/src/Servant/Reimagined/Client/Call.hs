-- | Type-safe endpoint calls derived from API types.
--
-- The key type family is 'CallArgs' which computes the argument type
-- from the endpoint type, and 'CallResult' which is the response type.
module Servant.Reimagined.Client.Call
  ( -- * Calling endpoints
    callEndpoint
    -- * Request building
  , EndpointRequest (..)
  , BuildPath (..)
  , ShowCapture (..)
  , buildUrl
  ) where

import Control.Exception (try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (statusIsSuccessful, statusCode)

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Client as HC

import Servant.Reimagined.Core.Method (KnownMethod, methodVal)
import qualified Servant.Reimagined.Core.Method as Core
import Servant.Reimagined.Core.Path (PathSegment (..), Captures, CapturesTuple)
import Servant.Reimagined.Core.Endpoint (Endpoint, NoBody)
import Servant.Reimagined.Core.Effect (Requires)
import Servant.Reimagined.Core.Wrapper
  ( Named, Versioned, ApiVersion (..), Describe, Description
  , WithParams, WithHeaders, RespondsWith
  )
import Servant.Reimagined.Client.Core


-- | Build a request for an endpoint type.
class EndpointRequest endpoint where
  -- | The type of arguments needed (captures + optional body).
  type ReqArgs endpoint

  -- | The expected deserialized response type.
  type ReqResult endpoint

  -- | Build the HTTP method, URL segments, and optional body.
  buildReq :: Proxy endpoint -> ReqArgs endpoint -> (ByteString, [Text], Maybe LBS.ByteString)


-- GET (no body)
instance (BuildPath path, Aeson.FromJSON resp)
  => EndpointRequest (Endpoint 'Core.GET path NoBody resp) where
    type ReqArgs (Endpoint 'Core.GET path NoBody resp) = PathArgs path
    type ReqResult (Endpoint 'Core.GET path NoBody resp) = resp
    buildReq _ args = ("GET", buildPath @path args, Nothing)

-- DELETE (no body)
instance (BuildPath path, Aeson.FromJSON resp)
  => EndpointRequest (Endpoint 'Core.DELETE path NoBody resp) where
    type ReqArgs (Endpoint 'Core.DELETE path NoBody resp) = PathArgs path
    type ReqResult (Endpoint 'Core.DELETE path NoBody resp) = resp
    buildReq _ args = ("DELETE", buildPath @path args, Nothing)

-- HEAD (no body)
instance (BuildPath path, Aeson.FromJSON resp)
  => EndpointRequest (Endpoint 'Core.HEAD path NoBody resp) where
    type ReqArgs (Endpoint 'Core.HEAD path NoBody resp) = PathArgs path
    type ReqResult (Endpoint 'Core.HEAD path NoBody resp) = resp
    buildReq _ args = ("HEAD", buildPath @path args, Nothing)

-- POST with JSON body
instance (BuildPath path, Aeson.ToJSON req, Aeson.FromJSON resp)
  => EndpointRequest (Endpoint 'Core.POST path req resp) where
    type ReqArgs (Endpoint 'Core.POST path req resp) = (PathArgs path, req)
    type ReqResult (Endpoint 'Core.POST path req resp) = resp
    buildReq _ (args, body) = ("POST", buildPath @path args, Just (Aeson.encode body))

-- PUT with JSON body
instance (BuildPath path, Aeson.ToJSON req, Aeson.FromJSON resp)
  => EndpointRequest (Endpoint 'Core.PUT path req resp) where
    type ReqArgs (Endpoint 'Core.PUT path req resp) = (PathArgs path, req)
    type ReqResult (Endpoint 'Core.PUT path req resp) = resp
    buildReq _ (args, body) = ("PUT", buildPath @path args, Just (Aeson.encode body))

-- PATCH with JSON body
instance (BuildPath path, Aeson.ToJSON req, Aeson.FromJSON resp)
  => EndpointRequest (Endpoint 'Core.PATCH path req resp) where
    type ReqArgs (Endpoint 'Core.PATCH path req resp) = (PathArgs path, req)
    type ReqResult (Endpoint 'Core.PATCH path req resp) = resp
    buildReq _ (args, body) = ("PATCH", buildPath @path args, Just (Aeson.encode body))

-- Requires delegates to inner
instance EndpointRequest inner => EndpointRequest (Requires e inner) where
  type ReqArgs (Requires e inner) = ReqArgs inner
  type ReqResult (Requires e inner) = ReqResult inner
  buildReq _ = buildReq (Proxy @inner)

-- Named delegates to inner (name is irrelevant for client calls)
instance EndpointRequest inner => EndpointRequest (Named name inner) where
  type ReqArgs (Named name inner) = ReqArgs inner
  type ReqResult (Named name inner) = ReqResult inner
  buildReq _ = buildReq (Proxy @inner)

-- Versioned prepends the version prefix to URL segments
instance (ApiVersion v, EndpointRequest inner)
  => EndpointRequest (Versioned v inner) where
    type ReqArgs (Versioned v inner) = ReqArgs inner
    type ReqResult (Versioned v inner) = ReqResult inner
    buildReq _ args =
      let (method, segments, mBody) = buildReq (Proxy @inner) args
      in (method, versionPrefix @v : segments, mBody)

-- Describe delegates to inner (description is irrelevant for client calls)
instance EndpointRequest inner => EndpointRequest (Describe desc inner) where
  type ReqArgs (Describe desc inner) = ReqArgs inner
  type ReqResult (Describe desc inner) = ReqResult inner
  buildReq _ = buildReq (Proxy @inner)

-- Description delegates to inner (description is irrelevant for client calls)
instance EndpointRequest inner => EndpointRequest (Description desc inner) where
  type ReqArgs (Description desc inner) = ReqArgs inner
  type ReqResult (Description desc inner) = ReqResult inner
  buildReq _ = buildReq (Proxy @inner)

-- WithParams delegates to inner (query params are not path-level)
instance EndpointRequest inner => EndpointRequest (WithParams ps inner) where
  type ReqArgs (WithParams ps inner) = ReqArgs inner
  type ReqResult (WithParams ps inner) = ReqResult inner
  buildReq _ = buildReq (Proxy @inner)

-- WithHeaders delegates to inner (headers are separate from path/body)
instance EndpointRequest inner => EndpointRequest (WithHeaders hs inner) where
  type ReqArgs (WithHeaders hs inner) = ReqArgs inner
  type ReqResult (WithHeaders hs inner) = ReqResult inner
  buildReq _ = buildReq (Proxy @inner)

-- RespondsWith delegates to inner (status code is irrelevant for client calls)
instance EndpointRequest inner => EndpointRequest (RespondsWith s inner) where
  type ReqArgs (RespondsWith s inner) = ReqArgs inner
  type ReqResult (RespondsWith s inner) = ReqResult inner
  buildReq _ = buildReq (Proxy @inner)


-- ===================================================================
-- Path building
-- ===================================================================

-- | The path arguments type — matches CapturesTuple from the core.
type PathArgs :: [PathSegment] -> Type
type family PathArgs (path :: [PathSegment]) :: Type where
  PathArgs path = CapturesTuple (Captures path)


-- | Build URL segments from a type-level path and capture values.
class BuildPath (path :: [PathSegment]) where
  buildPath :: PathArgs path -> [Text]

instance BuildPath '[] where
  buildPath () = []

instance (KnownSymbol s, BuildPath rest, PathArgs ('Lit s ': rest) ~ PathArgs rest)
  => BuildPath ('Lit s ': rest) where
    buildPath args = T.pack (symbolVal (Proxy @s)) : buildPath @rest args

-- Single capture (no more captures after)
instance {-# OVERLAPPING #-} (ShowCapture t, KnownSymbol s, PathArgs ('Capture t ': '[ 'Lit s ]) ~ t)
  => BuildPath ('Capture t ': '[ 'Lit s ]) where
    buildPath cap = [showCapture cap, T.pack (symbolVal (Proxy @s))]

-- Single capture at end of path
instance {-# OVERLAPPING #-} (ShowCapture t)
  => BuildPath '[ 'Capture t ] where
    buildPath cap = [showCapture cap]

-- CaptureNamed followed by a literal (identical to Capture — name is cosmetic)
instance {-# OVERLAPPING #-} (ShowCapture t, KnownSymbol s, PathArgs ('CaptureNamed name t ': '[ 'Lit s ]) ~ t)
  => BuildPath ('CaptureNamed name t ': '[ 'Lit s ]) where
    buildPath cap = [showCapture cap, T.pack (symbolVal (Proxy @s))]

-- CaptureNamed at end of path
instance {-# OVERLAPPING #-} (ShowCapture t)
  => BuildPath '[ 'CaptureNamed name t ] where
    buildPath cap = [showCapture cap]


-- | Convert a capture value to a text URL segment.
class ShowCapture a where
  showCapture :: a -> Text

instance ShowCapture Int where
  showCapture = T.pack . show

instance ShowCapture Integer where
  showCapture = T.pack . show

instance ShowCapture Text where
  showCapture = id

instance ShowCapture String where
  showCapture = T.pack


-- ===================================================================
-- High-level call function
-- ===================================================================

-- | Call an endpoint on a client.
--
-- @
-- result <- callEndpoint @(Get UsersPath (Json [User])) client ()
-- result <- callEndpoint @(Get UserByIdPath (Json User)) client 42
-- result <- callEndpoint @(Post UsersPath (Json CreateUser) (Json User)) client ((), newUser)
-- @
callEndpoint
  :: forall endpoint
   . (EndpointRequest endpoint, Aeson.FromJSON (ReqResult endpoint))
  => Client
  -> ReqArgs endpoint
  -> IO (Either ClientError (ReqResult endpoint))
callEndpoint client args = do
  let (method, segments, mBody) = buildReq (Proxy @endpoint) args
      url = buildUrl (clientBaseUrl client) segments
      urlStr = T.unpack url

  initReq <- HC.parseRequest urlStr
  let req0 = initReq
        { HC.method = method
        , HC.requestHeaders =
            [("Accept", "application/json"), ("Content-Type", "application/json")]
        }
      req1 = case mBody of
        Nothing -> req0
        Just body -> req0 { HC.requestBody = HC.RequestBodyLBS body }

  req2 <- interceptRequest (clientInterceptor client) req1

  result <- try @HC.HttpException $ HC.httpLbs req2 (clientManager client)
  case result of
    Left ex -> pure (Left (HttpError ex))
    Right resp -> do
      let status = HC.responseStatus resp
          body = LBS.toStrict (HC.responseBody resp)
      if statusIsSuccessful status
        then case Aeson.eitherDecodeStrict' body of
          Right val -> pure (Right val)
          Left err  -> pure (Left (DecodeError (T.pack err)))
        else pure (Left (StatusError (statusCode status) body))


-- ===================================================================
-- Helpers
-- ===================================================================

-- | Join a base URL with path segments to form a complete URL.
buildUrl :: Text -> [Text] -> Text
buildUrl base segments = base <> "/" <> T.intercalate "/" segments

methodToBS :: Core.Method -> ByteString
methodToBS Core.GET     = "GET"
methodToBS Core.POST    = "POST"
methodToBS Core.PUT     = "PUT"
methodToBS Core.DELETE  = "DELETE"
methodToBS Core.PATCH   = "PATCH"
methodToBS Core.HEAD    = "HEAD"
methodToBS Core.OPTIONS = "OPTIONS"
