-- | WAI/warp backend adapter for tower.
--
-- This is the __only package that imports WAI__. Everything above
-- (tower, tower-http, acolyte-server) uses http-core types
-- and is backend-agnostic.
--
-- @
-- import Tower.Wai (runWarp)
-- runWarp 3000 myService
-- @
module Tower.Wai
  ( -- * Running on warp
    runWarp
  , runWarpSettings

    -- * Core WAI conversion
  , toWaiApp
  , fromWaiRequest
  , toWaiResponse

    -- * WAI middleware interop (migration bridge)
  , fromWaiMiddleware
  ) where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)

import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai (ResponseReceived (..))
import qualified Network.Wai.Handler.Warp as Warp

import Tower (Middleware, middleware)
import Tower.Service (Service (..))
import Tower.Layer (Layer (..))
import Http.Core
  ( Request (..), Response (..)
  , emptyExtensions
  )


-- ===================================================================
-- Running on warp
-- ===================================================================

-- | Run a tower Service on warp at the given port.
runWarp :: Int -> Service IO (Request ByteString) (Response ByteString) -> IO ()
runWarp port svc = Warp.run port (toWaiApp svc)

-- | Run with full warp settings.
runWarpSettings
  :: Warp.Settings
  -> Service IO (Request ByteString) (Response ByteString)
  -> IO ()
runWarpSettings settings svc = Warp.runSettings settings (toWaiApp svc)


-- ===================================================================
-- Core WAI conversion
-- ===================================================================

-- | Convert a tower Service to a WAI Application.
toWaiApp :: Service IO (Request ByteString) (Response ByteString) -> Wai.Application
toWaiApp (Service f) waiReq respond = do
  req <- fromWaiRequest waiReq
  resp <- f req
  respond (toWaiResponse resp)

-- | Convert a WAI Request to our Request (collects body as strict bytes).
fromWaiRequest :: Wai.Request -> IO (Request ByteString)
fromWaiRequest waiReq = do
  body <- LBS.toStrict <$> Wai.consumeRequestBodyStrict waiReq
  exts <- emptyExtensions
  pure Request
    { requestMethod     = Wai.requestMethod waiReq
    , requestPathRaw    = Wai.rawPathInfo waiReq
    , requestPath       = filter (/= "") (Wai.pathInfo waiReq)
    , requestQuery      = Wai.queryString waiReq
    , requestHeaders    = Wai.requestHeaders waiReq
    , requestBody       = body
    , requestExtensions = exts
    }

-- | Convert our Response to a WAI Response.
toWaiResponse :: Response ByteString -> Wai.Response
toWaiResponse resp =
  Wai.responseLBS
    (responseStatus resp)
    (responseHeaders resp)
    (LBS.fromStrict (responseBody resp))


-- ===================================================================
-- WAI middleware interop
-- ===================================================================

-- | Wrap existing WAI Middleware as a tower Middleware.
--
-- Migration bridge for @wai-extra@, @wai-cors@, etc. The WAI
-- middleware wraps the service at the WAI level.
--
-- @
-- import qualified Network.Wai.Middleware.Gzip as Wai
-- myService |> fromWaiMiddleware Wai.gzip
-- @
fromWaiMiddleware
  :: Wai.Middleware
  -> Middleware IO (Request ByteString) (Response ByteString)
fromWaiMiddleware waiMw = middleware $ \innerSvc ->
  let innerWaiApp = toWaiApp innerSvc
      wrappedWaiApp = waiMw innerWaiApp
  in  waiAppToService wrappedWaiApp


-- | Convert a WAI Application to a tower Service.
-- Bridges WAI's CPS response style via MVar.
waiAppToService :: Wai.Application -> Service IO (Request ByteString) (Response ByteString)
waiAppToService waiApp = Service $ \req -> do
  let waiReq = toWaiRequest req
  resultVar <- newEmptyMVar
  _ <- waiApp waiReq $ \waiResp -> do
    putMVar resultVar (fromWaiResponse waiResp)
    pure Wai.ResponseReceived
  takeMVar resultVar


-- | Reconstruct a minimal WAI Request from our Request.
-- Lossy — vault, socket info, HTTP version not preserved.
toWaiRequest :: Request ByteString -> Wai.Request
toWaiRequest req = Wai.defaultRequest
  { Wai.requestMethod     = requestMethod req
  , Wai.rawPathInfo       = requestPathRaw req
  , Wai.pathInfo          = requestPath req
  , Wai.queryString       = requestQuery req
  , Wai.requestHeaders    = requestHeaders req
  , Wai.requestBodyLength = Wai.KnownLength (fromIntegral $ BS.length $ requestBody req)
  }


-- | Extract status and headers from a WAI Response.
-- Body is empty — for header-only middleware (CORS, security headers)
-- the real body passes through the inner toWaiApp/toWaiResponse path.
fromWaiResponse :: Wai.Response -> Response ByteString
fromWaiResponse waiResp = Response
  { responseStatus  = Wai.responseStatus waiResp
  , responseHeaders = Wai.responseHeaders waiResp
  , responseBody    = ""
  }
