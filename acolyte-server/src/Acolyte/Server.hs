-- | @acolyte-server@ — HTTP server interpretation.
--
-- Interprets acolyte-core API types into a spire Service.
-- Combine with spire-wai to run on warp.
--
-- @
-- import Acolyte.Core
-- import Acolyte.Server
-- import Spire.Wai (runWarp)
--
-- type API = '[ Get '[ Lit "hello" ] Text ]
--
-- main :: IO ()
-- main = do
--   let svc = serve router
--   runWarp 3000 svc
-- @
module Acolyte.Server
  ( -- * Extractors
    FromRequestParts (..)
  , FromRequest (..)
  , PathCapture (..)
  , JsonBody (..)
  , ValidatedBody (..)
  , AppState (..)
  , RawBody (..)
  , ReqHeader (..)
  , Extension (..)
  , Optional (..)
  , ReqMethod (..)
  , QueryParam (..)
  , QueryParams (..)
  , OptionalParam (..)
  , HeaderMap (..)
  , BodyBytes (..)
  , StringBody (..)
  , Form (..)
  , FromForm (..)
  , RawForm (..)
  , Multipart (..)
  , FilePart (..)
  , parseFormUrlEncoded
  , parseMultipart
  , FullRequest (..)
  , RawQuery (..)
  , OriginalUri (..)
  , MatchedPath (..)
  , NestedPath (..)
  , ConnectInfo (..)
  , RawPathParams (..)
  , ParseCapture (..)
  , ServerError (..)
  , mkError

    -- * Responses
  , IntoResponse (..)
  , Json (..)
  , JsonError (..)
  , jsonError

    -- * Handler binding
  , BoundHandler (..)
  , HandlerFn
  , MatchResult (..)
  , HasEndpointInfo (..)
  , ReflectPath (..)
  , mkHandler0
  , mkHandler1Parts
  , mkHandler1Body
  , mkHandler2PartsBody

    -- * Ergonomic handler conversion
  , ToHandler (..)

    -- * Router (low-level)
  , Router
  , emptyRouter
  , addRoute
  , dispatch
  , serve
  , serveWithState
  , CaptureList (..)
  , injectCaptures

    -- * Automatic wiring (high-level)
  , mkServer
  , mkServerWith
  , WrappedHandler (..)
  , wrapHandler
  , handle
  , BuildServer (..)
  , BuildHandlers (..)

    -- * Ergonomic server construction (no wrapHandler needed)
  , mkApi
  , BuildApi (..)
  , toBoundHandler

    -- * Effectful server (typed middleware tracking)
  , EffectfulServer (..)
  , effectfulServer
  , effectfulApi
  , fromRouter
  , provide
  , run

    -- * Sub-API composition (for APIs > 25 endpoints)
  , combineServer2
  , combineServer3
  , combineServer4
  , combineServer5
  , combineServer6
  , combineServer7
  , combineServer8
  , subRouter

    -- * Combined effectful server (backward-compatible aliases)
  , CombinedServer
  , combinedFromRouter
  , provideEffect
  , runCombined

    -- * Named routes (record-based handler registration)
  , NamedApi (..)
  , mkNamedApi
  , effectfulNamedApi
    -- * Automatic record-based handler binding (requires Named wrappers)
  , BuildRecordApi (..)
  , mkRecordApi
  , effectfulRecordApi

    -- * Content negotiation
  , FormatEncoder (..)
  , NegotiatedResponse (..)
  , negotiate
  , parseAccept
  , matchFormat

    -- * Runtime validation
  , validationLayer
  , BuildRouteTable (..)

    -- * Streaming support
  , SSEvent (..)
  , sseEvent
  , sseData
  , ServerStreamHandler
  , ClientStreamHandler
  , BidiStreamHandler
  , sseResponse
  , sseResponseSync
  , sseChunk
  ) where

import Acolyte.Server.Extract
import Acolyte.Server.Response
import Acolyte.Server.Handler
import Acolyte.Server.Router
import Acolyte.Server.Wiring
import Acolyte.Server.Effects
import Acolyte.Server.Combine
import Acolyte.Server.CombineEffects
import Acolyte.Server.Validate
import Acolyte.Server.ToHandler
import Acolyte.Server.MkApi
import Acolyte.Server.Named
import Acolyte.Server.Negotiate
import Acolyte.Server.Streaming
