-- | @servant-reimagined-server@ — HTTP server interpretation.
--
-- Interprets servant-reimagined-core API types into a tower Service.
-- Combine with tower-wai to run on warp.
--
-- @
-- import Servant.Reimagined.Core
-- import Servant.Reimagined.Server
-- import Tower.Wai (runWarp)
--
-- type API = '[ Get '[ Lit "hello" ] Text ]
--
-- main :: IO ()
-- main = do
--   let svc = serve router
--   runWarp 3000 svc
-- @
module Servant.Reimagined.Server
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
  , sseChunk
  ) where

import Servant.Reimagined.Server.Extract
import Servant.Reimagined.Server.Response
import Servant.Reimagined.Server.Handler
import Servant.Reimagined.Server.Router
import Servant.Reimagined.Server.Wiring
import Servant.Reimagined.Server.Effects
import Servant.Reimagined.Server.Combine
import Servant.Reimagined.Server.CombineEffects
import Servant.Reimagined.Server.Validate
import Servant.Reimagined.Server.ToHandler
import Servant.Reimagined.Server.MkApi
import Servant.Reimagined.Server.Named
import Servant.Reimagined.Server.Negotiate
import Servant.Reimagined.Server.Streaming
