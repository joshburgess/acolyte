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
  , AppState (..)
  , RawBody (..)
  , ReqHeader (..)
  , Extension (..)
  , Optional (..)
  , ReqMethod (..)
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

    -- * Router (low-level)
  , Router
  , emptyRouter
  , addRoute
  , dispatch
  , serve
  , serveWithState
  , ParseCapture (..)
  , CaptureList (..)
  , injectCaptures

    -- * Automatic wiring (high-level)
  , mkServer
  , WrappedHandler (..)
  , wrapHandler
  , BuildServer (..)

    -- * Effectful server (typed middleware tracking)
  , EffectfulServer (..)
  , effectfulServer
  , provide
  , run
  ) where

import Servant.Reimagined.Server.Extract
import Servant.Reimagined.Server.Response
import Servant.Reimagined.Server.Handler
import Servant.Reimagined.Server.Router
import Servant.Reimagined.Server.Wiring
import Servant.Reimagined.Server.Effects
