-- | EffectfulServer: compile-time middleware effect tracking.
--
-- The builder pattern tracks which middleware effects have been
-- provided as a phantom type parameter. The 'run' method only
-- compiles when all effects declared in the API (via 'Requires')
-- have been discharged via 'provide'.
--
-- @
-- type API = '[ Requires Auth (Get UserPath (Json User))
--             , Requires Cors (Get PublicPath (Json Data))
--             , Get HealthPath String
--             ]
--
-- main = runWarp 3000
--   $ run
--   $ provide @Auth authMw
--   $ provide @Cors corsMw
--   $ effectfulServer @API handlers
-- @
module Acolyte.Server.Effects
  ( -- * Builder
    EffectfulServer (..)
  , effectfulServer
  , effectfulApi
  , fromRouter
    -- * Adding effects
  , provide
    -- * Finalizing
  , run
  ) where

import Data.Kind (Type)
import Data.ByteString (ByteString)

import Tower (Middleware, Service)
import Tower.Service (Service (..))
import Tower.Layer (applyLayer)
import Http.Core (Request, Response)

import Acolyte.Core.API (Serves)
import Acolyte.Core.Effect (AllEffectsProvided)
import Acolyte.Server.Wiring (BuildServer, mkServer)
import Acolyte.Server.MkApi (BuildApi, mkApi)
import Acolyte.Server.Router (Router, serve)


-- | A server builder that tracks which effects have been provided.
--
-- @api@ is the API type (type-level list of endpoints).
-- @provided@ is the type-level list of effects discharged so far.
-- Starts as @'[]@ and grows with each 'provide' call.
--
-- Used for both single-API servers (via 'effectfulServer') and
-- combined multi-sub-API servers (via 'fromRouter').
data EffectfulServer (api :: [Type]) (provided :: [Type]) = EffectfulServer
  { esService :: !(Service IO (Request ByteString) (Response ByteString))
  }


-- | Create an effectful server from an API type and handler tuple.
effectfulServer
  :: forall api handlers
   . (Serves api handlers, BuildServer api handlers)
  => handlers
  -> EffectfulServer api '[]
effectfulServer handlers = EffectfulServer (mkServer @api handlers)


-- | Create an effectful server from an API type and plain handler functions.
--
-- Like 'effectfulServer' but uses 'mkApi' — no 'wrapHandler' or
-- 'toHandler' ceremony needed.
--
-- @
-- effectfulApi \@API (healthHandler, getUserHandler)
-- @
effectfulApi
  :: forall api handlers
   . (Serves api handlers, BuildApi api handlers)
  => handlers
  -> EffectfulServer api '[]
effectfulApi handlers = EffectfulServer (mkApi @api handlers)


-- | Create an effectful server from a pre-built router.
--
-- Used with sub-API composition:
--
-- @
-- fromRouter @FullAPI
--   $ subRouter @API2 h2
--   $ subRouter @API1 h1
--   $ emptyRouter
-- @
fromRouter
  :: forall api
   . Router
  -> EffectfulServer api '[]
fromRouter router = EffectfulServer (serve router)


-- | Provide a middleware that satisfies an effect requirement, applying it to the server.
--
-- Usage: @provide \@Auth authMiddleware@
provide
  :: forall e api provided
   . Middleware IO (Request ByteString) (Response ByteString)
  -> EffectfulServer api provided
  -> EffectfulServer api (e ': provided)
provide mw (EffectfulServer svc) =
  EffectfulServer (applyLayer mw svc)


-- | Finalize the server into a tower Service.
--
-- Only compiles if every 'Requires' in the API has been provided.
run
  :: forall api provided
   . AllEffectsProvided api provided
  => EffectfulServer api provided
  -> Service IO (Request ByteString) (Response ByteString)
run (EffectfulServer svc) = svc
