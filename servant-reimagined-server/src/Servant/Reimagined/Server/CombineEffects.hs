-- | Effect-tracked combined server.
--
-- Combines multiple sub-APIs and verifies that all required effects
-- across ALL sub-APIs are provided before the server can start.
--
-- @
-- type FullAPI = UsersAPI ++ ArticlesAPI
--
-- server = runCombined @FullAPI
--   $ provide @Auth authMw
--   $ combinedEffectful
--       (subRouter @UsersAPI usersHandlers)
--       (subRouter @ArticlesAPI articleHandlers)
-- @
module Servant.Reimagined.Server.CombineEffects
  ( -- * Combined effectful server
    CombinedServer (..)
  , combinedFromRouter
    -- * Effect tracking
  , provideEffect
  , runCombined
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

import Tower (Middleware)
import Tower.Service (Service)
import Tower.Layer (applyLayer)
import Http.Core (Request, Response)

import Servant.Reimagined.Core.Effect (AllEffectsProvided)
import Servant.Reimagined.Server.Router (Router, serve)


-- | A combined server with phantom-tracked effects.
--
-- @fullApi@ is the combined API type (e.g., @UsersAPI ++ ArticlesAPI@).
-- @provided@ is the type-level list of effects discharged so far.
data CombinedServer (fullApi :: [Type]) (provided :: [Type]) = CombinedServer
  { csService :: !(Service IO (Request ByteString) (Response ByteString))
  }


-- | Create a combined server from a pre-built router.
--
-- @
-- let router = subRouter @API2 h2 $ subRouter @API1 h1 $ emptyRouter
-- combinedFromRouter @FullAPI router
-- @
combinedFromRouter
  :: forall fullApi
   . Router
  -> CombinedServer fullApi '[]
combinedFromRouter router = CombinedServer (serve router)


-- | Declare that a middleware effect has been provided.
provideEffect
  :: forall e fullApi provided
   . Middleware IO (Request ByteString) (Response ByteString)
  -> CombinedServer fullApi provided
  -> CombinedServer fullApi (e ': provided)
provideEffect mw (CombinedServer svc) =
  CombinedServer (applyLayer mw svc)


-- | Finalize the combined server.
--
-- Only compiles if every 'Requires' across ALL sub-APIs in @fullApi@
-- has been provided.
runCombined
  :: forall fullApi provided
   . AllEffectsProvided fullApi provided
  => CombinedServer fullApi provided
  -> Service IO (Request ByteString) (Response ByteString)
runCombined (CombinedServer svc) = svc
