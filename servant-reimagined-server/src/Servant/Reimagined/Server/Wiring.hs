-- | Automatic handler wiring: connect handler tuples to API types.
--
-- 'BuildServer' walks a type-level API list and a handler tuple in
-- lockstep, building a 'Router' with one 'BoundHandler' per endpoint.
--
-- @
-- type API = '[ Get HealthPath Text, Get UserByIdPath (Json User) ]
--
-- svc = mkServer @API
--   ( mkHandler0 (pure ("ok" :: Text))
--   , mkHandler1Parts (\(PathCapture uid) -> pure (Json (lookupUser uid)))
--   )
-- @
--
-- The 'Serves' constraint from the core ensures the tuple length
-- matches. 'BuildServer' additionally requires 'HasEndpointInfo'
-- for each endpoint to extract routing metadata.
module Servant.Reimagined.Server.Wiring
  ( -- * Server construction
    mkServer
    -- * Handler wrapping (re-exports for convenience)
  , WrappedHandler (..)
  , wrapHandler
    -- * BuildServer class (internal)
  , BuildServer (..)
  ) where

import Data.Kind (Type)
import Data.ByteString (ByteString)
import Data.Text (Text)

import Tower.Service (Service (..))
import Http.Core (Request, Response)

import Servant.Reimagined.Core.API (Serves)
import Servant.Reimagined.Server.Handler
import Servant.Reimagined.Server.Router (Router, emptyRouter, addRoute, serve)


-- | A type-erased handler paired with a phantom endpoint type.
-- Users create these with 'wrapHandler'.
data WrappedHandler endpoint = WrappedHandler
  { whHandler :: !HandlerFn
  }


-- | Wrap a HandlerFn for a specific endpoint type.
--
-- @
-- wrapHandler @(Get HealthPath Text) (mkHandler0 (pure "ok"))
-- @
wrapHandler :: forall endpoint. HandlerFn -> WrappedHandler endpoint
wrapHandler = WrappedHandler


-- | Build a server from an API type and handler tuple.
--
-- @
-- mkServer @API (handler1, handler2, handler3)
-- @
--
-- The 'Serves' constraint checks tuple length. 'BuildServer' does
-- the runtime wiring.
mkServer
  :: forall api handlers
   . (Serves api handlers, BuildServer api handlers)
  => handlers
  -> Service IO (Request ByteString) (Response ByteString)
mkServer handlers = serve (buildRouter @api handlers emptyRouter)


-- | Class that walks the API list and handler tuple, populating a Router.
class BuildServer (api :: [Type]) handlers where
  buildRouter :: handlers -> Router -> Router


-- Arity 1
instance HasEndpointInfo e1
  => BuildServer '[e1] (WrappedHandler e1) where
  buildRouter h router =
    addRoute (mkBound @e1 h) router

-- Arity 2
instance (HasEndpointInfo e1, HasEndpointInfo e2)
  => BuildServer '[e1, e2] (WrappedHandler e1, WrappedHandler e2) where
  buildRouter (h1, h2) router =
    addRoute (mkBound @e2 h2)
    . addRoute (mkBound @e1 h1)
    $ router

-- Arity 3
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3)
  => BuildServer '[e1, e2, e3] (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3) where
  buildRouter (h1, h2, h3) router =
    addRoute (mkBound @e3 h3)
    . addRoute (mkBound @e2 h2)
    . addRoute (mkBound @e1 h1)
    $ router

-- Arity 4
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4)
  => BuildServer '[e1, e2, e3, e4]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4) where
  buildRouter (h1, h2, h3, h4) router =
    addRoute (mkBound @e4 h4)
    . addRoute (mkBound @e3 h3)
    . addRoute (mkBound @e2 h2)
    . addRoute (mkBound @e1 h1)
    $ router

-- Arity 5
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5)
  => BuildServer '[e1, e2, e3, e4, e5]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5) where
  buildRouter (h1, h2, h3, h4, h5) router =
    addRoute (mkBound @e5 h5)
    . addRoute (mkBound @e4 h4)
    . addRoute (mkBound @e3 h3)
    . addRoute (mkBound @e2 h2)
    . addRoute (mkBound @e1 h1)
    $ router

-- Arities 6-16 follow the same pattern. Adding up to 8 for now.

-- Arity 6
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6)
  => BuildServer '[e1, e2, e3, e4, e5, e6]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6) where
  buildRouter (h1, h2, h3, h4, h5, h6) router =
    addRoute (mkBound @e6 h6)
    . addRoute (mkBound @e5 h5)
    . addRoute (mkBound @e4 h4)
    . addRoute (mkBound @e3 h3)
    . addRoute (mkBound @e2 h2)
    . addRoute (mkBound @e1 h1)
    $ router

-- Arity 7
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7) router =
    addRoute (mkBound @e7 h7)
    . addRoute (mkBound @e6 h6)
    . addRoute (mkBound @e5 h5)
    . addRoute (mkBound @e4 h4)
    . addRoute (mkBound @e3 h3)
    . addRoute (mkBound @e2 h2)
    . addRoute (mkBound @e1 h1)
    $ router

-- Arity 8
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8) router =
    addRoute (mkBound @e8 h8)
    . addRoute (mkBound @e7 h7)
    . addRoute (mkBound @e6 h6)
    . addRoute (mkBound @e5 h5)
    . addRoute (mkBound @e4 h4)
    . addRoute (mkBound @e3 h3)
    . addRoute (mkBound @e2 h2)
    . addRoute (mkBound @e1 h1)
    $ router


-- | Build a BoundHandler from a WrappedHandler using type-level endpoint info.
mkBound :: forall endpoint. HasEndpointInfo endpoint => WrappedHandler endpoint -> BoundHandler
mkBound (WrappedHandler fn) = BoundHandler
  { bhMethod  = endpointMethod @endpoint
  , bhPattern = endpointPattern @endpoint
  , bhMatchFn = endpointMatcher @endpoint
  , bhHandler = fn
  }
