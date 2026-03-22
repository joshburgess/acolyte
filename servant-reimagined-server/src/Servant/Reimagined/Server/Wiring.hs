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


-- Arity 9
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9) router =
    addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 10
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10) router =
    addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 11
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11) router =
    addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 12
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12) router =
    addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 13
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13) router =
    addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 14
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14) router =
    addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 15
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15) router =
    addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 16
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16) router =
    addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router


-- Arity 17
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17) router =
    addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 18
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18) router =
    addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 19
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18, WrappedHandler e19) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19) router =
    addRoute (mkBound @e19 h19) . addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 20
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18, WrappedHandler e19, WrappedHandler e20) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20) router =
    addRoute (mkBound @e20 h20) . addRoute (mkBound @e19 h19) . addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 21
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18, WrappedHandler e19, WrappedHandler e20, WrappedHandler e21) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21) router =
    addRoute (mkBound @e21 h21) . addRoute (mkBound @e20 h20) . addRoute (mkBound @e19 h19) . addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 22
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18, WrappedHandler e19, WrappedHandler e20, WrappedHandler e21, WrappedHandler e22) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22) router =
    addRoute (mkBound @e22 h22) . addRoute (mkBound @e21 h21) . addRoute (mkBound @e20 h20) . addRoute (mkBound @e19 h19) . addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 23
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22, HasEndpointInfo e23)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18, WrappedHandler e19, WrappedHandler e20, WrappedHandler e21, WrappedHandler e22, WrappedHandler e23) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23) router =
    addRoute (mkBound @e23 h23) . addRoute (mkBound @e22 h22) . addRoute (mkBound @e21 h21) . addRoute (mkBound @e20 h20) . addRoute (mkBound @e19 h19) . addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 24
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22, HasEndpointInfo e23, HasEndpointInfo e24)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18, WrappedHandler e19, WrappedHandler e20, WrappedHandler e21, WrappedHandler e22, WrappedHandler e23, WrappedHandler e24) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24) router =
    addRoute (mkBound @e24 h24) . addRoute (mkBound @e23 h23) . addRoute (mkBound @e22 h22) . addRoute (mkBound @e21 h21) . addRoute (mkBound @e20 h20) . addRoute (mkBound @e19 h19) . addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router

-- Arity 25
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22, HasEndpointInfo e23, HasEndpointInfo e24, HasEndpointInfo e25)
  => BuildServer '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25]
       (WrappedHandler e1, WrappedHandler e2, WrappedHandler e3, WrappedHandler e4, WrappedHandler e5, WrappedHandler e6, WrappedHandler e7, WrappedHandler e8, WrappedHandler e9, WrappedHandler e10, WrappedHandler e11, WrappedHandler e12, WrappedHandler e13, WrappedHandler e14, WrappedHandler e15, WrappedHandler e16, WrappedHandler e17, WrappedHandler e18, WrappedHandler e19, WrappedHandler e20, WrappedHandler e21, WrappedHandler e22, WrappedHandler e23, WrappedHandler e24, WrappedHandler e25) where
  buildRouter (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24, h25) router =
    addRoute (mkBound @e25 h25) . addRoute (mkBound @e24 h24) . addRoute (mkBound @e23 h23) . addRoute (mkBound @e22 h22) . addRoute (mkBound @e21 h21) . addRoute (mkBound @e20 h20) . addRoute (mkBound @e19 h19) . addRoute (mkBound @e18 h18) . addRoute (mkBound @e17 h17) . addRoute (mkBound @e16 h16) . addRoute (mkBound @e15 h15) . addRoute (mkBound @e14 h14) . addRoute (mkBound @e13 h13) . addRoute (mkBound @e12 h12) . addRoute (mkBound @e11 h11) . addRoute (mkBound @e10 h10) . addRoute (mkBound @e9 h9) . addRoute (mkBound @e8 h8) . addRoute (mkBound @e7 h7) . addRoute (mkBound @e6 h6) . addRoute (mkBound @e5 h5) . addRoute (mkBound @e4 h4) . addRoute (mkBound @e3 h3) . addRoute (mkBound @e2 h2) . addRoute (mkBound @e1 h1) $ router


-- | Build a BoundHandler from a WrappedHandler using type-level endpoint info.
mkBound :: forall endpoint. HasEndpointInfo endpoint => WrappedHandler endpoint -> BoundHandler
mkBound (WrappedHandler fn) = BoundHandler
  { bhMethod  = endpointMethod @endpoint
  , bhPattern = endpointPattern @endpoint
  , bhMatchFn = endpointMatcher @endpoint
  , bhHandler = fn
  }
