{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Ergonomic server construction: pass handler functions directly.
--
-- @mkApi@ lets you build a server from an API type and a tuple of
-- plain handler functions — no 'wrapHandler', no 'toHandler', no
-- ceremony:
--
-- @
-- type API = '[ Get HealthPath Text
--             , Get UserByIdPath (Json Text)
--             ]
--
-- healthHandler :: IO Text
-- healthHandler = pure "ok"
--
-- getUserHandler :: PathCapture Int -> IO (Json Text)
-- getUserHandler (PathCapture n) = pure (Json (T.pack ("user-" ++ show n)))
--
-- svc = mkApi \@API (healthHandler, getUserHandler)
-- @
--
-- Each handler must satisfy 'ToHandler' (the same class used with the
-- explicit API). Endpoint metadata is extracted via 'HasEndpointInfo'.
-- The tuple is matched positionally to the type-level API list.
module Servant.Reimagined.Server.MkApi
  ( -- * Server construction
    mkApi
    -- * BuildApi class (internal)
  , BuildApi (..)
    -- * Helper
  , toBoundHandler
  ) where

import Data.Kind (Type)
import Data.ByteString (ByteString)

import Tower.Service (Service (..))
import Http.Core (Request, Response)

import Servant.Reimagined.Core.API (Serves)
import Servant.Reimagined.Server.Handler
  ( BoundHandler (..), HasEndpointInfo (..) )
import Servant.Reimagined.Server.ToHandler (ToHandler (..))
import Servant.Reimagined.Server.Router
  ( Router, emptyRouter, addRoute )
import qualified Servant.Reimagined.Server.Router as Router


-- | Build a 'BoundHandler' from a handler function and endpoint metadata.
--
-- Combines 'toHandler' (typed function -> HandlerFn) with
-- 'HasEndpointInfo' (endpoint type -> routing metadata).
toBoundHandler
  :: forall endpoint f. (HasEndpointInfo endpoint, ToHandler f)
  => f -> BoundHandler
toBoundHandler f = BoundHandler
  { bhMethod  = endpointMethod @endpoint
  , bhPattern = endpointPattern @endpoint
  , bhMatchFn = endpointMatcher @endpoint
  , bhHandler = toHandler f
  }


-- | Build a tower Service from an API type and a tuple of handler functions.
--
-- This is the most ergonomic entry point. Each handler function is
-- positionally matched to the corresponding endpoint in the API
-- type-level list. Handlers must satisfy 'ToHandler' — they can be
-- plain @IO r@ actions or functions taking extractors
-- (@PathCapture@, @JsonBody@, etc.).
--
-- @
-- mkApi \@API (healthHandler, getUserHandler, createUserHandler)
-- @
mkApi
  :: forall api handlers
   . (Serves api handlers, BuildApi api handlers)
  => handlers
  -> Service IO (Request ByteString) (Response ByteString)
mkApi handlers = Router.serve (buildApi @api handlers emptyRouter)


-- | Class that walks the API list and handler tuple, populating a Router.
--
-- Each endpoint gets its routing metadata from 'HasEndpointInfo',
-- and each handler is converted via 'ToHandler'.
class BuildApi (api :: [Type]) handlers where
  buildApi :: handlers -> Router -> Router


-- Arity 1
instance (HasEndpointInfo e1, ToHandler h1)
  => BuildApi '[e1] h1 where
  buildApi h router =
    addRoute (toBoundHandler @e1 h) router

-- Arity 2
instance (HasEndpointInfo e1, HasEndpointInfo e2, ToHandler h1, ToHandler h2)
  => BuildApi '[e1, e2] (h1, h2) where
  buildApi (h1, h2) router =
    addRoute (toBoundHandler @e2 h2)
    . addRoute (toBoundHandler @e1 h1)
    $ router

-- Arity 3
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, ToHandler h1, ToHandler h2, ToHandler h3)
  => BuildApi '[e1, e2, e3] (h1, h2, h3) where
  buildApi (h1, h2, h3) router =
    addRoute (toBoundHandler @e3 h3)
    . addRoute (toBoundHandler @e2 h2)
    . addRoute (toBoundHandler @e1 h1)
    $ router

-- Arity 4
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4)
  => BuildApi '[e1, e2, e3, e4] (h1, h2, h3, h4) where
  buildApi (h1, h2, h3, h4) router =
    addRoute (toBoundHandler @e4 h4)
    . addRoute (toBoundHandler @e3 h3)
    . addRoute (toBoundHandler @e2 h2)
    . addRoute (toBoundHandler @e1 h1)
    $ router

-- Arity 5
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5)
  => BuildApi '[e1, e2, e3, e4, e5] (h1, h2, h3, h4, h5) where
  buildApi (h1, h2, h3, h4, h5) router =
    addRoute (toBoundHandler @e5 h5)
    . addRoute (toBoundHandler @e4 h4)
    . addRoute (toBoundHandler @e3 h3)
    . addRoute (toBoundHandler @e2 h2)
    . addRoute (toBoundHandler @e1 h1)
    $ router

-- Arity 6
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6)
  => BuildApi '[e1, e2, e3, e4, e5, e6] (h1, h2, h3, h4, h5, h6) where
  buildApi (h1, h2, h3, h4, h5, h6) router =
    addRoute (toBoundHandler @e6 h6)
    . addRoute (toBoundHandler @e5 h5)
    . addRoute (toBoundHandler @e4 h4)
    . addRoute (toBoundHandler @e3 h3)
    . addRoute (toBoundHandler @e2 h2)
    . addRoute (toBoundHandler @e1 h1)
    $ router

-- Arity 7
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7] (h1, h2, h3, h4, h5, h6, h7) where
  buildApi (h1, h2, h3, h4, h5, h6, h7) router =
    addRoute (toBoundHandler @e7 h7)
    . addRoute (toBoundHandler @e6 h6)
    . addRoute (toBoundHandler @e5 h5)
    . addRoute (toBoundHandler @e4 h4)
    . addRoute (toBoundHandler @e3 h3)
    . addRoute (toBoundHandler @e2 h2)
    . addRoute (toBoundHandler @e1 h1)
    $ router

-- Arity 8
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8] (h1, h2, h3, h4, h5, h6, h7, h8) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8) router =
    addRoute (toBoundHandler @e8 h8)
    . addRoute (toBoundHandler @e7 h7)
    . addRoute (toBoundHandler @e6 h6)
    . addRoute (toBoundHandler @e5 h5)
    . addRoute (toBoundHandler @e4 h4)
    . addRoute (toBoundHandler @e3 h3)
    . addRoute (toBoundHandler @e2 h2)
    . addRoute (toBoundHandler @e1 h1)
    $ router

-- Arity 9
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9] (h1, h2, h3, h4, h5, h6, h7, h8, h9) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9) router =
    addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 10
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10) router =
    addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 11
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11) router =
    addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 12
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12) router =
    addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 13
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13) router =
    addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 14
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14) router =
    addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 15
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15) router =
    addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 16
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16) router =
    addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 17
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17) router =
    addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 18
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18) router =
    addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 19
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18, ToHandler h19)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19) router =
    addRoute (toBoundHandler @e19 h19) . addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 20
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18, ToHandler h19, ToHandler h20)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20) router =
    addRoute (toBoundHandler @e20 h20) . addRoute (toBoundHandler @e19 h19) . addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 21
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18, ToHandler h19, ToHandler h20, ToHandler h21)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21) router =
    addRoute (toBoundHandler @e21 h21) . addRoute (toBoundHandler @e20 h20) . addRoute (toBoundHandler @e19 h19) . addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 22
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18, ToHandler h19, ToHandler h20, ToHandler h21, ToHandler h22)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22) router =
    addRoute (toBoundHandler @e22 h22) . addRoute (toBoundHandler @e21 h21) . addRoute (toBoundHandler @e20 h20) . addRoute (toBoundHandler @e19 h19) . addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 23
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22, HasEndpointInfo e23, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18, ToHandler h19, ToHandler h20, ToHandler h21, ToHandler h22, ToHandler h23)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23) router =
    addRoute (toBoundHandler @e23 h23) . addRoute (toBoundHandler @e22 h22) . addRoute (toBoundHandler @e21 h21) . addRoute (toBoundHandler @e20 h20) . addRoute (toBoundHandler @e19 h19) . addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 24
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22, HasEndpointInfo e23, HasEndpointInfo e24, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18, ToHandler h19, ToHandler h20, ToHandler h21, ToHandler h22, ToHandler h23, ToHandler h24)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24) router =
    addRoute (toBoundHandler @e24 h24) . addRoute (toBoundHandler @e23 h23) . addRoute (toBoundHandler @e22 h22) . addRoute (toBoundHandler @e21 h21) . addRoute (toBoundHandler @e20 h20) . addRoute (toBoundHandler @e19 h19) . addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router

-- Arity 25
instance (HasEndpointInfo e1, HasEndpointInfo e2, HasEndpointInfo e3, HasEndpointInfo e4, HasEndpointInfo e5, HasEndpointInfo e6, HasEndpointInfo e7, HasEndpointInfo e8, HasEndpointInfo e9, HasEndpointInfo e10, HasEndpointInfo e11, HasEndpointInfo e12, HasEndpointInfo e13, HasEndpointInfo e14, HasEndpointInfo e15, HasEndpointInfo e16, HasEndpointInfo e17, HasEndpointInfo e18, HasEndpointInfo e19, HasEndpointInfo e20, HasEndpointInfo e21, HasEndpointInfo e22, HasEndpointInfo e23, HasEndpointInfo e24, HasEndpointInfo e25, ToHandler h1, ToHandler h2, ToHandler h3, ToHandler h4, ToHandler h5, ToHandler h6, ToHandler h7, ToHandler h8, ToHandler h9, ToHandler h10, ToHandler h11, ToHandler h12, ToHandler h13, ToHandler h14, ToHandler h15, ToHandler h16, ToHandler h17, ToHandler h18, ToHandler h19, ToHandler h20, ToHandler h21, ToHandler h22, ToHandler h23, ToHandler h24, ToHandler h25)
  => BuildApi '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25] (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24, h25) where
  buildApi (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24, h25) router =
    addRoute (toBoundHandler @e25 h25) . addRoute (toBoundHandler @e24 h24) . addRoute (toBoundHandler @e23 h23) . addRoute (toBoundHandler @e22 h22) . addRoute (toBoundHandler @e21 h21) . addRoute (toBoundHandler @e20 h20) . addRoute (toBoundHandler @e19 h19) . addRoute (toBoundHandler @e18 h18) . addRoute (toBoundHandler @e17 h17) . addRoute (toBoundHandler @e16 h16) . addRoute (toBoundHandler @e15 h15) . addRoute (toBoundHandler @e14 h14) . addRoute (toBoundHandler @e13 h13) . addRoute (toBoundHandler @e12 h12) . addRoute (toBoundHandler @e11 h11) . addRoute (toBoundHandler @e10 h10) . addRoute (toBoundHandler @e9 h9) . addRoute (toBoundHandler @e8 h8) . addRoute (toBoundHandler @e7 h7) . addRoute (toBoundHandler @e6 h6) . addRoute (toBoundHandler @e5 h5) . addRoute (toBoundHandler @e4 h4) . addRoute (toBoundHandler @e3 h3) . addRoute (toBoundHandler @e2 h2) . addRoute (toBoundHandler @e1 h1) $ router
