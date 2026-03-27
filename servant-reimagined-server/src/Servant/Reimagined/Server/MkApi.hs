{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
    -- * Protected auth enforcement
  , FirstArg
  ) where

import Data.Kind (Type, Constraint)
import Data.ByteString (ByteString)
import GHC.TypeLits (TypeError, ErrorMessage (..))

import Tower.Service (Service (..))
import Http.Core (Request, Response)

import Servant.Reimagined.Core.API (Serves)
import Servant.Reimagined.Core.Wrapper (Protected)
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


-- | Decompose a tuple into its first element and the rest.
--
-- Supports arities 2–25. For a 2-tuple, the tail is the bare second
-- element (not a 1-tuple). This enables 'BuildApi' to recursively
-- peel one handler at a time from a handler tuple.
class SplitTuple t where
  type TupleHead t :: Type
  type TupleTail t :: Type
  tupleHead :: t -> TupleHead t
  tupleTail :: t -> TupleTail t

instance SplitTuple (a, b) where
  type TupleHead (a, b) = a
  type TupleTail (a, b) = b
  tupleHead (a, _) = a
  tupleTail (_, b) = b

instance SplitTuple (a, b, c) where
  type TupleHead (a, b, c) = a
  type TupleTail (a, b, c) = (b, c)
  tupleHead (a, _, _) = a
  tupleTail (_, b, c) = (b, c)

instance SplitTuple (a, b, c, d) where
  type TupleHead (a, b, c, d) = a
  type TupleTail (a, b, c, d) = (b, c, d)
  tupleHead (a, _, _, _) = a
  tupleTail (_, b, c, d) = (b, c, d)

instance SplitTuple (a, b, c, d, e) where
  type TupleHead (a, b, c, d, e) = a
  type TupleTail (a, b, c, d, e) = (b, c, d, e)
  tupleHead (a, _, _, _, _) = a
  tupleTail (_, b, c, d, e) = (b, c, d, e)

instance SplitTuple (a, b, c, d, e, f) where
  type TupleHead (a, b, c, d, e, f) = a
  type TupleTail (a, b, c, d, e, f) = (b, c, d, e, f)
  tupleHead (a, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f) = (b, c, d, e, f)

instance SplitTuple (a, b, c, d, e, f, g) where
  type TupleHead (a, b, c, d, e, f, g) = a
  type TupleTail (a, b, c, d, e, f, g) = (b, c, d, e, f, g)
  tupleHead (a, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g) = (b, c, d, e, f, g)

instance SplitTuple (a, b, c, d, e, f, g, h) where
  type TupleHead (a, b, c, d, e, f, g, h) = a
  type TupleTail (a, b, c, d, e, f, g, h) = (b, c, d, e, f, g, h)
  tupleHead (a, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h) = (b, c, d, e, f, g, h)

instance SplitTuple (a, b, c, d, e, f, g, h, i) where
  type TupleHead (a, b, c, d, e, f, g, h, i) = a
  type TupleTail (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h, i)
  tupleHead (a, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h, i)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j) = (b, c, d, e, f, g, h, i, j)
  tupleHead (a, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j) = (b, c, d, e, f, g, h, i, j)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k) = (b, c, d, e, f, g, h, i, j, k)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k) = (b, c, d, e, f, g, h, i, j, k)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l) = (b, c, d, e, f, g, h, i, j, k, l)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l) = (b, c, d, e, f, g, h, i, j, k, l)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m) = (b, c, d, e, f, g, h, i, j, k, l, m)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m) = (b, c, d, e, f, g, h, i, j, k, l, m)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (b, c, d, e, f, g, h, i, j, k, l, m, n)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n) = (b, c, d, e, f, g, h, i, j, k, l, m, n)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)

instance SplitTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  type TupleHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = a
  type TupleTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
  tupleHead (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
  tupleTail (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)


-- | Class that walks the API list and handler tuple, populating a Router.
--
-- Each endpoint gets its routing metadata from 'HasEndpointInfo',
-- and each handler is converted via 'ToHandler'. Uses 'SplitTuple'
-- to recursively peel handlers from the tuple.
class BuildApi (api :: [Type]) handlers where
  buildApi :: handlers -> Router -> Router

-- Single endpoint: bare handler (not a tuple)
instance (HasEndpointInfo e, ToHandler h)
  => BuildApi '[e] h where
  buildApi h router =
    addRoute (toBoundHandler @e h) router

-- Two+ endpoints: peel the first handler from the tuple, recurse on the rest
instance {-# OVERLAPPABLE #-}
  ( HasEndpointInfo e
  , ToHandler (TupleHead handlers)
  , SplitTuple handlers
  , BuildApi es (TupleTail handlers)
  )
  => BuildApi (e ': es) handlers where
  buildApi handlers router =
    buildApi @es (tupleTail handlers)
      (addRoute (toBoundHandler @e (tupleHead handlers)) router)


-- ===================================================================
-- Protected auth enforcement
-- ===================================================================

-- | Extract the first argument type from a function.
-- Produces a clear 'TypeError' if the handler is not a function
-- (i.e., it doesn't accept the auth type as its first argument).
type FirstArg :: Type -> Type
type family FirstArg (f :: Type) :: Type where
  FirstArg (a -> b) = a
  FirstArg other = TypeError
    ( 'Text "Protected endpoint handler must be a function (auth -> ...),"
      ':$$: 'Text "but got: " ':<>: 'ShowType other
      ':$$: 'Text "The handler's first argument must be the auth type."
    )

-- Single Protected endpoint: enforce auth as first handler argument
instance {-# OVERLAPPING #-}
  ( HasEndpointInfo (Protected auth inner)
  , ToHandler h
  , FirstArg h ~ auth
  ) => BuildApi '[Protected auth inner] h where
  buildApi h router =
    addRoute (toBoundHandler @(Protected auth inner) h) router

-- Protected in a multi-endpoint API: enforce auth on the head handler
instance {-# OVERLAPPING #-}
  ( HasEndpointInfo (Protected auth inner)
  , ToHandler (TupleHead handlers)
  , FirstArg (TupleHead handlers) ~ auth
  , SplitTuple handlers
  , BuildApi es (TupleTail handlers)
  ) => BuildApi (Protected auth inner ': es) handlers where
  buildApi handlers router =
    buildApi @es (tupleTail handlers)
      (addRoute (toBoundHandler @(Protected auth inner) (tupleHead handlers)) router)
