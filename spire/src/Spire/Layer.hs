-- | Layers and Middleware for composing services.
--
-- A 'Layer' wraps an inner service to produce an outer service.
-- It is a middleware factory — it carries configuration and produces
-- a transformed service when applied.
--
-- A 'Middleware' is a Layer that does not change the request/response
-- types. Most HTTP middleware (CORS, tracing, timeouts) is this kind.
--
-- @
-- -- A logging middleware
-- logging :: Middleware IO Request Response
-- logging = middleware $ \inner -> Service $ \req -> do
--   putStrLn $ "Request: " ++ show req
--   resp <- runService inner req
--   putStrLn $ "Response: " ++ show resp
--   pure resp
--
-- -- Apply it
-- myService' = applyLayer logging myService
-- @
module Spire.Layer
  ( -- * Layer type
    Layer (..)
    -- * Middleware (same req/resp)
  , Middleware
    -- * Construction
  , middleware
    -- * Application
  , applyLayer
  , (|>)
  , (<|)
    -- * Composition
  , composeLayer
  , identity
    -- * Combinators
  , before
  , after
  , around
  ) where

import Spire.Service (Service (..))


-- | A layer wraps an inner service to produce an outer service.
--
-- Layers are middleware factories. The type parameters are:
--
-- * @m@ — the monad
-- * @reqI, respI@ — the inner service's request/response types
-- * @reqO, respO@ — the outer (wrapped) service's request/response types
--
-- Most layers don't change types (see 'Middleware'). Layers that DO
-- change types include request/response body transformers, protocol
-- adapters, etc.
newtype Layer m reqI respI reqO respO = Layer
  { runLayer :: Service m reqI respI -> Service m reqO respO
  }


-- | A middleware does not change request/response types.
--
-- This is the common case: CORS, logging, auth, compression all
-- take a @Service m req resp@ and produce a @Service m req resp@.
type Middleware m req resp = Layer m req resp req resp


-- | Construct a middleware from a service transformer.
--
-- @
-- logging :: Middleware IO String String
-- logging = middleware $ \inner -> Service $ \req -> do
--   putStrLn $ ">> " ++ req
--   resp <- runService inner req
--   putStrLn $ "<< " ++ resp
--   pure resp
-- @
middleware :: (Service m req resp -> Service m req resp) -> Middleware m req resp
middleware = Layer
{-# INLINABLE middleware #-}


-- | Apply a layer to a service, producing a new service.
--
-- @
-- myService' = applyLayer loggingLayer myService
-- @
applyLayer :: Layer m reqI respI reqO respO -> Service m reqI respI -> Service m reqO respO
applyLayer (Layer f) = f
{-# INLINABLE applyLayer #-}


-- | Apply a layer to a service (operator form, left-to-right).
--
-- Read as "service piped through layer":
--
-- @
-- myService |> logging |> cors |> timeout
-- @
--
-- Layers apply inside-out: @logging@ wraps @myService@, then @cors@
-- wraps that, then @timeout@ wraps the whole thing. The outermost
-- layer (timeout) sees the request first.
--
-- Wait — that's wrong. Let's be precise. @s |> l@ means @l@ wraps @s@.
-- So @myService |> logging |> cors@ means cors wraps (logging wraps myService).
-- The outermost layer (cors) sees the request first.
(|>) :: Service m reqI respI -> Layer m reqI respI reqO respO -> Service m reqO respO
(|>) svc layer = applyLayer layer svc
{-# INLINE (|>) #-}
infixl 1 |>


-- | Apply a layer to a service (operator form, right-to-left).
--
-- @
-- timeout <| cors <| logging <| myService
-- @
(<|) :: Layer m reqI respI reqO respO -> Service m reqI respI -> Service m reqO respO
(<|) = applyLayer
{-# INLINE (<|) #-}
infixr 1 <|


-- | Compose two layers. The result applies @inner@ first, then @outer@.
--
-- @
-- combined = composeLayer outer inner
-- -- equivalent to: \\svc -> outer (inner svc)
-- @
composeLayer
  :: Layer m req2 resp2 req3 resp3
  -> Layer m req1 resp1 req2 resp2
  -> Layer m req1 resp1 req3 resp3
composeLayer (Layer outer) (Layer inner) = Layer (outer . inner)
{-# INLINABLE composeLayer #-}


-- | The identity layer — wraps a service without modification.
identity :: Layer m req resp req resp
identity = Layer id
{-# INLINABLE identity #-}


-- | Run a callback before the service processes the request.
--
-- @
-- logRequest :: Middleware IO Request Response
-- logRequest = before $ \req -> putStrLn ("Request: " ++ show req)
-- @
before :: Monad m => (req -> m ()) -> Middleware m req resp
before hook = Layer $ \(Service inner) -> Service $ \req -> do
  hook req
  inner req
{-# INLINABLE before #-}


-- | Run a callback after the service produces the response.
--
-- @
-- logResponse :: Middleware IO Request Response
-- logResponse = after $ \req resp -> putStrLn ("Response: " ++ show resp)
-- @
after :: Monad m => (req -> resp -> m ()) -> Middleware m req resp
after hook = Layer $ \(Service inner) -> Service $ \req -> do
  resp <- inner req
  hook req resp
  pure resp
{-# INLINABLE after #-}


-- | Run a callback that wraps the entire service call.
--
-- @
-- timing :: Middleware IO Request Response
-- timing = around $ \req callInner -> do
--   t0 <- getCurrentTime
--   resp <- callInner req
--   t1 <- getCurrentTime
--   putStrLn $ "Took: " ++ show (diffUTCTime t1 t0)
--   pure resp
-- @
around :: (req -> (req -> m resp) -> m resp) -> Middleware m req resp
around wrapper = Layer $ \(Service inner) -> Service $ \req ->
  wrapper req inner
{-# INLINABLE around #-}
