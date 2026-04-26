-- | Runtime request validation middleware.
--
-- Validates incoming requests against the API type before they reach
-- handlers: correct method for matched path, path actually matches
-- a known route. Rejects non-matching requests with 400/404 before
-- handler dispatch.
--
-- @
-- svc |> validationLayer @MyAPI
-- @
module Acolyte.Server.Validate
  ( -- * Validation middleware
    validationLayer
    -- * Route table
  , RouteInfo (..)
  , BuildRouteTable (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status404, status405)

import Spire (Middleware, middleware)
import Spire.Service (Service (..))
import Http.Core (Request (..), Response (..))

import Acolyte.Server.Handler (HasEndpointInfo (..), MatchResult (..))


-- | Info about a single route for validation.
data RouteInfo = RouteInfo
  { riMethod  :: !ByteString
  , riPattern :: !Text
  , riMatcher :: [Text] -> Maybe MatchResult
  }


-- | Build a route table from an API type.
class BuildRouteTable (api :: [Type]) where
  routeTable :: [RouteInfo]

instance BuildRouteTable '[] where
  routeTable = []

instance (HasEndpointInfo e, BuildRouteTable rest)
  => BuildRouteTable (e ': rest) where
    routeTable = RouteInfo
      { riMethod  = endpointMethod @e
      , riPattern = endpointPattern @e
      , riMatcher = endpointMatcher @e
      } : routeTable @rest


-- | Validation middleware that rejects requests not matching the API.
--
-- Checks:
-- 1. Path matches at least one endpoint → if not, 404
-- 2. Method matches for the matched path → if not, 405
--
-- This runs BEFORE the router, catching obviously invalid requests
-- early (useful when the router has fallback behavior or when you
-- want a strict "only defined routes" policy).
validationLayer
  :: forall api. BuildRouteTable api
  => Middleware IO (Request ByteString) (Response ByteString)
validationLayer = middleware $ \(Service inner) -> Service $ \req -> do
  let segments = requestPath req
      method   = requestMethod req
      routes   = routeTable @api
      pathMatches = [ ri | ri <- routes, isJust (riMatcher ri segments) ]
  case pathMatches of
    [] -> pure (Response status404 [] "Not Found: no route matches this path")
    matches ->
      if any (\ri -> riMethod ri == method) matches
      then inner req  -- valid: let it through
      else pure (Response status405 [] "Method Not Allowed")


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False
