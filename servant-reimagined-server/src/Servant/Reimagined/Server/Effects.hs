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
--   $ effectfulServer @API handlers
--   & provide @Auth (secureHeadersLayer defaultSecureHeaders)
--   & provide @Cors (someCorsMw)
--   & run    -- only compiles because Auth and Cors are provided
-- @
module Servant.Reimagined.Server.Effects
  ( -- * Builder
    EffectfulServer (..)
  , effectfulServer
    -- * Adding effects
  , provide
    -- * Finalizing
  , run
  ) where

import Data.Kind (Type)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))

import Tower (Middleware, Service)
import Tower.Service (Service (..))
import Tower.Layer (applyLayer)
import Http.Core (Request, Response)

import Servant.Reimagined.Core.API (Serves)
import Servant.Reimagined.Core.Effect (AllEffectsProvided)
import Servant.Reimagined.Server.Wiring (BuildServer, mkServer)


-- | A server builder that tracks which effects have been provided.
--
-- @api@ is the API type (type-level list of endpoints).
-- @provided@ is the type-level list of effects discharged so far.
-- Starts as @'[]@ and grows with each 'provide' call.
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


-- | Declare that a middleware effect has been provided, and apply
-- the corresponding tower middleware.
--
-- @
-- & provide @Auth authMiddleware
-- @
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
