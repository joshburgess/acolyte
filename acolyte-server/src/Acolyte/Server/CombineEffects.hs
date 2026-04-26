-- | Backward-compatible aliases for combined effect-tracked servers.
--
-- 'CombinedServer' is now a type alias for 'EffectfulServer'.
-- 'provideEffect' and 'runCombined' are aliases for 'provide' and 'run'.
-- 'combinedFromRouter' is an alias for 'fromRouter'.
--
-- New code should use 'EffectfulServer', 'provide', 'run', and
-- 'fromRouter' directly from "Acolyte.Server.Effects".
module Acolyte.Server.CombineEffects
  ( -- * Combined effectful server (aliases)
    CombinedServer
  , combinedFromRouter
    -- * Effect tracking (aliases)
  , provideEffect
  , runCombined
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

import Tower (Middleware)
import Http.Core (Request, Response)

import Acolyte.Core.Effect (AllEffectsProvided)
import Acolyte.Server.Router (Router)
import Acolyte.Server.Effects
  ( EffectfulServer, fromRouter, provide, run )
import Tower.Service (Service)


-- | Alias for 'EffectfulServer'. Kept for backward compatibility.
type CombinedServer = EffectfulServer

-- | Alias for 'fromRouter'. Kept for backward compatibility.
combinedFromRouter
  :: forall fullApi
   . Router
  -> EffectfulServer fullApi '[]
combinedFromRouter = fromRouter
{-# INLINE combinedFromRouter #-}

-- | Alias for 'provide'. Kept for backward compatibility.
provideEffect
  :: forall e fullApi provided
   . Middleware IO (Request ByteString) (Response ByteString)
  -> EffectfulServer fullApi provided
  -> EffectfulServer fullApi (e ': provided)
provideEffect = provide @e
{-# INLINE provideEffect #-}

-- | Alias for 'run'. Kept for backward compatibility.
runCombined
  :: forall fullApi provided
   . AllEffectsProvided fullApi provided
  => EffectfulServer fullApi provided
  -> Service IO (Request ByteString) (Response ByteString)
runCombined = run
{-# INLINE runCombined #-}
