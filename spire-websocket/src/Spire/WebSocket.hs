-- | WebSocket session types for acolyte.
--
-- This module re-exports everything needed to write session-typed
-- WebSocket handlers. Session types are defined in
-- "Acolyte.Core.Session"; runtime enforcement lives in
-- "Spire.WebSocket.Session".
--
-- @
-- import Spire.WebSocket
-- import Acolyte.Core.Session (SessionType (..))
--
-- type ChatProtocol =
--   'Recv Text ('Send Text ('Rec ('Offer
--     ('Recv Text ('Send Text 'Var))
--     ('Recv Text 'End))))
-- @
module Spire.WebSocket
  ( -- * Session runtime
    module Spire.WebSocket.Session
    -- * Session type definitions (re-export from core)
  , SessionType (..)
  , Dual
  ) where

import Spire.WebSocket.Session
import Acolyte.Core.Session (SessionType (..), Dual)
