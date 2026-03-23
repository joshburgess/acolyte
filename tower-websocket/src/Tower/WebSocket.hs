-- | WebSocket session types for servant-reimagined.
--
-- This module re-exports everything needed to write session-typed
-- WebSocket handlers. Session types are defined in
-- "Servant.Reimagined.Core.Session"; runtime enforcement lives in
-- "Tower.WebSocket.Session".
--
-- @
-- import Tower.WebSocket
-- import Servant.Reimagined.Core.Session (SessionType (..))
--
-- type ChatProtocol =
--   'Recv Text ('Send Text ('Rec ('Offer
--     ('Recv Text ('Send Text 'Var))
--     ('Recv Text 'End))))
-- @
module Tower.WebSocket
  ( -- * Session runtime
    module Tower.WebSocket.Session
    -- * Session type definitions (re-export from core)
  , SessionType (..)
  , Dual
  ) where

import Tower.WebSocket.Session
import Servant.Reimagined.Core.Session (SessionType (..), Dual)
