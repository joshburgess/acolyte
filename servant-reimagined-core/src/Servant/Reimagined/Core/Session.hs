-- | Session types for WebSocket protocol enforcement.
--
-- A session type describes the exact sequence of messages a WebSocket
-- handler must send and receive. The 'Dual' type family computes the
-- mirror protocol that the client sees.
--
-- At the core level, these are pure type definitions. Runtime enforcement
-- via LinearTypes happens in the server package — a linear @Session s@
-- value must be used exactly once, consuming it to produce the next
-- session state.
--
-- @
-- type ChatProtocol =
--   Recv JoinMsg (Send WelcomeMsg
--     (Rec (Offer
--       (Recv ChatMsg (Send BroadcastMsg Var))
--       (Recv LeaveMsg End))))
-- @
module Servant.Reimagined.Core.Session
  ( -- * Session type kind
    SessionType (..)
    -- * Dual computation
  , Dual
  ) where

import Data.Kind (Type)


-- | A session type describing a WebSocket protocol.
--
-- * @Send a s@ — send a message of type @a@, continue with session @s@
-- * @Recv a s@ — receive a message of type @a@, continue with session @s@
-- * @Offer s1 s2@ — offer the peer a choice between two continuations
-- * @Select s1 s2@ — select one of two offered continuations
-- * @Rec s@ — recursive protocol (binds the recursion variable)
-- * @Var@ — recursion point (unfolds to the enclosing 'Rec')
-- * @End@ — session complete
data SessionType
  = Send Type SessionType
  | Recv Type SessionType
  | Offer SessionType SessionType
  | Select SessionType SessionType
  | Rec SessionType
  | Var
  | End


-- | Compute the dual (mirror) of a session type.
--
-- The dual is what the other side of the protocol sees:
-- sends become receives, offers become selects, and vice versa.
-- Recursive structure is preserved.
--
-- @
-- Dual (Send a s)     ~ Recv a (Dual s)
-- Dual (Recv a s)     ~ Send a (Dual s)
-- Dual (Offer s1 s2)  ~ Select (Dual s1) (Dual s2)
-- Dual (Select s1 s2) ~ Offer (Dual s1) (Dual s2)
-- Dual (Rec s)        ~ Rec (Dual s)
-- Dual Var            ~ Var
-- Dual End            ~ End
-- @
--
-- Involutive: @Dual (Dual s) ~ s@ for all @s@.
type Dual :: SessionType -> SessionType
type family Dual (s :: SessionType) :: SessionType where
  Dual ('Send a s)      = 'Recv a (Dual s)
  Dual ('Recv a s)      = 'Send a (Dual s)
  Dual ('Offer s1 s2)   = 'Select (Dual s1) (Dual s2)
  Dual ('Select s1 s2)  = 'Offer (Dual s1) (Dual s2)
  Dual ('Rec s)          = 'Rec (Dual s)
  Dual 'Var              = 'Var
  Dual 'End              = 'End
