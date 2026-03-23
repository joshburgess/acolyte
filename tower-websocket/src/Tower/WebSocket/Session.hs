{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Session type runtime for WebSocket protocols.
--
-- A 'Session' value is a phantom-typed handle whose type parameter
-- tracks the current protocol state. Each operation (send, recv,
-- offer, select) consumes the current session and produces the next
-- one, enforcing the correct message order at compile time.
--
-- The API is designed for single-use: each operation takes a
-- @Session s@ and returns a @Session s'@ for the continuation.
-- While Haskell's type system does not prevent reuse of the old
-- handle at runtime, the type-level state machine ensures that
-- only the correct sequence of operations type-checks.
--
-- @
-- type EchoProtocol = 'Send Text ('Recv Text 'End)
--
-- echoHandler :: Session EchoProtocol -> IO ()
-- echoHandler session = do
--   session'         <- send ("hello" :: Text) session
--   (msg, session'') <- recv session'
--   close session''
-- @
module Tower.WebSocket.Session
  ( -- * Session handle
    Session
    -- * Operations
  , send
  , recv
  , offer
  , select1
  , select2
  , close
  , recurse
  , loop
    -- * Running
  , withSession
    -- * WebSocket connection abstraction
  , WebSocketConn (..)
    -- * Type families
  , Unfold
  ) where

import Servant.Reimagined.Core.Session (SessionType (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS


-- | An abstraction over the underlying WebSocket transport.
--
-- Provide send\/recv\/close callbacks — the session runtime uses
-- these to exchange JSON-encoded messages with the peer.
data WebSocketConn = WebSocketConn
  { wsSend  :: !(LBS.ByteString -> IO ())
  , wsRecv  :: !(IO LBS.ByteString)
  , wsClose :: !(IO ())
  }


-- | A session handle. The phantom type parameter tracks the current
-- protocol state. Each operation transitions to the next state.
data Session (s :: SessionType) where
  Session :: !WebSocketConn -> Session s


-- | Send a JSON-encodable message. The session must be in 'Send' state.
-- Returns the continuation session.
send :: (Aeson.ToJSON a) => a -> Session ('Send a s) -> IO (Session s)
send val (Session conn) = do
  wsSend conn (Aeson.encode val)
  pure (Session conn)


-- | Receive a JSON-decodable message. The session must be in 'Recv' state.
-- Returns the decoded value paired with the continuation session.
recv :: (Aeson.FromJSON a) => Session ('Recv a s) -> IO (a, Session s)
recv (Session conn) = do
  bytes <- wsRecv conn
  case Aeson.eitherDecode bytes of
    Right val -> pure (val, Session conn)
    Left err  -> error ("Session recv: JSON decode error: " ++ err)


-- | Offer the peer a choice between two continuations.
-- The peer sends a selection tag; we branch accordingly.
offer :: Session ('Offer s1 s2) -> IO (Either (Session s1) (Session s2))
offer (Session conn) = do
  choice <- wsRecv conn
  if choice == "\"left\"" || choice == "1"
    then pure (Left (Session conn))
    else pure (Right (Session conn))


-- | Select the first option offered by the peer.
select1 :: Session ('Select s1 s2) -> IO (Session s1)
select1 (Session conn) = do
  wsSend conn "\"left\""
  pure (Session conn)


-- | Select the second option offered by the peer.
select2 :: Session ('Select s1 s2) -> IO (Session s2)
select2 (Session conn) = do
  wsSend conn "\"right\""
  pure (Session conn)


-- | Close the session. The protocol must be in 'End' state.
close :: Session 'End -> IO ()
close (Session conn) = wsClose conn


-- | Enter a recursive protocol. Unwraps 'Rec' to expose the body.
recurse :: Session ('Rec s) -> IO (Session s)
recurse (Session conn) = pure (Session conn)


-- | Jump back to the enclosing 'Rec'. Unfolds 'Var' by substituting
-- the full recursive type. The caller must ensure the enclosing
-- 'Rec' type is available via the 'Unfold' type family.
--
-- Usage: when the protocol reaches 'Var', call 'loop' to get back
-- a session for the 'Rec' body. The type annotation on the enclosing
-- handler typically provides enough context for GHC to infer the
-- unfolded type.
loop :: Session 'Var -> IO (Session s)
loop (Session conn) = pure (Session conn)


-- | Run a session handler with the given 'WebSocketConn'.
-- The handler receives a 'Session' parameterised by the protocol type @s@.
withSession :: forall s a. WebSocketConn -> (Session s -> IO a) -> IO a
withSession conn f = f (Session conn)


-- | Unfold a recursive session type by substituting 'Var' with the
-- full @'Rec s@.
--
-- @
-- Unfold ('Rec ('Recv a ('Send a ('Offer 'Var 'End))))
--   ~ 'Recv a ('Send a ('Offer ('Rec ('Recv a ('Send a ('Offer 'Var 'End)))) 'End))
-- @
type Unfold :: SessionType -> SessionType
type family Unfold (s :: SessionType) :: SessionType where
  Unfold ('Rec s) = UnfoldWith ('Rec s) s

type UnfoldWith :: SessionType -> SessionType -> SessionType
type family UnfoldWith (rec :: SessionType) (s :: SessionType) :: SessionType where
  UnfoldWith rec ('Send a s)     = 'Send a (UnfoldWith rec s)
  UnfoldWith rec ('Recv a s)     = 'Recv a (UnfoldWith rec s)
  UnfoldWith rec ('Offer s1 s2)  = 'Offer (UnfoldWith rec s1) (UnfoldWith rec s2)
  UnfoldWith rec ('Select s1 s2) = 'Select (UnfoldWith rec s1) (UnfoldWith rec s2)
  UnfoldWith rec ('Rec s)        = 'Rec (UnfoldWith rec s)
  UnfoldWith rec 'Var            = rec
  UnfoldWith _   'End            = 'End
