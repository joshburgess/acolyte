{-# LANGUAGE OverloadedStrings #-}
-- | WebSocket session-typed chat protocol.
--
-- Demonstrates compile-time enforcement of message ordering using
-- session types. The protocol ensures that a client must:
--   1. Send a JoinMsg
--   2. Receive a WelcomeMsg
--   3. Loop: either receive a ChatMsg (and broadcast) or receive a LeaveMsg and end
--
-- The Dual (client-side) protocol is:
--
-- @
-- Dual ChatProtocol =
--   'Recv JoinMsg ('Send WelcomeMsg
--     ('Rec ('Select
--       ('Send ChatMsg ('Recv BroadcastMsg 'Var))
--       ('Send LeaveMsg 'End))))
-- @
--
-- Swapping every Send/Recv, Offer/Select — the client sends where the
-- server receives, and selects where the server offers.
module Main (main) where

import Data.IORef
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Acolyte.Prelude


-- ===================================================================
-- 1. Message types
-- ===================================================================

data JoinMsg = JoinMsg { joinUser :: !Text }
  deriving (Generic, Show, ToJSON, FromJSON)

data WelcomeMsg = WelcomeMsg { welcomeText :: !Text, onlineCount :: !Int }
  deriving (Generic, Show, ToJSON, FromJSON)

data ChatMsg = ChatMsg { chatUser :: !Text, chatText :: !Text }
  deriving (Generic, Show, ToJSON, FromJSON)

data BroadcastMsg = BroadcastMsg { broadFrom :: !Text, broadText :: !Text }
  deriving (Generic, Show, ToJSON, FromJSON)

data LeaveMsg = LeaveMsg { leaveUser :: !Text }
  deriving (Generic, Show, ToJSON, FromJSON)


-- ===================================================================
-- 2. Session type: the chat protocol (server's perspective)
-- ===================================================================

-- Server receives a join, sends a welcome, then enters a recursive
-- loop that offers the client a choice: send another chat message,
-- or leave.
type ChatProtocol =
  'Recv JoinMsg ('Send WelcomeMsg
    ('Rec ('Offer
      ('Recv ChatMsg ('Send BroadcastMsg 'Var))
      ('Recv LeaveMsg 'End))))


-- ===================================================================
-- 3. Session-typed server handler
-- ===================================================================

-- | Handle one client session. The session type ensures every
-- recv/send/offer/recurse/close is in the correct order.
chatHandler :: Session ChatProtocol -> IO ()
chatHandler s0 = do
  -- Phase 1: join handshake
  (JoinMsg user, s1) <- recv s0
  putStrLn $ "  [join] " ++ T.unpack user

  s2 <- send (WelcomeMsg ("Welcome, " <> user <> "!") 1) s1

  -- Phase 2: enter the recursive chat loop
  s3 <- recurse s2
  chatLoop user s3

  where
    -- The Rec body after recurse is:
    --   'Offer ('Recv ChatMsg ('Send BroadcastMsg 'Var))
    --          ('Recv LeaveMsg 'End)
    -- After Unfold, 'Var is replaced with the full 'Rec ..., but
    -- recurse already strips 'Rec, giving us the body directly.
    chatLoop :: Text -> Session ('Offer
        ('Recv ChatMsg ('Send BroadcastMsg 'Var))
        ('Recv LeaveMsg 'End)) -> IO ()
    chatLoop user sLoop = do
      choice <- offer sLoop
      case choice of
        Left sChat -> do
          -- Client chose to send a chat message
          (ChatMsg _from msg, sChat') <- recv sChat
          putStrLn $ "  [chat] " ++ T.unpack user ++ ": " ++ T.unpack msg
          sChat'' <- send (BroadcastMsg user msg) sChat'
          -- Loop back (Var -> Rec body)
          sNext <- loop sChat''
          chatLoop user sNext

        Right sLeave -> do
          -- Client chose to leave
          (LeaveMsg who, sEnd) <- recv sLeave
          putStrLn $ "  [leave] " ++ T.unpack who
          close sEnd


-- ===================================================================
-- 4. Mock WebSocket connection (for demonstration)
-- ===================================================================

-- | Build a mock WebSocketConn that replays a scripted conversation.
-- In a real application, this would wrap an actual WebSocket library.
mockConn :: [LBS.ByteString] -> IO (WebSocketConn, IORef [LBS.ByteString])
mockConn script = do
  ref <- newIORef script
  sentRef <- newIORef []
  let conn = WebSocketConn
        { wsSend  = \msg -> do
            modifyIORef' sentRef (msg :)
            putStrLn $ "  -> sent: " ++ show msg
        , wsRecv  = do
            msgs <- readIORef ref
            case msgs of
              []     -> error "mock: no more messages"
              (m:ms) -> do
                writeIORef ref ms
                putStrLn $ "  <- recv: " ++ show m
                pure m
        , wsClose = putStrLn "  [connection closed]"
        }
  pure (conn, sentRef)


-- ===================================================================
-- 5. Run the demo
-- ===================================================================

main :: IO ()
main = do
  putStrLn "Session-typed chat protocol demo"
  putStrLn "================================"
  putStrLn ""

  -- Script a client conversation:
  --   1. Send JoinMsg
  --   2. Receive WelcomeMsg (automatic)
  --   3. Choose left (chat) -> send ChatMsg, receive BroadcastMsg
  --   4. Choose left (chat) -> send ChatMsg, receive BroadcastMsg
  --   5. Choose right (leave) -> send LeaveMsg
  let script =
        [ Aeson.encode (JoinMsg "alice")
        , "\"left\""                                        -- offer choice: chat
        , Aeson.encode (ChatMsg "alice" "hello everyone!")
        , "\"left\""                                        -- offer choice: chat
        , Aeson.encode (ChatMsg "alice" "how's it going?")
        , "\"right\""                                       -- offer choice: leave
        , Aeson.encode (LeaveMsg "alice")
        ]

  (conn, _sent) <- mockConn script

  putStrLn "Running server-side session handler:\n"
  withSession @ChatProtocol conn chatHandler

  putStrLn "\nDone. All session transitions were type-checked at compile time."
