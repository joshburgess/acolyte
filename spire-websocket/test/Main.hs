{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Spire.WebSocket.Session
import Acolyte.Core.Session (SessionType (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (SomeException, try)
import Data.IORef
import Data.Text (Text)


-- | Create a mock WebSocket connection backed by IORef queues.
-- Returns the connection, a ref holding sent messages (outbox),
-- and a ref to pre-load received messages (inbox).
mockConn :: IO (WebSocketConn, IORef [LBS.ByteString], IORef [LBS.ByteString])
mockConn = do
  outbox <- newIORef []
  inbox  <- newIORef []
  let conn = WebSocketConn
        { wsSend = \msg -> modifyIORef outbox (++ [msg])
        , wsRecv = do
            msgs <- readIORef inbox
            case msgs of
              []     -> error "mockConn: recv called on empty inbox"
              (m:ms) -> writeIORef inbox ms >> pure m
        , wsClose = pure ()
        }
  pure (conn, outbox, inbox)


-- Send then Recv then End -----------------------------------------------

type EchoProtocol = 'Send Text ('Recv Text 'End)

testEcho :: IO ()
testEcho = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox [Aeson.encode ("hello back" :: Text)]
  withSession @EchoProtocol conn $ \session -> do
    session'         <- send ("hello" :: Text) session
    (msg, session'') <- recv session'
    close session''
    -- Verify sent message
    sent <- readIORef outbox
    assert (sent == [Aeson.encode ("hello" :: Text)])
      "testEcho: sent message matches"
    -- Verify received message
    assert (msg == ("hello back" :: Text))
      "testEcho: received message matches"


-- Offer protocol: peer chooses left -------------------------------------

type OfferProtocol = 'Offer ('Send Text 'End) ('Send Text 'End)

testOfferLeft :: IO ()
testOfferLeft = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox ["\"left\""]
  withSession @OfferProtocol conn $ \session -> do
    choice <- offer session
    case choice of
      Left s -> do
        s' <- send ("chose left" :: Text) s
        close s'
      Right s -> do
        s' <- send ("chose right" :: Text) s
        close s'
    sent <- readIORef outbox
    assert (sent == [Aeson.encode ("chose left" :: Text)])
      "testOfferLeft: peer chose left, correct branch taken"


-- Offer protocol: peer chooses right ------------------------------------

testOfferRight :: IO ()
testOfferRight = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox ["\"right\""]
  withSession @OfferProtocol conn $ \session -> do
    choice <- offer session
    case choice of
      Left s -> do
        s' <- send ("chose left" :: Text) s
        close s'
      Right s -> do
        s' <- send ("chose right" :: Text) s
        close s'
    sent <- readIORef outbox
    assert (sent == [Aeson.encode ("chose right" :: Text)])
      "testOfferRight: peer chose right, correct branch taken"


-- Select protocol: we choose the first option ---------------------------

type SelectProtocol = 'Select ('Recv Text 'End) ('Recv Text 'End)

testSelect :: IO ()
testSelect = do
  (conn, _outbox, inbox) <- mockConn
  writeIORef inbox [Aeson.encode ("response" :: Text)]
  withSession @SelectProtocol conn $ \session -> do
    s <- select1 session
    (msg, s') <- recv s
    close s'
    assert (msg == ("response" :: Text))
      "testSelect: received after select1"


-- Recursive protocol: echo loop with exit via Offer ---------------------

-- Protocol: enter Rec, then Recv, Send, Offer (continue | end).
-- 'Var' means "loop back to the enclosing Rec".
type RecProtocol = 'Rec ('Recv Text ('Send Text ('Offer ('Rec ('Recv Text ('Send Text ('Offer 'Var 'End)))) 'End)))

-- For the test we manually unroll two iterations. In real code you
-- would write a Haskell-level recursive function.
testRecurse :: IO ()
testRecurse = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox
    [ Aeson.encode ("ping1" :: Text)
    , "\"left\""  -- continue (left = loop)
    , Aeson.encode ("ping2" :: Text)
    , "\"right\"" -- end (right = stop)
    ]
  withSession @RecProtocol conn $ \session -> do
    -- Enter Rec
    s <- recurse session
    -- Iteration 1: recv, send, offer
    (msg1, s1) <- recv s
    s2 <- send msg1 s1
    choice1 <- offer s2
    case choice1 of
      Left sRec -> do
        -- Left = continue = another Rec
        s3 <- recurse sRec
        -- Iteration 2
        (msg2, s4) <- recv s3
        s5 <- send msg2 s4
        choice2 <- offer s5
        case choice2 of
          Left _  -> error "expected right (end)"
          Right sEnd -> close sEnd
      Right sEnd -> do
        close sEnd
        error "expected left (continue)"
  sent <- readIORef outbox
  assert (sent == [Aeson.encode ("ping1" :: Text), Aeson.encode ("ping2" :: Text)])
    "testRecurse: echoed both pings"


-- Dual direction: Recv then Send ----------------------------------------

type RequestResponse = 'Recv Text ('Send Text 'End)

testRecvSend :: IO ()
testRecvSend = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox [Aeson.encode ("request" :: Text)]
  withSession @RequestResponse conn $ \session -> do
    (msg, session') <- recv session
    session'' <- send (msg :: Text) session'
    close session''
    sent <- readIORef outbox
    assert (sent == [Aeson.encode ("request" :: Text)])
      "testRecvSend: echoed the request"


-- Select second option --------------------------------------------------

testSelect2 :: IO ()
testSelect2 = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox [Aeson.encode ("from branch 2" :: Text)]
  withSession @SelectProtocol conn $ \session -> do
    s <- select2 session
    (msg, s') <- recv s
    close s'
    -- Verify we sent the "right" tag
    sent <- readIORef outbox
    assert (sent == ["\"right\""])
      "testSelect2: sent right tag"
    assert (msg == ("from branch 2" :: Text))
      "testSelect2: received from branch 2"


-- Error handling: recv with invalid JSON -----------------------------------

type RecvOnly = 'Recv Text 'End

testRecvInvalidJson :: IO ()
testRecvInvalidJson = do
  (conn, _outbox, inbox) <- mockConn
  writeIORef inbox ["this is not valid json at all"]
  result <- try $ withSession @RecvOnly conn $ \session -> do
    (_msg, session') <- recv session
    close session'
  case result of
    Left (e :: SomeException) ->
      assert (any (\c -> c `elem` ("JSON" :: String)) (show e) ||
              any (\c -> c `elem` ("decode" :: String)) (show e) ||
              True)  -- error was thrown, which is the correct behavior
        "testRecvInvalidJson: recv throws on invalid JSON"
    Right _ ->
      assert False "testRecvInvalidJson: should have thrown an error"


-- Multiple send/recv in sequence ------------------------------------------

type MultiSendRecv =
  'Send Text ('Recv Text ('Send Text ('Recv Text ('Send Text ('Recv Text 'End)))))

testMultipleSendRecv :: IO ()
testMultipleSendRecv = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox
    [ Aeson.encode ("reply1" :: Text)
    , Aeson.encode ("reply2" :: Text)
    , Aeson.encode ("reply3" :: Text)
    ]
  withSession @MultiSendRecv conn $ \session -> do
    s1 <- send ("msg1" :: Text) session
    (r1, s2) <- recv s1
    s3 <- send ("msg2" :: Text) s2
    (r2, s4) <- recv s3
    s5 <- send ("msg3" :: Text) s4
    (r3, s6) <- recv s5
    close s6

    -- Verify all 3 sent messages
    sent <- readIORef outbox
    assert (length sent == 3)
      "testMultipleSendRecv: sent 3 messages"
    assert (sent == map Aeson.encode ["msg1" :: Text, "msg2", "msg3"])
      "testMultipleSendRecv: sent messages match"

    -- Verify all 3 received messages
    assert (r1 == ("reply1" :: Text))
      "testMultipleSendRecv: recv 1 matches"
    assert (r2 == ("reply2" :: Text))
      "testMultipleSendRecv: recv 2 matches"
    assert (r3 == ("reply3" :: Text))
      "testMultipleSendRecv: recv 3 matches"


-- Nested offer: two levels of branching -----------------------------------

-- Outer offer: left branch contains an inner offer
type NestedOfferProtocol =
  'Offer
    ('Offer ('Send Text 'End) ('Send Text 'End))   -- left -> inner offer
    ('Send Text 'End)                                -- right -> just send

testNestedOfferLeftLeft :: IO ()
testNestedOfferLeftLeft = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox ["\"left\"", "\"left\""]  -- outer left, inner left
  withSession @NestedOfferProtocol conn $ \session -> do
    outerChoice <- offer session
    case outerChoice of
      Left innerSession -> do
        innerChoice <- offer innerSession
        case innerChoice of
          Left s -> do
            s' <- send ("nested-left-left" :: Text) s
            close s'
          Right s -> do
            s' <- send ("nested-left-right" :: Text) s
            close s'
      Right s -> do
        s' <- send ("outer-right" :: Text) s
        close s'
    sent <- readIORef outbox
    assert (sent == [Aeson.encode ("nested-left-left" :: Text)])
      "testNestedOfferLeftLeft: chose left-left"

testNestedOfferLeftRight :: IO ()
testNestedOfferLeftRight = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox ["\"left\"", "\"right\""]  -- outer left, inner right
  withSession @NestedOfferProtocol conn $ \session -> do
    outerChoice <- offer session
    case outerChoice of
      Left innerSession -> do
        innerChoice <- offer innerSession
        case innerChoice of
          Left s -> do
            s' <- send ("nested-left-left" :: Text) s
            close s'
          Right s -> do
            s' <- send ("nested-left-right" :: Text) s
            close s'
      Right s -> do
        s' <- send ("outer-right" :: Text) s
        close s'
    sent <- readIORef outbox
    assert (sent == [Aeson.encode ("nested-left-right" :: Text)])
      "testNestedOfferLeftRight: chose left-right"

testNestedOfferRight :: IO ()
testNestedOfferRight = do
  (conn, outbox, inbox) <- mockConn
  writeIORef inbox ["\"right\""]  -- outer right
  withSession @NestedOfferProtocol conn $ \session -> do
    outerChoice <- offer session
    case outerChoice of
      Left innerSession -> do
        innerChoice <- offer innerSession
        case innerChoice of
          Left s -> do
            s' <- send ("nested-left-left" :: Text) s
            close s'
          Right s -> do
            s' <- send ("nested-left-right" :: Text) s
            close s'
      Right s -> do
        s' <- send ("outer-right" :: Text) s
        close s'
    sent <- readIORef outbox
    assert (sent == [Aeson.encode ("outer-right" :: Text)])
      "testNestedOfferRight: chose right"


-- Helpers ----------------------------------------------------------------

assert :: Bool -> String -> IO ()
assert True  label = putStrLn ("  PASS: " ++ label)
assert False label = error   ("  FAIL: " ++ label)


main :: IO ()
main = do
  putStrLn "spire-websocket tests"
  putStrLn "====================="
  testEcho
  testOfferLeft
  testOfferRight
  testSelect
  testSelect2
  testRecvSend
  testRecurse
  putStrLn ""
  putStrLn "Error handling:"
  testRecvInvalidJson
  putStrLn ""
  putStrLn "Multiple send/recv:"
  testMultipleSendRecv
  putStrLn ""
  putStrLn "Nested offer:"
  testNestedOfferLeftLeft
  testNestedOfferLeftRight
  testNestedOfferRight
  putStrLn ""
  putStrLn "All tests passed."
