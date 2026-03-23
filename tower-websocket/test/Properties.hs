{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef

import Tower.WebSocket.Session
import Servant.Reimagined.Core.Session (Dual)
import qualified Servant.Reimagined.Core.Session as S


-- ===================================================================
-- Mock connection (same pattern as test/Main.hs)
-- ===================================================================

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


-- ===================================================================
-- Property: Send then Recv roundtrip for any Text
-- ===================================================================

type SendRecvProtocol = 'S.Send Text ('S.Recv Text 'S.End)

prop_sendRecvRoundtrip :: Property
prop_sendRecvRoundtrip = property $ do
  txt <- forAll $ Gen.text (Range.linear 0 200) Gen.unicode
  result <- evalIO $ do
    (conn, outbox, inbox) <- mockConn
    -- Pre-load the inbox with the JSON-encoded text (simulating the peer echo)
    writeIORef inbox [Aeson.encode txt]
    withSession @SendRecvProtocol conn $ \session -> do
      session'         <- send txt session
      (received, session'') <- recv session'
      close session''
      -- Verify what was sent
      sent <- readIORef outbox
      pure (sent, received)
  let (sent, received) = result
  -- The sent message should be the JSON encoding of txt
  sent === [Aeson.encode txt]
  -- The received message should equal the original
  received === txt


-- ===================================================================
-- Compile-time property: Dual (Dual s) ~ s
--
-- If Dual is not involutive, these type equalities will fail to
-- compile, so the test suite will not build. This is a compile-time
-- property test — the runtime simply verifies it compiled.
-- ===================================================================

-- Helper: a value-level witness that two types are equal.
-- GHC will refuse to compile this if the constraint is not satisfied.
dualInvolutionWitness :: (Dual (Dual s) ~ s) => ()
dualInvolutionWitness = ()

-- Test several representative session types:

_witness1 :: ()
_witness1 = dualInvolutionWitness @'S.End

_witness2 :: ()
_witness2 = dualInvolutionWitness @('S.Send Text 'S.End)

_witness3 :: ()
_witness3 = dualInvolutionWitness @('S.Recv Text ('S.Send Text 'S.End))

_witness4 :: ()
_witness4 = dualInvolutionWitness @('S.Offer ('S.Send Text 'S.End) ('S.Recv Text 'S.End))

_witness5 :: ()
_witness5 = dualInvolutionWitness @('S.Select ('S.Recv Text 'S.End) ('S.Send Text 'S.End))

_witness6 :: ()
_witness6 = dualInvolutionWitness @('S.Rec ('S.Send Text ('S.Recv Text 'S.Var)))

_witness7 :: ()
_witness7 = dualInvolutionWitness @'S.Var

prop_dualInvolutive :: Property
prop_dualInvolutive = property $ do
  -- If we got here, the module compiled, which means all the
  -- Dual (Dual s) ~ s witnesses above type-checked.
  _witness1 `seq` _witness2 `seq` _witness3 `seq`
    _witness4 `seq` _witness5 `seq` _witness6 `seq`
    _witness7 `seq` success


-- ===================================================================
-- Main
-- ===================================================================

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  ok <- tests
  if ok then pure () else error "Property tests failed"
