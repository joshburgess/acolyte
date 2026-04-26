{-# LANGUAGE OverloadedStrings #-}
-- | Streaming handler support: SSE, chunked responses, and bidirectional streams.
--
-- Server-Sent Events (SSE) for 'ServerStream' endpoints:
--
-- @
-- handler :: (SSEvent Item -> IO ()) -> IO ()
-- handler emit = do
--   items <- loadItems
--   mapM_ (\i -> emit (SSEvent (Json i) Nothing Nothing)) items
-- @
--
-- Chunked request bodies for 'ClientStream' endpoints:
--
-- @
-- handler :: IO (Maybe ByteString) -> IO (Json Summary)
-- handler readChunk = do
--   chunks <- collectChunks readChunk
--   pure (Json (summarize chunks))
-- @
--
-- Bidirectional via WebSocket for 'BidiStream' endpoints:
--
-- @
-- handler :: IO (Maybe Msg) -> (Msg -> IO ()) -> IO ()
-- handler recv send = do
--   mmsg <- recv
--   case mmsg of
--     Just msg -> send (process msg) >> handler recv send
--     Nothing  -> pure ()
-- @
module Acolyte.Server.Streaming
  ( -- * Server-Sent Events
    SSEvent (..)
  , sseEvent
  , sseData
    -- * Streaming handler types
  , ServerStreamHandler
  , ClientStreamHandler
  , BidiStreamHandler
    -- * SSE response builders
  , sseResponse
  , sseResponseSync
  , sseChunk
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Network.HTTP.Types (status200)

import qualified Data.Aeson as Aeson

import Http.Core (Response (..))
import Http.Core.Body (Body (..), BodyChunk (..), streamBody)


-- | A Server-Sent Event.
--
-- @
-- SSEvent { ssePayload = Json item, sseId = Just "1", sseEventType = Nothing }
-- @
data SSEvent a = SSEvent
  { ssePayload   :: !a
  , sseId        :: !(Maybe Text)
  , sseEventType :: !(Maybe Text)
  }

-- | Convenience: data-only SSE (no id or event type).
sseData :: a -> SSEvent a
sseData a = SSEvent a Nothing Nothing

-- | Convenience: SSE with an event type.
sseEvent :: Text -> a -> SSEvent a
sseEvent typ a = SSEvent a Nothing (Just typ)


-- | A handler that pushes server-sent events to the client.
-- Receives an emit function and calls it for each event.
type ServerStreamHandler a = (SSEvent a -> IO ()) -> IO ()

-- | A handler that reads chunks from a client upload.
-- Receives a pull function that returns Nothing at end-of-stream.
type ClientStreamHandler a r = (IO (Maybe a) -> IO r)

-- | A handler that reads from the client and writes to the client.
-- Receives pull (read) and push (write) functions.
type BidiStreamHandler a b = IO (Maybe a) -> (b -> IO ()) -> IO ()


-- | Encode an SSEvent as an SSE-format ByteString chunk.
sseChunk :: Aeson.ToJSON a => SSEvent a -> ByteString
sseChunk ev = BS.concat $ concat
  [ case sseEventType ev of
      Nothing  -> []
      Just typ -> ["event: ", TE.encodeUtf8 typ, "\n"]
  , case sseId ev of
      Nothing -> []
      Just i  -> ["id: ", TE.encodeUtf8 i, "\n"]
  , ["data: ", LBS.toStrict (Aeson.encode (ssePayload ev)), "\n\n"]
  ]


-- | Build a streaming 'Response Body' from a 'ServerStreamHandler'.
--
-- The handler runs concurrently in a forked thread, and events are
-- delivered to the response stream as they are produced. This enables
-- true server-push: the client receives events incrementally rather
-- than all at once after the handler completes.
--
-- Sets @Content-Type: text/event-stream@ and produces a 'BodyStream'
-- that emits SSE-formatted chunks. The tower-server layer renders
-- this with @Transfer-Encoding: chunked@.
sseResponse :: Aeson.ToJSON a => ServerStreamHandler a -> IO (Response Body)
sseResponse handler = do
  -- MVar-based queue: handler writes chunks, response stream reads them.
  -- Nothing signals end-of-stream.
  queue <- newEmptyMVar :: IO (MVar (Maybe ByteString))

  -- Fork the handler so events stream as they are produced
  _ <- forkIO $ do
    handler (\ev -> putMVar queue (Just (sseChunk ev)))
    putMVar queue Nothing  -- signal completion

  let pull = do
        mChunk <- takeMVar queue
        case mChunk of
          Nothing -> pure Nothing
          Just c  -> pure (Just (Chunk c))

  let headers =
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        ]

  pure (Response status200 headers (BodyStream pull))


-- | Build a streaming 'Response Body' synchronously.
--
-- Runs the handler to completion, collects all events, then serves
-- them from an in-memory list. Useful for simple cases where all
-- events are known upfront and no concurrent streaming is needed.
sseResponseSync :: Aeson.ToJSON a => ServerStreamHandler a -> IO (Response Body)
sseResponseSync handler = do
  ref <- newIORef ([] :: [ByteString])

  let emit ev = modifyIORef' ref (++ [sseChunk ev])
  handler emit

  chunks <- readIORef ref
  chunksRef <- newIORef chunks

  let pull = do
        cs <- readIORef chunksRef
        case cs of
          []     -> pure Nothing
          (c:rest) -> do
            writeIORef chunksRef rest
            pure (Just (Chunk c))

  let headers =
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        ]

  pure (Response status200 headers (BodyStream pull))
