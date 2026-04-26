-- | Streaming example: Server-Sent Events
--
-- Demonstrates SSE event formatting and the async streaming response builder.
--
-- Run:  cabal run streaming
module Main (main) where

import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)

import Acolyte.Server.Streaming
  (SSEvent (..), sseData, sseEvent, sseChunk, sseResponse, sseResponseSync)

import Http.Core (Response (..))
import Http.Core.Body (Body (..))


main :: IO ()
main = do
  putStrLn "SSE Streaming Example"
  putStrLn "====================="
  putStrLn ""

  -- Demonstrate SSE chunk encoding
  putStrLn "1. SSE-formatted events:"
  putStrLn ""

  -- Simple data-only event
  BS8.putStr (sseChunk (sseData ("Hello, world!" :: Text)))

  -- Event with a type field
  BS8.putStr (sseChunk (sseEvent "update" ("New item added" :: Text)))

  -- Event with ID and type
  BS8.putStr (sseChunk (SSEvent ("Numbered event" :: Text) (Just "42") (Just "message")))

  putStrLn ""

  -- Demonstrate synchronous SSE response
  putStrLn "2. Synchronous SSE response (sseResponseSync):"
  resp <- sseResponseSync $ \emit -> do
    emit (sseData ("event-1" :: Text))
    emit (sseData ("event-2" :: Text))
    emit (sseEvent "done" ("finished" :: Text))
  putStrLn ("   Status: 200, Content-Type: text/event-stream")
  putStrLn ("   Body type: " ++ showBodyType (responseBody resp))
  putStrLn ""

  -- Demonstrate async SSE response
  putStrLn "3. Async SSE response (sseResponse):"
  putStrLn "   The handler runs in a forked thread."
  putStrLn "   Events are delivered to the stream as they are produced."
  asyncResp <- sseResponse $ \emit -> do
    emit (sseData ("async-event-1" :: Text))
    emit (sseData ("async-event-2" :: Text))
  putStrLn ("   Status: 200, Body type: " ++ showBodyType (responseBody asyncResp))
  putStrLn ""

  putStrLn "Done."

showBodyType :: Body -> String
showBodyType (BodyStream _)  = "BodyStream (chunked)"
showBodyType (BodyStrict _)  = "BodyStrict"
showBodyType (BodyFile _ _)  = "BodyFile"
showBodyType BodyEmpty       = "BodyEmpty"
