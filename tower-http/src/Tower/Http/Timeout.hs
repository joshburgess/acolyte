-- | Request timeout middleware.
--
-- Kills the handler if it takes longer than the configured duration
-- and returns 408 Request Timeout.
module Tower.Http.Timeout
  ( timeoutLayer
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Network.HTTP.Types (status408)

import Tower (Middleware, middleware)
import Tower.Service (Service (..))
import Http.Core (Request, Response (..))


-- | Middleware that enforces a request timeout.
--
-- @
-- svc |> timeoutLayer 5000000  -- 5 second timeout
-- @
timeoutLayer :: Int  -- ^ Timeout in microseconds
             -> Middleware IO (Request ByteString) (Response ByteString)
timeoutLayer usec = middleware $ \(Service inner) -> Service $ \req -> do
  resultVar <- newEmptyMVar

  -- Run the handler in a thread
  tid <- forkIO $ do
    resp <- inner req
    putMVar resultVar (Just resp)

  -- Run the timeout in another thread
  _ <- forkIO $ do
    threadDelay usec
    putMVar resultVar Nothing

  result <- takeMVar resultVar
  case result of
    Just resp -> pure resp
    Nothing -> do
      killThread tid
      pure (Response status408 [("Content-Type", "text/plain")] "Request Timeout")
