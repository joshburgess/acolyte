-- | Retry policies for HTTP client calls.
--
-- Wraps IO actions with configurable retry logic including
-- exponential backoff and status-code-based retry decisions.
module Servant.Reimagined.Client.Retry
  ( RetryPolicy (..)
  , defaultRetryPolicy
  , noRetry
  , exponentialBackoff
  , withRetry
  ) where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (Status, statusCode)


-- | Configuration for retrying failed HTTP requests.
data RetryPolicy = RetryPolicy
  { rpMaxRetries  :: !Int            -- ^ Maximum number of retries (default: 3)
  , rpBaseDelay   :: !Int            -- ^ Base delay in microseconds (default: 100000 = 100ms)
  , rpMaxDelay    :: !Int            -- ^ Maximum delay cap in microseconds (default: 5000000 = 5s)
  , rpShouldRetry :: Status -> Bool  -- ^ Which status codes to retry
  }


-- | Sensible defaults: 3 retries, 100ms base, 5s cap, retry on 502/503/504.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy
  { rpMaxRetries  = 3
  , rpBaseDelay   = 100000
  , rpMaxDelay    = 5000000
  , rpShouldRetry = \s -> statusCode s `elem` [502, 503, 504]
  }


-- | Never retry.
noRetry :: RetryPolicy
noRetry = RetryPolicy
  { rpMaxRetries  = 0
  , rpBaseDelay   = 0
  , rpMaxDelay    = 0
  , rpShouldRetry = const False
  }


-- | Exponential backoff with the given max retries and base delay (microseconds).
-- Retries on 502, 503, 504 by default.
exponentialBackoff :: Int -> Int -> RetryPolicy
exponentialBackoff maxRetries baseDelayUs = RetryPolicy
  { rpMaxRetries  = maxRetries
  , rpBaseDelay   = baseDelayUs
  , rpMaxDelay    = baseDelayUs * (2 ^ maxRetries)
  , rpShouldRetry = \s -> statusCode s `elem` [502, 503, 504]
  }


-- | Wrap an IO action with retry logic.
--
-- On each attempt, if the response status matches the retry predicate
-- and we haven't exhausted retries, wait with exponential backoff then retry.
-- The final attempt (or a non-retryable response) is returned as-is.
withRetry :: RetryPolicy -> IO (HC.Response ByteString) -> IO (HC.Response ByteString)
withRetry policy action = go 0
  where
    go attempt
      | attempt >= rpMaxRetries policy = action
      | otherwise = do
          resp <- action
          if rpShouldRetry policy (HC.responseStatus resp) && attempt < rpMaxRetries policy
            then do
              let delay = min (rpMaxDelay policy) (rpBaseDelay policy * (2 ^ attempt))
              threadDelay delay
              go (attempt + 1)
            else pure resp
