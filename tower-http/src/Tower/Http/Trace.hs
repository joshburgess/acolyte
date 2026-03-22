-- | Request/response tracing middleware.
--
-- Logs request method, path, response status, and elapsed time via
-- a user-provided callback. This avoids a hard dependency on any
-- specific logging framework.
--
-- @
-- svc |> traceLayer (\\entry -> putStrLn (show entry))
-- @
module Tower.Http.Trace
  ( -- * Trace entry
    TraceEntry (..)
    -- * Layer
  , traceLayer
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (Method, Status, statusCode)
import Data.IORef
import System.IO (hFlush, stdout)

import Tower (Middleware, middleware)
import Tower.Service (Service (..))
import Http.Core (Request (..), Response (..))


-- | Information recorded for each request/response pair.
data TraceEntry = TraceEntry
  { traceMethod :: !Method
  , tracePath   :: !ByteString
  , traceStatus :: !Int
  } deriving (Show, Eq)


-- | Middleware that traces each request/response.
--
-- The callback receives a 'TraceEntry' after each request completes.
-- Keep the callback fast — it runs in the request path.
traceLayer
  :: (TraceEntry -> IO ())
  -> Middleware IO (Request ByteString) (Response ByteString)
traceLayer onTrace = middleware $ \(Service inner) -> Service $ \req -> do
  resp <- inner req
  let entry = TraceEntry
        { traceMethod = requestMethod req
        , tracePath   = requestPathRaw req
        , traceStatus = statusCode (responseStatus resp)
        }
  onTrace entry
  pure resp
