-- | Request ID middleware.
--
-- Generates a unique request ID for each request (or preserves an
-- existing one from the @X-Request-Id@ header). The ID is:
--
-- 1. Stored in request 'Extensions' (accessible to handlers)
-- 2. Copied to the response @X-Request-Id@ header
--
-- @
-- svc |> requestIdLayer
-- @
module Tower.Http.RequestId
  ( -- * Request ID type
    RequestId (..)
    -- * Layer
  , requestIdLayer
    -- * Header name
  , requestIdHeader
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.List (lookup)
import Network.HTTP.Types (HeaderName)

import Tower (Middleware, middleware)
import Tower.Service (Service (..))
import Http.Core
  ( Request (..), Response (..)
  , Extensions, insertExtension
  )


-- | A request identifier, either propagated from the client or generated.
newtype RequestId = RequestId { unRequestId :: ByteString }
  deriving (Eq, Ord, Show)


-- | The header name used for request IDs.
requestIdHeader :: HeaderName
requestIdHeader = "X-Request-Id"


-- | Simple counter-based ID generator (sufficient for single-process).
-- In production you'd use UUID v4 or similar.
newtype IdGen = IdGen (IORef Int)

newIdGen :: IO IdGen
newIdGen = IdGen <$> newIORef 0

nextId :: IdGen -> IO ByteString
nextId (IdGen ref) = do
  n <- atomicModifyIORef' ref (\n -> (n + 1, n))
  pure $ BS.pack $ "req-" ++ show n


-- | Middleware that assigns and propagates request IDs.
requestIdLayer :: IO (Middleware IO (Request ByteString) (Response ByteString))
requestIdLayer = do
  gen <- newIdGen
  pure $ middleware $ \(Service inner) -> Service $ \req -> do
    -- Use existing header or generate new
    let existing = lookup requestIdHeader (requestHeaders req)
    rid <- case existing of
      Just val -> pure val
      Nothing  -> nextId gen

    -- Store in extensions for handler access
    let reqId = RequestId rid
    insertExtension reqId (requestExtensions req)

    -- Call inner service
    resp <- inner req

    -- Add to response headers
    pure resp
      { responseHeaders = (requestIdHeader, rid) : responseHeaders resp }
