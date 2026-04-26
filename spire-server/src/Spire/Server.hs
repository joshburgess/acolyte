-- | Minimal spire-native HTTP/1.1 server — no WAI dependency.
--
-- Accepts TCP connections, parses HTTP/1.1, dispatches to a spire
-- Service, and writes HTTP/1.1 responses. Supports keep-alive and
-- graceful shutdown.
--
-- @
-- import Spire.Server (runServer)
-- import Spire (Service (..))
-- import Http.Core
--
-- main :: IO ()
-- main = runServer 3000 $ Service $ \req ->
--   pure (Response status200 [("Content-Type", "text/plain")] (fromBytes "hello"))
-- @
module Spire.Server
  ( -- * Running the server (Body-based — native)
    runServer
  , runServerWithShutdown
    -- * Running the server (ByteString-based — convenient)
  , runServerBS
  , runServerConfigBS
    -- * Body adapter
  , adaptToBody
    -- * Configuration
  , ServerConfig (..)
  , defaultConfig
  , runServerConfig
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, catch, SomeException, finally)
import System.Timeout (timeout)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status400, status408, status500, statusCode, Query, parseQuery, urlDecode)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Spire.Service (Service (..))
import Http.Core
import Http.Core.Body
import Spire.Server.Parse
  ( parseRequestHead, RequestHead (..), readBody
  , readRequestHead, ParseLimits (..), defaultParseLimits
  )
import Spire.Server.Render (renderFull, sendResponse)


-- | Server configuration.
data ServerConfig = ServerConfig
  { configPort            :: !Int
  , configHost            :: !HostName
  , configMaxBodySize     :: !Int       -- ^ Max request body (bytes). Default: 2 MiB.
  , configRecvBufferSize  :: !Int       -- ^ Socket receive buffer. Default: 4096.
  , configKeepAlive       :: !Bool      -- ^ Support HTTP keep-alive. Default: True.
  , configHeaderTimeout   :: !Int       -- ^ Max microseconds to read headers. Default: 30s. (slow loris protection)
  , configIdleTimeout     :: !Int       -- ^ Max microseconds idle between keep-alive requests. Default: 60s.
  , configOnException     :: !(SomeException -> IO ())  -- ^ Exception handler. Default: ignore.
  }

-- | Access the exception handler (avoids record selector ambiguity).
onException :: ServerConfig -> SomeException -> IO ()
onException = configOnException

-- | Default server configuration for the given port, binding to @127.0.0.1@.
defaultConfig :: Int -> ServerConfig
defaultConfig port = ServerConfig
  { configPort           = port
  , configHost           = "127.0.0.1"
  , configMaxBodySize    = 2 * 1024 * 1024
  , configRecvBufferSize = 4096
  , configKeepAlive      = True
  , configHeaderTimeout  = 30 * 1000000   -- 30 seconds
  , configIdleTimeout    = 60 * 1000000   -- 60 seconds
  , configOnException    = \_ -> pure ()
  }


-- | Run the server on the given port.
--
-- For long-running production deployments, run with these RTS options:
--
-- @
-- +RTS -Fd30 --nonmoving-gc -RTS
-- @
--
-- * @-Fd30@ returns unused memory to the OS every 30 seconds
--   (prevents resident memory from staying high after traffic spikes)
-- * @--nonmoving-gc@ uses the non-moving garbage collector for lower
--   GC pause latency (important for request-serving processes)
runServer :: Int -> Service IO (Request Body) (Response Body) -> IO ()
runServer port svc = runServerConfig (defaultConfig port) svc


-- | Run with a shutdown signal (MVar becomes full to trigger shutdown).
runServerWithShutdown
  :: ServerConfig
  -> Service IO (Request Body) (Response Body)
  -> MVar ()  -- ^ Put () to trigger shutdown
  -> IO ()
runServerWithShutdown config svc shutdownVar = do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) (Just (configHost config)) (Just (show (configPort config)))
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 128

    -- Accept loop
    activeConns <- newIORef (0 :: Int)
    let loop = do
          -- Check for shutdown
          shutdown <- tryReadMVar shutdownVar
          case shutdown of
            Just _  -> waitForConns activeConns
            Nothing -> do
              -- Accept with timeout so we can check shutdown periodically
              result <- catch
                (Just <$> accept sock)
                (\(_ :: SomeException) -> pure Nothing)
              case result of
                Nothing -> loop
                Just (conn, _addr) -> do
                  atomicModifyIORef' activeConns (\n -> (n + 1, ()))
                  _ <- forkIO $ handleConnection config svc conn
                    `finally` atomicModifyIORef' activeConns (\n -> (n - 1, ()))
                  loop
    loop


-- | Run with default shutdown behavior (runs forever until killed).
runServerConfig :: ServerConfig -> Service IO (Request Body) (Response Body) -> IO ()
runServerConfig config svc = do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) (Just (configHost config)) (Just (show (configPort config)))
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 128
    acceptLoop config svc sock


-- | Accept connections forever.
acceptLoop :: ServerConfig -> Service IO (Request Body) (Response Body) -> Socket -> IO ()
acceptLoop config svc sock = do
  (conn, _addr) <- accept sock
  _ <- forkIO $ handleConnection config svc conn
  acceptLoop config svc sock


-- | Handle a single connection (possibly multiple requests via keep-alive).
--
-- Known limitation: HTTP pipelining (multiple requests sent before any
-- response is read) is not supported. If the client pipelines requests,
-- leftover bytes from the body read (belonging to the next pipelined
-- request) are discarded. In practice this is not an issue: browsers
-- do not pipeline HTTP/1.1 requests, and HTTP/2 supersedes pipelining.
-- The keep-alive loop only handles sequential request-response pairs.
handleConnection
  :: ServerConfig
  -> Service IO (Request Body) (Response Body)
  -> Socket
  -> IO ()
handleConnection config svc conn = go `finally` close conn
  where
    limits = ParseLimits
      { maxHeaderSize = 8 * 1024
      , maxBodySize   = configMaxBodySize config
      , recvBufSize   = configRecvBufferSize config
      }

    go = do
      -- Read headers with timeout (slow loris protection)
      mResult <- timeout (configHeaderTimeout config) (readRequestHead conn limits)
      case mResult of
        Nothing -> do
          -- Header read timed out
          sendAll conn (renderFull status408 [] "Request Timeout")
        Just (Left errMsg) -> do
          -- Parse error
          sendAll conn (renderFull status400 [] errMsg)
        Just (Right (reqHead, remainder)) -> do
          -- Read body with limits
          bodyResult <- readBody conn limits (rhHeaders reqHead) remainder
          case bodyResult of
            Left errMsg -> do
              sendAll conn (renderFull status400 [] errMsg)
            Right body -> do
              -- Build our Request
              exts <- emptyExtensions
              let pathRaw = rhPath reqHead
                  (pathPart, queryPart) = BS8.break (== '?') pathRaw
                  path = filter (/= "") $ map (TE.decodeUtf8Lenient . urlDecode True) $ BS.split 0x2F pathPart
                  query = parseQueryString (BS.drop 1 queryPart)
                  req = Request
                    { requestMethod     = rhMethod reqHead
                    , requestPathRaw    = pathPart
                    , requestPath       = path
                    , requestQuery      = query
                    , requestHeaders    = rhHeaders reqHead
                    , requestBody       = fromBytes body
                    , requestExtensions = exts
                    }

              -- Dispatch to spire Service (catch handler exceptions)
              resp <- catch
                (runService svc req)
                (\(e :: SomeException) -> do
                  onException config e
                  pure (Response status500
                    [("Content-Type", "text/plain")]
                    (fromBytes "Internal Server Error")))

              -- Render and send response (streaming or strict)
              sendResponse (sendAll conn)
                (responseStatus resp)
                (responseHeaders resp)
                (responseBody resp)

              -- Keep-alive: check Connection header (case-insensitive)
              let connHeader = lookup "connection" (rhHeaders reqHead)
                  isClose = case connHeader of
                    Just v  -> BS8.map toLower v == "close"
                    Nothing -> False
                  keepAlive = configKeepAlive config && not isClose
              if keepAlive
                then go
                else pure ()


-- | Parse a query string (without the leading '?').
parseQueryString :: ByteString -> Query
parseQueryString bs
  | BS.null bs = []
  | otherwise  = parseQuery bs


-- | Wait for all active connections to finish.
waitForConns :: IORef Int -> IO ()
waitForConns ref = do
  n <- readIORef ref
  if n > 0
    then threadDelay 100000 >> waitForConns ref  -- 100ms poll
    else pure ()


-- ===================================================================
-- ByteString convenience: no manual Body adapter needed
-- ===================================================================

-- | Run a ByteString-based service on spire-server.
--
-- This is the convenient entry point for services built with
-- acolyte-server (which produces @Service IO (Request
-- ByteString) (Response ByteString)@). The Body conversion is
-- handled internally — users never see the Body type.
--
-- @
-- main = runServerBS 3000 myService
-- @
runServerBS :: Int -> Service IO (Request ByteString) (Response ByteString) -> IO ()
runServerBS port svc = runServer port (adaptToBody svc)


-- | Run a ByteString-based service with full config.
runServerConfigBS :: ServerConfig -> Service IO (Request ByteString) (Response ByteString) -> IO ()
runServerConfigBS config svc = runServerConfig config (adaptToBody svc)


-- | Adapt a ByteString-based service to a Body-based service.
--
-- Converts the request body from Body to ByteString (collecting
-- streaming bodies), and wraps the ByteString response body as
-- a strict Body.
--
-- This is the bridge between acolyte-server (which works
-- with strict ByteString) and spire-server (which works with Body).
adaptToBody
  :: Service IO (Request ByteString) (Response ByteString)
  -> Service IO (Request Body) (Response Body)
adaptToBody (Service f) = Service $ \req -> do
  bodyBytes <- bodyToStrict (requestBody req)
  let bsReq = req { requestBody = bodyBytes }
  resp <- f bsReq
  pure resp { responseBody = fromBytes (responseBody resp) }


