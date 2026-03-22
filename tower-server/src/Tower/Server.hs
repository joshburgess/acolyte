-- | Minimal tower-native HTTP/1.1 server — no WAI dependency.
--
-- Accepts TCP connections, parses HTTP/1.1, dispatches to a tower
-- Service, and writes HTTP/1.1 responses. Supports keep-alive and
-- graceful shutdown.
--
-- @
-- import Tower.Server (runServer)
-- import Tower (Service (..))
-- import Http.Core
--
-- main :: IO ()
-- main = runServer 3000 $ Service $ \req ->
--   pure (Response status200 [("Content-Type", "text/plain")] (fromBytes "hello"))
-- @
module Tower.Server
  ( -- * Running the server
    runServer
  , runServerWithShutdown
    -- * Configuration
  , ServerConfig (..)
  , defaultConfig
  , runServerConfig
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, catch, SomeException, finally)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status400, status500, statusCode)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Tower.Service (Service (..))
import Http.Core
import Http.Core.Body
import Tower.Server.Parse
import Tower.Server.Render


-- | Server configuration.
data ServerConfig = ServerConfig
  { configPort            :: !Int
  , configHost            :: !HostName
  , configMaxBodySize     :: !Int       -- ^ Max request body (bytes). Default: 2 MiB.
  , configRecvBufferSize  :: !Int       -- ^ Socket receive buffer. Default: 4096.
  , configKeepAlive       :: !Bool      -- ^ Support HTTP keep-alive. Default: True.
  }

defaultConfig :: Int -> ServerConfig
defaultConfig port = ServerConfig
  { configPort           = port
  , configHost           = "127.0.0.1"
  , configMaxBodySize    = 2 * 1024 * 1024
  , configRecvBufferSize = 4096
  , configKeepAlive      = True
  }


-- | Run the server on the given port.
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
handleConnection
  :: ServerConfig
  -> Service IO (Request Body) (Response Body)
  -> Socket
  -> IO ()
handleConnection config svc conn = go BS.empty `finally` close conn
  where
    go leftover = do
      -- Read enough to parse headers
      input <- if BS.null leftover
        then recv conn (configRecvBufferSize config)
        else pure leftover

      if BS.null input
        then pure ()  -- Connection closed
        else case parseRequestHead input of
          Nothing -> do
            -- Bad request
            let resp = renderFull status400 [] "Bad Request"
            sendAll conn resp
          Just (reqHead, remainder) -> do
            -- Read body
            body <- readBody conn (rhHeaders reqHead) remainder

            -- Build our Request
            exts <- emptyExtensions
            let pathRaw = rhPath reqHead
                path = filter (/= "") $ T.splitOn "/" (TE.decodeUtf8 pathRaw)
                req = Request
                  { requestMethod     = rhMethod reqHead
                  , requestPathRaw    = pathRaw
                  , requestPath       = path
                  , requestQuery      = []  -- TODO: parse query string
                  , requestHeaders    = rhHeaders reqHead
                  , requestBody       = fromBytes body
                  , requestExtensions = exts
                  }

            -- Dispatch to tower Service
            resp <- catch
              (runService svc req)
              (\(e :: SomeException) ->
                pure (Response status500
                  [("Content-Type", "text/plain")]
                  (fromBytes "Internal Server Error")))

            -- Render and send response
            respBytes <- renderResponse
              (responseStatus resp)
              (responseHeaders resp)
              (responseBody resp)
            sendAll conn respBytes

            -- Keep-alive: check Connection header
            let connHeader = lookup "connection" (rhHeaders reqHead)
                keepAlive = configKeepAlive config
                  && connHeader /= Just "close"
            if keepAlive
              then go BS.empty  -- handle next request on same connection
              else pure ()


-- | Wait for all active connections to finish.
waitForConns :: IORef Int -> IO ()
waitForConns ref = do
  n <- readIORef ref
  if n > 0
    then threadDelay 100000 >> waitForConns ref  -- 100ms poll
    else pure ()


