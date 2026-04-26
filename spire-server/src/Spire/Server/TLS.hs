-- | TLS support for spire-server.
--
-- Accepts a pre-configured @TLS.ServerParams@ so users have full
-- control over cipher suites, certificate loading, and TLS versions.
-- See the @tls@ package documentation for configuration options.
--
-- @
-- import qualified Network.TLS as TLS
-- import qualified Network.TLS.Extra.Cipher as TLS
-- import Spire.Server.TLS
--
-- main = do
--   params <- loadTLSParams "cert.pem" "key.pem"
--   runServerTLS 443 "0.0.0.0" params myService
-- @
module Spire.Server.TLS
  ( -- * Running with TLS
    runServerTLS
    -- * Helper for loading credentials
  , loadTLSParams
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, catch, finally, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status400, status500, parseQuery, urlDecode)
import Network.Socket
import qualified Network.TLS as TLS

import Spire.Service (Service (..))
import Http.Core
import Http.Core.Body
import Spire.Server.Parse (parseRequestHead, RequestHead (..))
import Spire.Server.Render (renderFull)


-- | Run the server with TLS on the given port.
runServerTLS
  :: Int                                    -- ^ Port
  -> HostName                               -- ^ Host
  -> TLS.ServerParams                       -- ^ TLS configuration
  -> Service IO (Request Body) (Response Body)
  -> IO ()
runServerTLS port host params svc = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 128
    let loop = do
          (conn, _) <- accept sock
          _ <- forkIO $ handleConn params svc conn
          loop
    loop


-- | Load TLS credentials and build ServerParams.
--
-- Uses strong cipher suites and TLS 1.2+. Returns the error string
-- if credential loading fails.
loadTLSParams :: FilePath -> FilePath -> IO (Either String TLS.ServerParams)
loadTLSParams certFile keyFile = do
  result <- TLS.credentialLoadX509 certFile keyFile
  pure $ case result of
    Left err   -> Left err
    Right cred -> Right $ TLS.defaultParamsServer
      { TLS.serverShared = (TLS.serverShared TLS.defaultParamsServer)
          { TLS.sharedCredentials = TLS.Credentials [cred]
          }
      }


handleConn :: TLS.ServerParams -> Service IO (Request Body) (Response Body) -> Socket -> IO ()
handleConn params svc sock = (do
  ctx <- TLS.contextNew sock params
  TLS.handshake ctx

  input <- TLS.recvData ctx
  case parseRequestHead input of
    Nothing -> do
      TLS.sendData ctx (LBS.fromStrict $ renderFull status400 [] "Bad Request")
      TLS.bye ctx
    Just (reqHead, remainder) -> do
      let bodyBytes = case lookup "content-length" (rhHeaders reqHead) of
            Just cl -> case reads (BS8.unpack cl) of
              [(n, "")] -> BS.take n remainder
              _         -> BS.empty
            Nothing -> BS.empty

      exts <- emptyExtensions
      let pathRaw = rhPath reqHead
          (pathPart, queryPart) = BS8.break (== '?') pathRaw
          path = filter (/= "") $ map (TE.decodeUtf8Lenient . urlDecode True) $ BS.split 0x2F pathPart
          query = if BS.null queryPart then [] else parseQuery (BS.drop 1 queryPart)
          req = Request
            { requestMethod     = rhMethod reqHead
            , requestPathRaw    = pathPart
            , requestPath       = path
            , requestQuery      = query
            , requestHeaders    = rhHeaders reqHead
            , requestBody       = fromBytes bodyBytes
            , requestExtensions = exts
            }

      resp <- catch
        (runService svc req)
        (\(_ :: SomeException) ->
          pure (Response status500 [("Content-Type", "text/plain")] (fromBytes "Internal Server Error")))

      respBody <- bodyToStrict (responseBody resp)
      TLS.sendData ctx (LBS.fromStrict $ renderFull (responseStatus resp) (responseHeaders resp) respBody)
      TLS.bye ctx
  ) `finally` close sock
