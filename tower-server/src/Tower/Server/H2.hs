{-# LANGUAGE OverloadedStrings #-}
-- | HTTP/2 server support via the @http2@ package.
--
-- Bridges the @http2@ library's callback-based server API to tower's
-- @Service IO (Request Body) (Response Body)@ abstraction. The same
-- tower Service that handles HTTP/1.1 requests handles HTTP/2 — no
-- code changes needed.
--
-- @
-- import Tower.Server.H2 (runServerH2, defaultH2Config)
--
-- main = runServerH2 (defaultH2Config 3000) myService
-- @
module Tower.Server.H2
  ( -- * Configuration
    H2Config (..)
  , defaultH2Config
    -- * Running
  , runServerH2
    -- * Bridge (for custom setups)
  , toH2Server
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, catch, SomeException, finally)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status500, parseQuery, urlDecode)
import Network.Socket

import qualified Network.HTTP2.Server as H2
import Network.HTTP.Semantics (Token(..))

import Tower.Service (Service (..))
import Http.Core
import Http.Core.Body


-- | HTTP/2 server configuration.
data H2Config = H2Config
  { h2Port         :: !Int
  , h2Host         :: !Text
  , h2Workers      :: !Int
  , h2MaxStreams    :: !Int
  } deriving (Show)

-- | Sensible defaults for HTTP/2.
defaultH2Config :: Int -> H2Config
defaultH2Config port = H2Config
  { h2Port      = port
  , h2Host      = "127.0.0.1"
  , h2Workers   = 4
  , h2MaxStreams = 100
  }


-- | Run an HTTP/2 server (h2c — cleartext, no TLS).
--
-- Listens on the configured port and dispatches each HTTP/2 stream
-- to the tower Service.
runServerH2 :: H2Config -> Service IO (Request Body) (Response Body) -> IO ()
runServerH2 config svc = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints)
    (Just (T.unpack (h2Host config)))
    (Just (show (h2Port config)))
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 128
    acceptLoopH2 config svc sock


acceptLoopH2 :: H2Config -> Service IO (Request Body) (Response Body) -> Socket -> IO ()
acceptLoopH2 config svc sock = do
  (conn, _addr) <- accept sock
  _ <- forkIO $ handleH2Connection config svc conn
  acceptLoopH2 config svc sock


handleH2Connection
  :: H2Config
  -> Service IO (Request Body) (Response Body)
  -> Socket
  -> IO ()
handleH2Connection _config svc conn =
  (do h2Conf <- H2.allocSimpleConfig conn 4096
      H2.run H2.defaultServerConfig h2Conf (toH2Server svc)
      H2.freeSimpleConfig h2Conf)
    `catch` (\(_ :: SomeException) -> pure ())
    `finally` close conn


-- | Bridge a tower Service to the http2 package's Server callback.
--
-- Each HTTP/2 stream (request) is dispatched to the tower Service,
-- and the response is sent back via the http2 callback.
toH2Server :: Service IO (Request Body) (Response Body) -> H2.Server
toH2Server (Service handler) h2Req _aux respond = do
  req <- fromH2Request h2Req
  resp <- handler req
    `catch` (\(_ :: SomeException) ->
      pure (Http.Core.Response status500
        [("Content-Type", "text/plain")]
        (fromBytes "Internal Server Error")))
  respond (toH2Response resp) []


-- | Convert an http2 Request to an http-core Request.
fromH2Request :: H2.Request -> IO (Request Body)
fromH2Request h2Req = do
  exts <- emptyExtensions
  let method  = maybe "GET" id (H2.requestMethod h2Req)
      pathRaw = maybe "/" id (H2.requestPath h2Req)
      (pathPart, queryPart) = BS.break (== 0x3F) pathRaw
      pathSegs = filter (/= "") $ map (TE.decodeUtf8Lenient . urlDecode True) $ BS.split 0x2F pathPart
      query = if BS.null queryPart then [] else parseQuery (BS.drop 1 queryPart)
      -- TokenHeaderList = [(Token, FieldValue)]
      -- Token has tokenKey :: CI ByteString, isPseudo :: Bool
      (tokenList, _valueTable) = H2.requestHeaders h2Req
      -- Filter out pseudo-headers (:method, :path, :scheme, :authority)
      headers = [ (tokenKey tok, val)
                | (tok, val) <- tokenList
                , not (isPseudo tok)
                ]
  let bodyStream = streamBody $ do
        chunk <- H2.getRequestBodyChunk h2Req
        if BS.null chunk
          then pure Nothing
          else pure (Just (Chunk chunk))
  pure Http.Core.Request
    { requestMethod     = method
    , requestPathRaw    = pathPart
    , requestPath       = pathSegs
    , requestQuery      = query
    , requestHeaders    = headers
    , requestBody       = bodyStream
    , requestExtensions = exts
    }


-- | Convert an http-core Response to an http2 Response.
toH2Response :: Response Body -> H2.Response
toH2Response resp =
  let status = responseStatus resp
      hdrs   = responseHeaders resp
  in case responseBody resp of
    BodyEmpty ->
      H2.responseNoBody status hdrs
    BodyStrict bs ->
      H2.responseBuilder status hdrs (Builder.byteString bs)
    BodyStream pull ->
      H2.responseStreaming status hdrs $ \write flush -> do
        let go = do
              mChunk <- pull
              case mChunk of
                Nothing          -> flush
                Just (Chunk bs)  -> write (Builder.byteString bs) >> go
                Just ChunkFlush  -> flush >> go
        go
    BodyFile path mRange ->
      case mRange of
        Nothing ->
          H2.responseFile status hdrs (H2.FileSpec path 0 maxBound)
        Just (FileRange off len) ->
          H2.responseFile status hdrs
            (H2.FileSpec path (fromIntegral off) (fromIntegral len))
