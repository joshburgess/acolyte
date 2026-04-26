-- | HTTP/1.1 request parsing.
--
-- Handles: Content-Length bodies, chunked transfer-encoding,
-- incremental header reading (headers may arrive across multiple
-- recv() calls), header size limits, and malformed request rejection.
module Spire.Server.Parse
  ( -- * Parsing
    parseRequestHead
  , RequestHead (..)
  , readBody
    -- * Incremental reading
  , readRequestHead
    -- * Limits
  , ParseLimits (..)
  , defaultParseLimits
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Word (Word8)
import Network.HTTP.Types (RequestHeaders, Header, HeaderName)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Numeric (readHex)


-- | Parsed request head (method, path, version, headers).
data RequestHead = RequestHead
  { rhMethod  :: !ByteString
  , rhPath    :: !ByteString
  , rhVersion :: !ByteString
  , rhHeaders :: !RequestHeaders
  } deriving (Show)


-- | Limits for parsing.
data ParseLimits = ParseLimits
  { maxHeaderSize :: !Int    -- ^ Max total header size in bytes. Default: 8 KiB.
  , maxBodySize   :: !Int    -- ^ Max request body size. Default: 2 MiB.
  , recvBufSize   :: !Int    -- ^ Socket receive buffer size. Default: 4096.
  } deriving (Show)

-- | Default parse limits: 8 KiB headers, 2 MiB body, 4096-byte recv buffer.
defaultParseLimits :: ParseLimits
defaultParseLimits = ParseLimits
  { maxHeaderSize = 8 * 1024
  , maxBodySize   = 2 * 1024 * 1024
  , recvBufSize   = 4096
  }


-- | Parse the request line and headers from a buffer.
--
-- Returns the parsed head and any leftover bytes (start of body).
-- Returns Nothing if the input is malformed.
parseRequestHead :: ByteString -> Maybe (RequestHead, ByteString)
parseRequestHead input = do
  idx <- findSubstring "\r\n\r\n" input
  let headerSection = BS.take idx input
      remainder = BS.drop (idx + 4) input

  let ls = BS8.lines headerSection
  case ls of
    [] -> Nothing
    (reqLine : headerLines) -> do
      (method, path, ver) <- parseRequestLine reqLine
      let headers = map parseHeaderLine (filter (not . BS.null) headerLines)
      Just (RequestHead method path ver headers, remainder)


-- | Read the request head incrementally from a socket.
--
-- Keeps reading until the header terminator (\r\n\r\n) is found
-- or the header size limit is exceeded.
--
-- Returns Left error message on failure, Right (head, leftover) on success.
readRequestHead :: Socket -> ParseLimits -> IO (Either ByteString (RequestHead, ByteString))
readRequestHead sock limits = go BS.empty
  where
    go buf
      | BS.length buf > maxHeaderSize limits =
          pure (Left "431 Request Header Fields Too Large")
      | otherwise =
          case findSubstring "\r\n\r\n" buf of
            Just _ -> case parseRequestHead buf of
              Just result -> pure (Right result)
              Nothing     -> pure (Left "400 Bad Request: malformed request line or headers")
            Nothing -> do
              chunk <- recv sock (recvBufSize limits)
              if BS.null chunk
                then pure (Left "400 Bad Request: connection closed before headers complete")
                else go (buf <> chunk)


-- | Parse "GET /path HTTP/1.1\r"
parseRequestLine :: ByteString -> Maybe (ByteString, ByteString, ByteString)
parseRequestLine line = case BS8.words (stripCR line) of
  [method, path, version] -> Just (method, path, version)
  _ -> Nothing


-- | Parse "Header-Name: value\r"
parseHeaderLine :: ByteString -> Header
parseHeaderLine line =
  let cleaned = stripCR line
  in case BS8.break (== ':') cleaned of
    (name, rest)
      | BS.null rest -> (CI.mk name, "")
      | otherwise    -> (CI.mk name, BS8.dropWhile (== ' ') (BS.drop 1 rest))


-- | Read the request body based on Content-Length or Transfer-Encoding.
readBody :: Socket -> ParseLimits -> RequestHeaders -> ByteString -> IO (Either ByteString ByteString)
readBody sock limits headers leftover
  -- Chunked transfer encoding
  | isChunked headers = readChunkedBody sock limits leftover
  -- Content-Length
  | Just clBS <- lookup "content-length" headers =
      case reads (BS8.unpack clBS) of
        [(cl, "")]
          | cl > maxBodySize limits -> pure (Left "413 Payload Too Large")
          | cl == 0                 -> pure (Right BS.empty)
          | otherwise               -> readExactly sock cl leftover
        _ -> pure (Left "400 Bad Request: invalid Content-Length")
  -- No body
  | otherwise = pure (Right BS.empty)


-- | Check if the request uses chunked transfer encoding.
isChunked :: RequestHeaders -> Bool
isChunked headers =
  case lookup "transfer-encoding" headers of
    Just te -> BS8.isInfixOf "chunked" te
    Nothing -> False


-- | Read a chunked transfer-encoded body.
--
-- Chunk format: {hex-size}\r\n{data}\r\n
-- Final chunk:  0\r\n\r\n
readChunkedBody :: Socket -> ParseLimits -> ByteString -> IO (Either ByteString ByteString)
readChunkedBody sock limits = go BS.empty
  where
    go acc buf
      | BS.length acc > maxBodySize limits = pure (Left "413 Payload Too Large")
      | otherwise =
          case findSubstring "\r\n" buf of
            Nothing -> do
              chunk <- recv sock (recvBufSize limits)
              if BS.null chunk
                then pure (Left "400 Bad Request: incomplete chunked body")
                else go acc (buf <> chunk)
            Just idx -> do
              let sizeLine = BS.take idx buf
                  rest = BS.drop (idx + 2) buf
              case readHex (BS8.unpack sizeLine) of
                [(0, "")] -> pure (Right acc)  -- final chunk
                [(n, "")] | n > maxBodySize limits - BS.length acc ->
                  pure (Left "413 Payload Too Large")
                [(n, "")] -> do
                  -- Read n bytes of chunk data + trailing \r\n
                  let needed = n + 2  -- data + \r\n
                  ensureResult <- ensureBytes sock (recvBufSize limits) needed rest
                  case ensureResult of
                    Left err -> pure (Left err)
                    Right fullChunk -> do
                      let chunkData = BS.take n fullChunk
                          remaining = BS.drop (n + 2) fullChunk
                      go (acc <> chunkData) remaining
                _ -> pure (Left "400 Bad Request: invalid chunk size")


-- | Ensure we have at least n bytes, reading from socket if needed.
-- Returns Left if the connection closed before enough bytes arrived.
ensureBytes :: Socket -> Int -> Int -> ByteString -> IO (Either ByteString ByteString)
ensureBytes sock bufSize needed buf
  | BS.length buf >= needed = pure (Right buf)
  | otherwise = do
      chunk <- recv sock bufSize
      if BS.null chunk
        then pure (Left "400 Bad Request: connection closed before chunk data complete")
        else ensureBytes sock bufSize needed (buf <> chunk)


-- | Read exactly n bytes from leftover + socket.
-- Returns Left if the connection closed before enough bytes arrived.
readExactly :: Socket -> Int -> ByteString -> IO (Either ByteString ByteString)
readExactly sock n leftover
  | BS.length leftover >= n = pure (Right (BS.take n leftover))
  | otherwise = do
      let needed = n - BS.length leftover
      more <- recvAll sock needed
      let result = leftover <> BS.take needed more
      if BS.length result < n
        then pure (Left "400 Bad Request: connection closed before body complete")
        else pure (Right result)


-- | Receive at least n bytes from a socket.
recvAll :: Socket -> Int -> IO ByteString
recvAll sock n = go BS.empty
  where
    go acc
      | BS.length acc >= n = pure acc
      | otherwise = do
          chunk <- recv sock 4096
          if BS.null chunk
            then pure acc
            else go (acc <> chunk)


-- ===================================================================
-- Helpers
-- ===================================================================

stripCR :: ByteString -> ByteString
stripCR bs
  | BS.null bs = bs
  | BS.last bs == 13 = BS.init bs
  | otherwise = bs

-- | Find the index of a substring in a ByteString.
findSubstring :: ByteString -> ByteString -> Maybe Int
findSubstring needle haystack = go 0
  where
    nLen = BS.length needle
    hLen = BS.length haystack
    go i
      | i + nLen > hLen = Nothing
      | BS.take nLen (BS.drop i haystack) == needle = Just i
      | otherwise = go (i + 1)
