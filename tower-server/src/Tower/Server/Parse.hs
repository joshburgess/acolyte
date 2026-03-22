-- | HTTP/1.1 request parsing.
--
-- Minimal parser for HTTP/1.1 requests: request line + headers.
-- Body is read separately based on Content-Length or chunked encoding.
module Tower.Server.Parse
  ( -- * Parsing
    parseRequestHead
  , RequestHead (..)
  , readBody
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Network.HTTP.Types (RequestHeaders, Header, HeaderName)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)


-- | Parsed request head (method, path, headers).
data RequestHead = RequestHead
  { rhMethod  :: !ByteString
  , rhPath    :: !ByteString
  , rhHeaders :: !RequestHeaders
  } deriving (Show)


-- | Parse the request line and headers from a buffer.
--
-- Returns the parsed head and any leftover bytes (start of body).
parseRequestHead :: ByteString -> Maybe (RequestHead, ByteString)
parseRequestHead input = do
  -- Find the end of headers (blank line)
  let headerEnd = BS8.pack "\r\n\r\n"
  idx <- findSubstring headerEnd input
  let headerSection = BS.take idx input
      remainder = BS.drop (idx + 4) input

  -- Parse request line
  let ls = BS8.lines headerSection
  case ls of
    [] -> Nothing
    (reqLine : headerLines) -> do
      (method, path) <- parseRequestLine reqLine
      let headers = map parseHeaderLine (filter (not . BS.null) headerLines)
      Just (RequestHead method path headers, remainder)


-- | Parse "GET /path HTTP/1.1\r"
parseRequestLine :: ByteString -> Maybe (ByteString, ByteString)
parseRequestLine line = case BS8.words (stripCR line) of
  [method, path, _version] -> Just (method, path)
  _ -> Nothing


-- | Parse "Header-Name: value\r"
parseHeaderLine :: ByteString -> Header
parseHeaderLine line =
  let cleaned = stripCR line
  in case BS8.break (== ':') cleaned of
    (name, rest)
      | BS.null rest -> (CI.mk name, "")
      | otherwise    -> (CI.mk name, BS8.dropWhile (== ' ') (BS.drop 1 rest))


-- | Read the request body based on Content-Length header.
-- Returns the body bytes, consuming from the socket as needed.
readBody :: Socket -> RequestHeaders -> ByteString -> IO ByteString
readBody sock headers leftover =
  case lookup "content-length" headers of
    Just clBS -> case reads (BS8.unpack clBS) of
      [(cl, "")] -> readExactly sock cl leftover
      _          -> pure BS.empty
    Nothing -> pure BS.empty  -- No body (GET, DELETE, etc.)


-- | Read exactly n bytes from leftover + socket.
readExactly :: Socket -> Int -> ByteString -> IO ByteString
readExactly sock n leftover
  | BS.length leftover >= n = pure (BS.take n leftover)
  | otherwise = do
      let needed = n - BS.length leftover
      more <- recvAll sock needed
      pure (leftover <> BS.take needed more)


-- | Receive at least n bytes from a socket.
recvAll :: Socket -> Int -> IO ByteString
recvAll sock n = go BS.empty
  where
    go acc
      | BS.length acc >= n = pure acc
      | otherwise = do
          chunk <- recv sock 4096
          if BS.null chunk
            then pure acc  -- connection closed
            else go (acc <> chunk)


-- ===================================================================
-- Helpers
-- ===================================================================

stripCR :: ByteString -> ByteString
stripCR bs
  | BS.null bs = bs
  | BS.last bs == 13 = BS.init bs  -- 13 = '\r'
  | otherwise = bs

toLowerW8 :: Word8 -> Word8
toLowerW8 w
  | w >= 65 && w <= 90 = w + 32  -- A-Z -> a-z
  | otherwise = w

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
