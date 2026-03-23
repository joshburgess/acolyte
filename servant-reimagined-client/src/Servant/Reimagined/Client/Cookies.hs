-- | Cookie management via interceptors.
--
-- Provides a mutable cookie jar that automatically adds Cookie headers
-- to outgoing requests and stores Set-Cookie values from responses.
module Servant.Reimagined.Client.Cookies
  ( CookieJar
  , newCookieJar
  , cookieInterceptor
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word8)
import qualified Network.HTTP.Client as HC

import Servant.Reimagined.Client.Core (Interceptor (..))


-- | A simple cookie jar backed by an IORef.
-- Stores cookies as (name, value) pairs.
newtype CookieJar = CookieJar (IORef [(ByteString, ByteString)])


-- | Create an empty cookie jar.
newCookieJar :: IO CookieJar
newCookieJar = CookieJar <$> newIORef []


-- | An interceptor that manages cookies automatically.
--
-- On each request, adds a @Cookie@ header with all stored cookies.
-- On each response, parses @Set-Cookie@ headers and stores the values.
cookieInterceptor :: CookieJar -> Interceptor
cookieInterceptor (CookieJar ref) = Interceptor
  { interceptRequest = \req -> do
      cookies <- readIORef ref
      let cookieHeader = BS.intercalate "; " [k <> "=" <> v | (k, v) <- cookies]
      pure $ if null cookies
        then req
        else req { HC.requestHeaders = ("Cookie", cookieHeader) : HC.requestHeaders req }
  , interceptResponse = \resp -> do
      let setCookies = [v | ("Set-Cookie", v) <- HC.responseHeaders resp]
      mapM_ (storeCookie ref) setCookies
      pure resp
  }


-- | Parse a Set-Cookie header value and store the name=value pair.
storeCookie :: IORef [(ByteString, ByteString)] -> ByteString -> IO ()
storeCookie ref sc = do
  let -- Split on ';' to get just the name=value part (ignore attributes)
      (nameVal, _) = BS.break (== semicolon) sc
      (name, rest) = BS.break (== equals) nameVal
      val = BS.drop 1 rest  -- drop the '='
  modifyIORef' ref ((BS.copy name, BS.copy val) :)
  where
    semicolon, equals :: Word8
    semicolon = 0x3B
    equals    = 0x3D
