-- | @servant-reimagined-client@ — type-safe HTTP client.
--
-- Derives client calls from the same API type that drives the server.
--
-- @
-- client <- mkClient "http://localhost:3000"
-- result <- callEndpoint @(Get UsersPath (Json [User])) client ()
-- @
module Servant.Reimagined.Client
  ( -- * Client
    Client (..)
  , mkClient
  , mkClientWith
    -- * Calling endpoints
  , callEndpoint
  , EndpointRequest (..)
    -- * Errors
  , ClientError (..)
    -- * Interceptors
  , Interceptor (..)
  , noInterceptor
    -- * Retry policies
  , RetryPolicy (..)
  , defaultRetryPolicy
  , noRetry
  , exponentialBackoff
  , withRetry
    -- * Cookie management
  , CookieJar
  , newCookieJar
  , cookieInterceptor
    -- * Named endpoint calls
  , callNamed
    -- * Path building
  , BuildPath (..)
  , ShowCapture (..)
  , buildUrl
  ) where

import Servant.Reimagined.Client.Core
import Servant.Reimagined.Client.Call
import Servant.Reimagined.Client.Named
import Servant.Reimagined.Client.Retry
import Servant.Reimagined.Client.Cookies
