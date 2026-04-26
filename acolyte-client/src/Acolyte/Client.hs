-- | @acolyte-client@ — type-safe HTTP client.
--
-- Derives client calls from the same API type that drives the server.
--
-- @
-- client <- mkClient "http://localhost:3000"
-- result <- callEndpoint @(Get UsersPath (Json [User])) client ()
-- @
module Acolyte.Client
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
    -- * Record-based client generation
  , mkClientRecord
  , GBuildClientRecord (..)
    -- * Path building
  , BuildPath (..)
  , ShowCapture (..)
  , buildUrl
  ) where

import Acolyte.Client.Core
import Acolyte.Client.Call
import Acolyte.Client.Named
import Acolyte.Client.Retry
import Acolyte.Client.Cookies
