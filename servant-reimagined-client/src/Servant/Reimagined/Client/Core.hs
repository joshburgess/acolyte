-- | Client core: types and configuration.
module Servant.Reimagined.Client.Core
  ( -- * Client type
    Client (..)
  , mkClient
  , mkClientWith
    -- * Client errors
  , ClientError (..)
    -- * Interceptors
  , Interceptor (..)
  , noInterceptor
  ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS


-- | A configured HTTP client targeting a specific base URL.
data Client = Client
  { clientBaseUrl     :: !Text
  , clientManager     :: !HC.Manager
  , clientInterceptor :: !Interceptor
  }


-- | Create a client with a default TLS-enabled manager.
mkClient :: Text -> IO Client
mkClient baseUrl = do
  mgr <- HC.newManager TLS.tlsManagerSettings
  pure Client
    { clientBaseUrl     = T.dropWhileEnd (== '/') baseUrl
    , clientManager     = mgr
    , clientInterceptor = noInterceptor
    }


-- | Create a client with a custom manager and interceptor.
mkClientWith :: Text -> HC.Manager -> Interceptor -> Client
mkClientWith baseUrl mgr interceptor = Client
  { clientBaseUrl     = T.dropWhileEnd (== '/') baseUrl
  , clientManager     = mgr
  , clientInterceptor = interceptor
  }


-- | Errors that can occur during a client call.
data ClientError
  = HttpError !HC.HttpException
  | DecodeError !Text
  | StatusError !Int !ByteString
  deriving (Show)

instance Exception ClientError


-- | An interceptor modifies requests before sending and responses after receiving.
data Interceptor = Interceptor
  { interceptRequest  :: HC.Request -> IO HC.Request
  , interceptResponse :: HC.Response ByteString -> IO (HC.Response ByteString)
  }


-- | No-op interceptor.
noInterceptor :: Interceptor
noInterceptor = Interceptor pure pure
