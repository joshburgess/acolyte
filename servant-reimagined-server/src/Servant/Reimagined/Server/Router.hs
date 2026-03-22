-- | Router and Server: dispatch requests to handlers, produce tower Service.
module Servant.Reimagined.Server.Router
  ( -- * Router
    Router
  , emptyRouter
  , addRoute
  , dispatch
    -- * Server construction
  , serve
  , serveWithState
    -- * Capture parsing
  , ParseCapture (..)
  , CaptureList (..)
  , injectCaptures
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Typeable (Typeable)
import Network.HTTP.Types (status404, status405)

import Tower.Service (Service (..))
import Http.Core
  ( Request (..), Response (..)
  , RequestParts (..), splitRequest
  , Extensions, insertExtension
  )
import Servant.Reimagined.Server.Handler
import Servant.Reimagined.Server.Extract (AppState (..), PathCapture (..))
import Servant.Reimagined.Server.Response (IntoResponse (..))


-- | A collection of routes.
newtype Router = Router [BoundHandler]

emptyRouter :: Router
emptyRouter = Router []

addRoute :: BoundHandler -> Router -> Router
addRoute bh (Router entries) = Router (entries ++ [bh])


-- | Wrapper for captured text segments stored in Extensions.
newtype CaptureList = CaptureList [Text]
  deriving (Typeable)


-- | Inject captured text segments into extensions.
injectCaptures :: [Text] -> Extensions -> IO ()
injectCaptures caps exts = insertExtension (CaptureList caps) exts


-- | Dispatch a request through the router.
dispatch :: Router -> Request ByteString -> IO (Response ByteString)
dispatch (Router entries) req = do
  let segments = requestPath req
      method   = requestMethod req
      (parts, body) = splitRequest req
  go entries segments method parts body False
  where
    go [] _segs _method _parts _body methodMatched
      | methodMatched = pure (Response status405 [] "Method Not Allowed")
      | otherwise     = pure (Response status404 [] "Not Found")

    go (bh : rest) segs method parts body methodMatched =
      case bhMatchFn bh segs of
        Just (MatchResult caps) ->
          if bhMethod bh == method
          then do
            injectCaptures caps (rpExtensions parts)
            bhHandler bh parts body
          else
            go rest segs method parts body True
        Nothing ->
          go rest segs method parts body methodMatched


-- | Parse a text capture into a typed value.
class ParseCapture a where
  parseCapture :: Text -> Maybe a

instance ParseCapture Int where
  parseCapture t = case T.decimal t of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

instance ParseCapture Text where
  parseCapture = Just

instance ParseCapture String where
  parseCapture = Just . T.unpack


-- | Build a tower Service from a router.
serve :: Router -> Service IO (Request ByteString) (Response ByteString)
serve router = Service (dispatch router)


-- | Build a service with shared state injected into every request.
serveWithState
  :: Typeable s
  => s -> Router -> Service IO (Request ByteString) (Response ByteString)
serveWithState state router = Service $ \req -> do
  insertExtension (AppState state) (requestExtensions req)
  dispatch router req
