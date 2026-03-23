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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Network.HTTP.Types (status404, status405)

import Tower.Service (Service (..))
import Http.Core
  ( Request (..), Response (..)
  , RequestParts (..), splitRequest
  , Extensions, insertExtension
  )
import Servant.Reimagined.Server.Handler
import Servant.Reimagined.Server.Extract
  ( AppState (..), PathCapture (..), BodyBytes (..)
  , CaptureList (..), ParseCapture (..)
  , MatchedPath (..), OriginalUri (..)
  )
import Servant.Reimagined.Server.Response (IntoResponse (..))


-- | A collection of routes, indexed by first path segment for fast dispatch.
--
-- Routes are split into two groups:
-- * 'riBySegment' — keyed by first literal path segment (O(log n) lookup)
-- * 'riWildcard' — routes whose first segment is a capture (fallback scan)
data Router = Router
  { riBySegment :: !(Map Text [BoundHandler])
  , riWildcard  :: ![BoundHandler]
  }

-- | An empty router with no routes registered.
emptyRouter :: Router
emptyRouter = Router Map.empty []

-- | Add a bound handler as a new route to the router.
addRoute :: BoundHandler -> Router -> Router
addRoute bh (Router bySegs wild) =
  case firstLiteral (bhPattern bh) of
    Just seg -> Router (Map.insertWith (++) seg [bh] bySegs) wild
    Nothing  -> Router bySegs (wild ++ [bh])

-- | Extract the first literal segment from a pattern like "/users/{capture}".
firstLiteral :: Text -> Maybe Text
firstLiteral pat =
  case filter (/= "") $ T.splitOn "/" pat of
    (seg : _) | not (T.isPrefixOf "{" seg) -> Just seg
    _ -> Nothing


-- | Inject captured text segments into extensions.
injectCaptures :: [Text] -> Extensions -> IO ()
injectCaptures caps exts = insertExtension (CaptureList caps) exts


-- | Dispatch a request through the router.
--
-- First looks up the first path segment in the index (O(log n)).
-- Falls back to wildcard routes if no indexed match.
dispatch :: Router -> Request ByteString -> IO (Response ByteString)
dispatch router req = do
  let segments = requestPath req
      method   = requestMethod req
      (parts, body) = splitRequest req
      -- Look up candidates by first segment
      candidates = case segments of
        (seg : _) -> Map.findWithDefault [] seg (riBySegment router)
                     ++ riWildcard router
        []        -> riWildcard router
  go candidates segments method parts body False
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
            insertExtension (BodyBytes body) (rpExtensions parts)
            insertExtension (MatchedPath (bhPattern bh)) (rpExtensions parts)
            insertExtension (OriginalUri (rpPathRaw parts)) (rpExtensions parts)
            bhHandler bh parts body
          else
            go rest segs method parts body True
        Nothing ->
          go rest segs method parts body methodMatched


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
