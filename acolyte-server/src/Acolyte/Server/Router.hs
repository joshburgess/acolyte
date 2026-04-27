-- | Router and Server: dispatch requests to handlers, produce spire Service.
module Acolyte.Server.Router
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
import Data.List (insertBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Network.HTTP.Types (status404, status405)

import Spire.Service (Service (..))
import Http.Core
  ( Request (..), Response (..)
  , RequestParts (..), splitRequest
  , Extensions, insertExtension
  )
import Acolyte.Server.Handler
import Acolyte.Server.Extract
  ( AppState (..), PathCapture (..), BodyBytes (..)
  , CaptureList (..), ParseCapture (..)
  , MatchedPath (..), OriginalUri (..)
  )
import Acolyte.Server.Response (IntoResponse (..))


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

-- | Whether a pattern segment is a literal or a capture placeholder.
--
-- The 'Ord' instance puts 'Literal' before 'Capture', so when patterns
-- are compared segment-by-segment a literal wins over a capture at the
-- same position. This is what makes literal paths match before
-- conflicting capture paths.
data Specificity = Literal | Capture
  deriving (Eq, Ord, Show)

-- | Add a bound handler as a new route to the router.
--
-- Within each prefix bucket the handler list is kept sorted by
-- specificity. Routes with more literal segments (and literals at
-- earlier positions) come first, so e.g. @/api/articles/feed@ is
-- always tried before @/api/articles/{slug}@ regardless of
-- declaration order.
addRoute :: BoundHandler -> Router -> Router
addRoute bh (Router bySegs wild) =
  case firstLiteral (bhPattern bh) of
    Just seg -> Router (Map.alter insertOrCreate seg bySegs) wild
    Nothing  -> Router bySegs (insertBySpec bh wild)
  where
    insertOrCreate Nothing   = Just [bh]
    insertOrCreate (Just bs) = Just (insertBySpec bh bs)

-- | Insert into a bucket sorted by pattern specificity (most specific
-- first). For routes of equal specificity the newly inserted handler
-- goes before existing ones, preserving the historical
-- "newer-overrides-older" behavior within a tie.
insertBySpec :: BoundHandler -> [BoundHandler] -> [BoundHandler]
insertBySpec = insertBy (comparing (patternSpecificity . bhPattern))

-- | Compute a pattern's specificity as a list of segment kinds.
--
-- Compared lexicographically with 'Literal' < 'Capture', so a more
-- specific pattern (more literals at earlier positions) sorts before
-- a less specific one.
patternSpecificity :: Text -> [Specificity]
patternSpecificity pat =
  map segKind $ filter (/= "") $ T.splitOn "/" pat
  where
    segKind seg
      | T.isPrefixOf "{" seg = Capture
      | otherwise            = Literal

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


-- | Build a spire Service from a router.
serve :: Router -> Service IO (Request ByteString) (Response ByteString)
serve router = Service (dispatch router)


-- | Build a service with shared state injected into every request.
serveWithState
  :: Typeable s
  => s -> Router -> Service IO (Request ByteString) (Response ByteString)
serveWithState state router = Service $ \req -> do
  insertExtension (AppState state) (requestExtensions req)
  dispatch router req
