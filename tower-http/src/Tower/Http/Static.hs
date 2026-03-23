-- | Static file serving and SPA fallback middleware.
--
-- Serves files from a directory when the request path matches a prefix.
-- Includes MIME type detection and directory traversal prevention.
--
-- For single-page applications, the SPA fallback layer serves @index.html@
-- when the inner service returns 404 for paths that don't look like files.
module Tower.Http.Static
  ( -- * Static file serving
    staticFilesLayer
  , StaticConfig (..)
  , defaultStaticConfig
    -- * SPA fallback
  , spaFallbackLayer
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (statusCode, mkStatus)
import System.Directory (canonicalizePath, doesFileExist, getFileSize)
import System.FilePath ((</>), takeExtension)
import Data.List (isPrefixOf)

import Tower (Middleware, middleware)
import Tower.Service (Service (..))
import Http.Core (Request (..), Response (..), ok, badRequest)


-- | Configuration for static file serving.
data StaticConfig = StaticConfig
  { staticPrefix :: !Text
    -- ^ URL prefix to match (e.g. @"/static"@).
  , staticDir    :: !FilePath
    -- ^ Directory to serve files from.
  } deriving (Show)


-- | Construct a 'StaticConfig' with the given prefix and directory.
defaultStaticConfig :: Text -> FilePath -> StaticConfig
defaultStaticConfig = StaticConfig


-- | Middleware that serves files from a directory for paths matching a prefix.
--
-- If the request path starts with the configured prefix, the layer strips the
-- prefix and maps the remainder to a filesystem path under the configured
-- directory. If the file exists, it is served with an appropriate Content-Type
-- header. If the file does not exist, the request passes through to the inner
-- service.
--
-- Directory traversal is prevented by rejecting any path containing @".."@.
staticFilesLayer :: StaticConfig -> Middleware IO (Request ByteString) (Response ByteString)
staticFilesLayer cfg = middleware $ \(Service inner) -> Service $ \req -> do
  let rawPath = TE.decodeUtf8Lenient (requestPathRaw req)
      prefix  = normalizePrefix (staticPrefix cfg)
  if T.isPrefixOf prefix rawPath
    then do
      let relPath = T.unpack (T.drop (T.length prefix) rawPath)
      if hasTraversal relPath
        then pure (badRequest [] "Invalid path")
        else do
          let filePath = staticDir cfg </> relPath
          -- Canonicalize both paths to resolve symlinks and ".." segments
          -- that might bypass the traversal check above.
          canonPath <- canonicalizePath filePath
          canonDir  <- canonicalizePath (staticDir cfg)
          if canonDir `isPrefixOf` canonPath
            then do
              exists <- doesFileExist canonPath
              if exists
                then serveFile canonPath
                else inner req
            else pure (badRequest [] "Invalid path")
    else inner req


-- | SPA fallback middleware.
--
-- If the inner service returns a 404 response and the request path does not
-- look like a file request (i.e. has no file extension) and does not start
-- with @"/api"@, serve @index.html@ from the given directory instead.
spaFallbackLayer :: FilePath -> Middleware IO (Request ByteString) (Response ByteString)
spaFallbackLayer dir = middleware $ \(Service inner) -> Service $ \req -> do
  resp <- inner req
  if statusCode (responseStatus resp) == 404 && isSpaRoute req
    then do
      let indexPath = dir </> "index.html"
      exists <- doesFileExist indexPath
      if exists
        then serveFile indexPath
        else pure resp
    else pure resp


-- ===================================================================
-- Internal helpers
-- ===================================================================

-- | Ensure the prefix ends with a slash for clean stripping.
normalizePrefix :: Text -> Text
normalizePrefix p
  | T.null p         = "/"
  | T.last p == '/'  = p
  | otherwise        = p <> "/"

-- | Check for directory traversal attempts.
hasTraversal :: String -> Bool
hasTraversal path = ".." `elem` splitPath path
  where
    splitPath = filter (not . null) . foldr go [[]]
    go '/' acc       = [] : acc
    go c   (x : xs)  = (c : x) : xs
    go _   []        = []

-- | Determine if a request path looks like a SPA route (not a file, not /api).
isSpaRoute :: Request ByteString -> Bool
isSpaRoute req =
  let rawPath = requestPathRaw req
  in not (hasFileExtension rawPath) && not (BS.isPrefixOf "/api" rawPath)

-- | Check if a path has a file extension.
-- Only examines the last path segment. Dotfiles (e.g. @.hidden@, @.env@)
-- are not considered to have a file extension — the dot must not be the
-- first character.
hasFileExtension :: ByteString -> Bool
hasFileExtension path =
  let segments = filter (not . BS.null) $ BS.split 0x2F path  -- split on '/'
  in case segments of
    [] -> False
    _  -> let lastSeg = last segments
              dotIdx = BS8.elemIndex '.' lastSeg
          in case dotIdx of
               Nothing -> False
               Just 0  -> False  -- dotfile like .hidden, not a file extension
               Just _  -> True

-- | Maximum file size for static file serving (50 MiB).
-- Files larger than this are rejected with 413 Payload Too Large
-- to avoid loading excessive data into memory.
maxStaticFileSize :: Integer
maxStaticFileSize = 50 * 1024 * 1024

-- | Serve a file from disk with appropriate Content-Type.
-- Rejects files larger than 'maxStaticFileSize' with 413.
serveFile :: FilePath -> IO (Response ByteString)
serveFile filePath = do
  size <- getFileSize filePath
  if size > maxStaticFileSize
    then pure (Response (mkStatus 413 "Payload Too Large") [] "File too large")
    else do
      contents <- BS.readFile filePath
      let ext  = drop 1 (takeExtension filePath)  -- drop the leading '.'
          mime = mimeType ext
      pure (ok [("Content-Type", mime)] contents)


-- | Map file extensions to MIME types.
mimeType :: String -> ByteString
mimeType ext = case ext of
  "html"  -> "text/html"
  "css"   -> "text/css"
  "js"    -> "application/javascript"
  "json"  -> "application/json"
  "png"   -> "image/png"
  "jpg"   -> "image/jpeg"
  "jpeg"  -> "image/jpeg"
  "gif"   -> "image/gif"
  "svg"   -> "image/svg+xml"
  "ico"   -> "image/x-icon"
  "woff"  -> "font/woff"
  "woff2" -> "font/woff2"
  "ttf"   -> "font/ttf"
  "txt"   -> "text/plain"
  "xml"   -> "application/xml"
  "pdf"   -> "application/pdf"
  _       -> "application/octet-stream"
