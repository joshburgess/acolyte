-- | HTTP body types: strict, streaming, file, and empty.
--
-- Direct-style sum type — no CPS callbacks like WAI. Composes
-- naturally with tower's @Service req -> IO resp@ model.
module Http.Core.Body
  ( -- * Body type
    Body (..)
  , FileRange (..)
    -- * Constructors
  , fromBytes
  , fromLazyBytes
  , fromBuilder
  , fromText
  , streamBody
  , streamFromList
  , fileBody
  , emptyBody
    -- * Consumption
  , bodyToStrict
  , foldBodyChunks
  , isEmptyBody
    -- * Chunk type
  , BodyChunk (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Data.IORef


-- | A chunk produced by a streaming body.
data BodyChunk
  = Chunk !ByteString  -- ^ A piece of data
  | ChunkFlush         -- ^ Flush hint (send buffered data now)
  deriving (Show, Eq)


-- | A byte range within a file (for Range requests).
data FileRange = FileRange
  { fileRangeOffset :: !Word64
  , fileRangeLength :: !Word64
  } deriving (Show, Eq)


-- | An HTTP body.
--
-- Four strategies for producing bytes:
--
-- * 'BodyStrict' — already in memory. Best for small bodies (JSON, text).
-- * 'BodyStream' — pull-based IO producer. The consumer calls the action
--   repeatedly; it returns 'Just chunk' or 'Nothing' when done. Simpler
--   than WAI's push-based 'StreamingBody' and composes better with
--   middleware that buffers or transforms chunks.
-- * 'BodyFile' — served from disk. Enables sendfile(2) zero-copy.
-- * 'BodyEmpty' — no body (HEAD, 204, 304).
data Body
  = BodyStrict !ByteString
  | BodyStream (IO (Maybe BodyChunk))
  | BodyFile   !FilePath !(Maybe FileRange)
  | BodyEmpty


-- ===================================================================
-- Constructors
-- ===================================================================

fromBytes :: ByteString -> Body
fromBytes = BodyStrict
{-# INLINE fromBytes #-}

fromLazyBytes :: LBS.ByteString -> Body
fromLazyBytes = BodyStrict . LBS.toStrict
{-# INLINE fromLazyBytes #-}

fromBuilder :: Builder.Builder -> Body
fromBuilder = BodyStrict . LBS.toStrict . Builder.toLazyByteString
{-# INLINE fromBuilder #-}

fromText :: Text -> Body
fromText = BodyStrict . TE.encodeUtf8
{-# INLINE fromText #-}

-- | Create a streaming body from a pull-based IO action.
--
-- The action is called repeatedly by the consumer. Return 'Nothing'
-- to signal end-of-body.
--
-- @
-- ref <- newIORef ["a", "b", "c"]
-- let body = streamBody $ do
--       chunks <- readIORef ref
--       case chunks of
--         []   -> pure Nothing
--         c:cs -> writeIORef ref cs >> pure (Just (Chunk c))
-- @
streamBody :: IO (Maybe BodyChunk) -> Body
streamBody = BodyStream

-- | Create a streaming body from a list of byte chunks.
-- Convenient for tests and simple cases.
streamFromList :: [ByteString] -> IO Body
streamFromList chunks = do
  ref <- newIORef chunks
  pure $ BodyStream $ do
    cs <- readIORef ref
    case cs of
      []       -> pure Nothing
      (c:rest) -> do
        writeIORef ref rest
        pure (Just (Chunk c))

fileBody :: FilePath -> Maybe FileRange -> Body
fileBody = BodyFile

emptyBody :: Body
emptyBody = BodyEmpty


-- ===================================================================
-- Consumption
-- ===================================================================

-- | Collect the entire body as strict bytes.
bodyToStrict :: Body -> IO ByteString
bodyToStrict (BodyStrict bs)       = pure bs
bodyToStrict (BodyStream pull)     = BS.concat <$> collectChunks pull
bodyToStrict (BodyFile path mRange) = do
  bs <- BS.readFile path
  pure $ case mRange of
    Nothing -> bs
    Just (FileRange off len) ->
      BS.take (fromIntegral len) (BS.drop (fromIntegral off) bs)
bodyToStrict BodyEmpty = pure BS.empty

-- | Fold over body chunks. Works for all body variants.
foldBodyChunks :: (a -> ByteString -> IO a) -> a -> Body -> IO a
foldBodyChunks f z (BodyStrict bs) = f z bs
foldBodyChunks f z (BodyStream pull) = go z
  where
    go !acc = do
      mChunk <- pull
      case mChunk of
        Nothing           -> pure acc
        Just (Chunk bs)   -> f acc bs >>= go
        Just ChunkFlush   -> go acc
foldBodyChunks f z (BodyFile path _) = BS.readFile path >>= f z
foldBodyChunks _ z BodyEmpty = pure z

-- | Check if a body is empty.
isEmptyBody :: Body -> Bool
isEmptyBody BodyEmpty       = True
isEmptyBody (BodyStrict bs) = BS.null bs
isEmptyBody _               = False


-- Internal
collectChunks :: IO (Maybe BodyChunk) -> IO [ByteString]
collectChunks pull = go []
  where
    go acc = do
      mChunk <- pull
      case mChunk of
        Nothing         -> pure (reverse acc)
        Just (Chunk bs) -> go (bs : acc)
        Just ChunkFlush -> go acc
