-- | Ergonomic handler conversion.
--
-- 'ToHandler' converts typed handler functions into the internal
-- 'HandlerFn' representation. Instead of writing:
--
-- @
-- getUserHandler :: HandlerFn
-- getUserHandler parts _body = do
--   mCaps <- lookupExtension \@CaptureList (rpExtensions parts)
--   case mCaps of
--     Just (CaptureList (idText : _)) ->
--       case parseCapture \@Int idText of ...
-- @
--
-- You write:
--
-- @
-- getUserHandler :: PathCapture Int -> IO (Json Text)
-- getUserHandler (PathCapture uid) = pure (Json (T.pack ("user-" ++ show uid)))
-- @
--
-- 'ToHandler' recursively peels arguments from left to right. Each
-- argument must have a 'FromRequestParts' instance. The final return
-- type must be @IO r@ where @r@ has an 'IntoResponse' instance.
module Acolyte.Server.ToHandler
  ( ToHandler (..)
  ) where

import Data.ByteString (ByteString)

import Http.Core (RequestParts (..), Response)
import Acolyte.Server.Extract (FromRequestParts (..))
import Acolyte.Server.Response (IntoResponse (..))
import Acolyte.Server.Handler (HandlerFn)


-- | Convert a typed handler function into a 'HandlerFn'.
--
-- @
-- -- No arguments:
-- health :: IO Text
--
-- -- One extractor:
-- getUser :: PathCapture Int -> IO (Json User)
--
-- -- Multiple extractors:
-- search :: QueryParam "q" Text -> OptionalParam "page" Int -> IO (Json [Result])
--
-- -- Body extractor:
-- create :: JsonBody CreateReq -> IO (Json Item)
--
-- -- Mixed:
-- update :: PathCapture Int -> JsonBody UpdateReq -> IO (Json Item)
-- @
class ToHandler f where
  toHandler :: f -> HandlerFn


-- | Base case: @IO r@ with no arguments.
instance {-# OVERLAPPING #-} IntoResponse r => ToHandler (IO r) where
  toHandler action _parts _body = intoResponse <$> action


-- | Recursive case: peel one 'FromRequestParts' argument.
instance (FromRequestParts a, ToHandler rest) => ToHandler (a -> rest) where
  toHandler f parts body = do
    ea <- fromRequestParts parts
    case ea of
      Left err -> pure (intoResponse err)
      Right a  -> toHandler (f a) parts body
