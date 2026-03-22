-- | Typed heterogeneous map for passing data between middleware and handlers.
--
-- 'Extensions' is keyed by 'TypeRep' — each type can have at most one
-- value stored. This is how middleware communicates with handlers:
-- auth middleware stores the authenticated user, tracing middleware
-- stores the request ID, and handlers extract what they need.
--
-- @
-- -- Middleware stores auth info
-- insertExtension (AuthUser "alice") exts
--
-- -- Handler retrieves it
-- mUser <- lookupExtension @AuthUser exts
-- @
--
-- Thread-safe via 'IORef'. Multiple middleware can insert concurrently
-- (though in practice middleware runs sequentially per request).
module Http.Core.Extensions
  ( -- * Extensions type
    Extensions
    -- * Construction
  , emptyExtensions
  , newExtensions
    -- * Operations
  , insertExtension
  , lookupExtension
  , deleteExtension
  , hasExtension
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable, TypeRep, typeRep, Proxy (..))
import Unsafe.Coerce (unsafeCoerce)


-- | An opaque wrapper around a value of any type.
-- Typed retrieval is safe because we key by TypeRep.
data Any = forall a. Any a

-- | A mutable, typed heterogeneous map.
--
-- Keyed by 'TypeRep' — each Typeable type can store at most one value.
-- Uses 'IORef' for thread-safe mutation.
newtype Extensions = Extensions (IORef (Map TypeRep Any))


-- | Create an empty 'Extensions'.
emptyExtensions :: IO Extensions
emptyExtensions = Extensions <$> newIORef Map.empty
{-# INLINE emptyExtensions #-}


-- | Alias for 'emptyExtensions'.
newExtensions :: IO Extensions
newExtensions = emptyExtensions
{-# INLINE newExtensions #-}


-- | Insert a typed value. Overwrites any existing value of the same type.
insertExtension :: forall a. Typeable a => a -> Extensions -> IO ()
insertExtension val (Extensions ref) =
  modifyIORef' ref (Map.insert (typeRep (Proxy @a)) (Any val))
{-# INLINE insertExtension #-}


-- | Look up a typed value. Returns 'Nothing' if no value of that type
-- has been inserted.
lookupExtension :: forall a. Typeable a => Extensions -> IO (Maybe a)
lookupExtension (Extensions ref) = do
  m <- readIORef ref
  pure $ case Map.lookup (typeRep (Proxy @a)) m of
    Just (Any val) -> Just (unsafeCoerce val)
    Nothing        -> Nothing
{-# INLINE lookupExtension #-}


-- | Remove a typed value. No-op if the type is not present.
deleteExtension :: forall a. Typeable a => Extensions -> IO ()
deleteExtension (Extensions ref) =
  modifyIORef' ref (Map.delete (typeRep (Proxy @a)))
{-# INLINE deleteExtension #-}


-- | Check whether a typed value is present.
hasExtension :: forall a. Typeable a => Extensions -> IO Bool
hasExtension (Extensions ref) = do
  m <- readIORef ref
  pure $ Map.member (typeRep (Proxy @a)) m
{-# INLINE hasExtension #-}
