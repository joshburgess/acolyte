-- | Typed heterogeneous map for passing data between middleware and handlers.
--
-- 'Extensions' is keyed by 'TypeRep' fingerprint — each type can have at
-- most one value stored. This is how middleware communicates with handlers:
-- auth middleware stores the authenticated user, tracing middleware
-- stores the request ID, and handlers extract what they need.
--
-- Uses 'IntMap' internally for O(1) lookup (hashed TypeRep fingerprint)
-- instead of O(log n) with 'Map TypeRep'.
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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Typeable (Typeable, typeRepFingerprint, typeRep, Proxy (..))
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)


-- | An opaque wrapper around a value of any type.
data Any = forall a. Any a

-- | Hash a TypeRep fingerprint to an Int for IntMap keying.
typeKey :: forall a. Typeable a => Int
typeKey =
  let Fingerprint w1 _ = typeRepFingerprint (typeRep (Proxy @a))
  in fromIntegral w1
{-# INLINE typeKey #-}

-- | A mutable, typed heterogeneous map.
--
-- Keyed by TypeRep fingerprint hash — O(1) lookup via IntMap.
newtype Extensions = Extensions (IORef (IntMap Any))


-- | Create an empty 'Extensions'.
emptyExtensions :: IO Extensions
emptyExtensions = Extensions <$> newIORef IntMap.empty
{-# INLINE emptyExtensions #-}


-- | Alias for 'emptyExtensions'.
newExtensions :: IO Extensions
newExtensions = emptyExtensions
{-# INLINE newExtensions #-}


-- | Insert a typed value. Overwrites any existing value of the same type.
insertExtension :: forall a. Typeable a => a -> Extensions -> IO ()
insertExtension val (Extensions ref) =
  modifyIORef' ref (IntMap.insert (typeKey @a) (Any val))
{-# INLINE insertExtension #-}


-- | Look up a typed value. Returns 'Nothing' if no value of that type
-- has been inserted.
lookupExtension :: forall a. Typeable a => Extensions -> IO (Maybe a)
lookupExtension (Extensions ref) = do
  m <- readIORef ref
  pure $ case IntMap.lookup (typeKey @a) m of
    Just (Any val) -> Just (unsafeCoerce val)
    Nothing        -> Nothing
{-# INLINE lookupExtension #-}


-- | Remove a typed value. No-op if the type is not present.
deleteExtension :: forall a. Typeable a => Extensions -> IO ()
deleteExtension (Extensions ref) =
  modifyIORef' ref (IntMap.delete (typeKey @a))
{-# INLINE deleteExtension #-}


-- | Check whether a typed value is present.
hasExtension :: forall a. Typeable a => Extensions -> IO Bool
hasExtension (Extensions ref) = do
  m <- readIORef ref
  pure $ IntMap.member (typeKey @a) m
{-# INLINE hasExtension #-}
