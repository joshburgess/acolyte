-- | The core Service abstraction.
--
-- A 'Service' transforms requests into responses in some monad.
-- This is the fundamental building block — HTTP servers, gRPC handlers,
-- message queue consumers, and anything else that processes requests
-- can be expressed as a Service.
--
-- @
-- -- An echo service
-- echo :: Service IO String String
-- echo = Service pure
--
-- -- A service that looks up users
-- userService :: Service IO UserId (Maybe User)
-- userService = Service $ \uid -> lookupUser uid
-- @
module Tower.Service
  ( -- * Service type
    Service (..)
    -- * Combinators
  , mapRequest
  , mapResponse
  , mapResponseM
  , contramapRequest
  , dimap
  , hoistService
  ) where


-- | A service transforms requests into responses in monad @m@.
--
-- This is a newtype over @req -> m resp@ — deliberately simple.
-- Composition happens via 'Tower.Layer.Layer', not via Service itself.
--
-- The monad parameter @m@ lets the same type work with 'IO',
-- @ReaderT Env IO@, or any monad — unlike Rust's Tower which
-- hardcodes @Future@ as the effect type.
newtype Service m req resp = Service
  { runService :: req -> m resp
  }


-- | Transform the response after the service produces it.
--
-- @
-- addHeader :: Service IO Request Response -> Service IO Request Response
-- addHeader = mapResponse (\resp -> resp { headers = ("X-Foo", "bar") : headers resp })
-- @
mapResponse :: Functor m => (resp -> resp') -> Service m req resp -> Service m req resp'
mapResponse f (Service g) = Service (fmap f . g)
{-# INLINE mapResponse #-}


-- | Transform the response monadically.
mapResponseM :: Monad m => (resp -> m resp') -> Service m req resp -> Service m req resp'
mapResponseM f (Service g) = Service (\req -> g req >>= f)
{-# INLINE mapResponseM #-}


-- | Transform the request before it reaches the service.
--
-- @
-- stripPrefix :: Service IO Request Response -> Service IO Request Response
-- stripPrefix = mapRequest (\req -> req { path = drop 1 (path req) })
-- @
mapRequest :: (req' -> req) -> Service m req resp -> Service m req' resp
mapRequest f (Service g) = Service (g . f)
{-# INLINE mapRequest #-}


-- | Alias for 'mapRequest' (emphasizes the contravariant direction).
contramapRequest :: (req' -> req) -> Service m req resp -> Service m req' resp
contramapRequest = mapRequest
{-# INLINE contramapRequest #-}


-- | Map over both request (contravariantly) and response (covariantly).
--
-- @
-- dimap reqTransform respTransform service
-- @
dimap :: Functor m => (req' -> req) -> (resp -> resp') -> Service m req resp -> Service m req' resp'
dimap f g (Service h) = Service (fmap g . h . f)
{-# INLINE dimap #-}


-- | Change the monad a service runs in.
--
-- @
-- liftService :: Service IO req resp -> Service (ReaderT Env IO) req resp
-- liftService = hoistService liftIO
-- @
hoistService :: (forall a. m a -> n a) -> Service m req resp -> Service n req resp
hoistService nat (Service f) = Service (nat . f)
{-# INLINE hoistService #-}
