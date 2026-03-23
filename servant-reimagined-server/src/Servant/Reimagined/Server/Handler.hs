-- | Handler binding: connecting typed handlers to API endpoints.
module Servant.Reimagined.Server.Handler
  ( -- * Bound handler
    BoundHandler (..)
  , HandlerFn
  , MatchResult (..)
    -- * Endpoint metadata
  , HasEndpointInfo (..)
    -- * Path reflection
  , ReflectPath (..)
    -- * Handler constructors
  , mkHandler0
  , mkHandler1Parts
  , mkHandler1Body
  , mkHandler2PartsBody
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Network.HTTP.Types (Method)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy (..))

import Http.Core
  ( Request (..), Response (..)
  , RequestParts (..)
  , Extensions, insertExtension
  )
import Servant.Reimagined.Server.Extract
import Servant.Reimagined.Server.Response

import Servant.Reimagined.Core.Endpoint (Endpoint)
import Servant.Reimagined.Core.Effect (Requires)
import Servant.Reimagined.Core.Wrapper
  ( Protected, Validated, Describe, WithParams, WithHeaders
  , ServerStream, ClientStream, BidiStream, RespondsWith
  )
import Servant.Reimagined.Core.Method (KnownMethod, methodVal)
import qualified Servant.Reimagined.Core.Method as Core
import Servant.Reimagined.Core.Path (PathSegment(..))


-- | A type-erased handler function.
type HandlerFn = RequestParts -> ByteString -> IO (Response ByteString)


-- | Result of matching a path.
data MatchResult = MatchResult
  { mrCaptures :: ![Text]
  } deriving (Show)


-- | A handler bound to a specific endpoint.
data BoundHandler = BoundHandler
  { bhMethod   :: !Method
  , bhPattern  :: !Text
  , bhMatchFn  :: [Text] -> Maybe MatchResult
  , bhHandler  :: HandlerFn
  }


-- ===================================================================
-- Endpoint metadata extraction
-- ===================================================================

-- | Extract HTTP method, URL pattern, and path matcher from an endpoint type.
class HasEndpointInfo endpoint where
  endpointMethod  :: Method
  endpointPattern :: Text
  endpointMatcher :: [Text] -> Maybe MatchResult


instance (KnownMethod m, ReflectPath path)
  => HasEndpointInfo (Endpoint m path req resp) where
    endpointMethod  = methodToBS (methodVal @m)
    endpointPattern = reflectPattern @path
    endpointMatcher = reflectMatch @path

-- Requires delegates to inner endpoint (Layer 1 effect tracking is phantom-only)
instance HasEndpointInfo inner
  => HasEndpointInfo (Requires e inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- Protected delegates to inner endpoint (Layer 2 wrapper)
instance HasEndpointInfo inner
  => HasEndpointInfo (Protected auth inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- Validated delegates to inner endpoint (Layer 2 wrapper)
instance HasEndpointInfo inner
  => HasEndpointInfo (Validated v inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- Describe delegates to inner endpoint (Layer 2 wrapper)
instance HasEndpointInfo inner
  => HasEndpointInfo (Describe desc inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- WithParams delegates to inner endpoint (transparent annotation)
instance HasEndpointInfo inner
  => HasEndpointInfo (WithParams ps inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- WithHeaders delegates to inner endpoint (transparent annotation)
instance HasEndpointInfo inner
  => HasEndpointInfo (WithHeaders hs inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- ServerStream delegates to inner endpoint (streaming marker)
instance HasEndpointInfo inner
  => HasEndpointInfo (ServerStream inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- ClientStream delegates to inner endpoint (streaming marker)
instance HasEndpointInfo inner
  => HasEndpointInfo (ClientStream inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- BidiStream delegates to inner endpoint (streaming marker)
instance HasEndpointInfo inner
  => HasEndpointInfo (BidiStream inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner

-- RespondsWith delegates to inner endpoint (status code annotation)
instance HasEndpointInfo inner
  => HasEndpointInfo (RespondsWith s inner) where
    endpointMethod  = endpointMethod @inner
    endpointPattern = endpointPattern @inner
    endpointMatcher = endpointMatcher @inner


methodToBS :: Core.Method -> Method
methodToBS Core.GET     = "GET"
methodToBS Core.POST    = "POST"
methodToBS Core.PUT     = "PUT"
methodToBS Core.DELETE  = "DELETE"
methodToBS Core.PATCH   = "PATCH"
methodToBS Core.HEAD    = "HEAD"
methodToBS Core.OPTIONS = "OPTIONS"


-- ===================================================================
-- Path reflection: type-level -> runtime
-- ===================================================================

-- | Reflect a type-level path into a runtime URL pattern and path matcher.
class ReflectPath (path :: [PathSegment]) where
  reflectPattern :: Text
  reflectMatch   :: [Text] -> Maybe MatchResult

instance ReflectPath '[] where
  reflectPattern = ""
  reflectMatch [] = Just (MatchResult [])
  reflectMatch _  = Nothing

instance (KnownSymbol s, ReflectPath rest) => ReflectPath ('Lit s ': rest) where
  reflectPattern = "/" <> T.pack (symbolVal (Proxy @s)) <> reflectPattern @rest
  reflectMatch (seg : segs)
    | seg == T.pack (symbolVal (Proxy @s)) = reflectMatch @rest segs
    | otherwise = Nothing
  reflectMatch [] = Nothing

instance ReflectPath rest => ReflectPath ('Capture t ': rest) where
  reflectPattern = "/{capture}" <> reflectPattern @rest
  reflectMatch (seg : segs) =
    case reflectMatch @rest segs of
      Just (MatchResult caps) -> Just (MatchResult (seg : caps))
      Nothing                 -> Nothing
  reflectMatch [] = Nothing


-- ===================================================================
-- Handler constructors
-- ===================================================================

-- | Build a handler that takes no extracted arguments.
mkHandler0 :: IntoResponse resp => IO resp -> HandlerFn
mkHandler0 action _parts _body = intoResponse <$> action

-- | Build a handler that extracts one argument from request parts (not body).
mkHandler1Parts
  :: (FromRequestParts a, IntoResponse resp)
  => (a -> IO resp) -> HandlerFn
mkHandler1Parts f parts _body = do
  ea <- fromRequestParts parts
  case ea of
    Left err -> pure (intoResponse err)
    Right a  -> intoResponse <$> f a

-- | Build a handler that extracts one argument from the request body.
mkHandler1Body
  :: (FromRequest b, IntoResponse resp)
  => (b -> IO resp) -> HandlerFn
mkHandler1Body f parts body = do
  eb <- fromRequest parts body
  case eb of
    Left err -> pure (intoResponse err)
    Right b  -> intoResponse <$> f b

-- | Build a handler that extracts one argument from request parts and one from the body.
mkHandler2PartsBody
  :: (FromRequestParts a, FromRequest b, IntoResponse resp)
  => (a -> b -> IO resp) -> HandlerFn
mkHandler2PartsBody f parts body = do
  ea <- fromRequestParts parts
  case ea of
    Left err -> pure (intoResponse err)
    Right a -> do
      eb <- fromRequest parts body
      case eb of
        Left err -> pure (intoResponse err)
        Right b  -> intoResponse <$> f a b
