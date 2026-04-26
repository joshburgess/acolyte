-- | Endpoint type: the specification of a single HTTP endpoint.
--
-- An 'Endpoint' carries the method, path, request body type, and response
-- type as phantom parameters. It exists only at the type level — the runtime
-- representation is empty (just a proxy/phantom).
--
-- The response type is the /declared/ happy-path type — what appears in
-- OpenAPI specs and client return types. Handlers are free to return
-- @Either AppError resp@ or any compatible response type.
module Acolyte.Core.Endpoint
  ( -- * Core endpoint type
    Endpoint
    -- * Convenience aliases
  , Get
  , Post
  , Put
  , Delete
  , Patch
    -- * Request body marker
  , NoBody
    -- * JSON response/request marker
  , Json (..)
  ) where

import Data.Kind (Type)
import Acolyte.Core.Method (Method (..))
import Acolyte.Core.Path (PathSegment)


-- | A marker for endpoints with no request body (GET, DELETE, HEAD).
data NoBody


-- | A single HTTP endpoint descriptor.
--
-- All parameters are phantom — this type has no runtime representation.
--
-- * @m@ — HTTP method ('GET, 'POST, etc.)
-- * @path@ — URL path as a type-level list of 'PathSegment'
-- * @req@ — request body type ('NoBody' for bodyless methods)
-- * @resp@ — declared response type (the happy-path type for OpenAPI/clients)
type Endpoint :: Method -> [PathSegment] -> Type -> Type -> Type
data Endpoint (m :: Method) (path :: [PathSegment]) (req :: Type) (resp :: Type)


-- | @GET@ endpoint with no request body.
--
-- @
-- type ListUsers = Get '[ Lit "users" ] (Json [User])
-- type GetUser   = Get '[ Lit "users", Capture Int ] (Json User)
-- @
type Get path resp = Endpoint 'GET path NoBody resp

-- | @POST@ endpoint with a request body.
--
-- @
-- type CreateUser = Post '[ Lit "users" ] (Json CreateUserReq) (Json User)
-- @
type Post path req resp = Endpoint 'POST path req resp

-- | @PUT@ endpoint with a request body.
type Put path req resp = Endpoint 'PUT path req resp

-- | @DELETE@ endpoint with no request body.
type Delete path resp = Endpoint 'DELETE path NoBody resp

-- | @PATCH@ endpoint with a request body.
type Patch path req resp = Endpoint 'PATCH path req resp


-- | A JSON-encoded value, used as a request or response body marker.
--
-- In API types: @Get path (Json User)@ declares a JSON response.
-- In handlers: @Json val@ wraps a value for JSON serialization.
--
-- The core package defines only the newtype. The server package
-- provides @IntoResponse (Json a)@ for HTTP responses.
newtype Json a = Json { unJson :: a }
  deriving (Show, Eq)
