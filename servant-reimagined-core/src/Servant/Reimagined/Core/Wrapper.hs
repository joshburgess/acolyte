-- | Type-level endpoint wrappers for compile-time enforcement.
--
-- These are Layer 2 wrappers (see ARCHITECTURE.md "Two-Layer Middleware
-- Model"). They sit in the API type and modify how individual handlers
-- are bound and dispatched. They are NOT tower Layers — they operate
-- inside the framework's handler dispatch with full type information.
--
-- * 'Protected' — requires authentication; handler must accept auth type
-- * 'Validated' — validates request body before handler runs
-- * 'Versioned' — scopes endpoint to an API version prefix
module Servant.Reimagined.Core.Wrapper
  ( -- * Authentication
    Protected
    -- * Validation
  , Validated
  , Validate (..)
    -- * Versioning prefix
  , Versioned
  , ApiVersion (..)
    -- * Description
  , Describe
    -- * Query parameter annotations
  , WithParams
  , QP
    -- * Header annotations
  , WithHeaders
  , HH
    -- * Streaming RPC markers
  , ServerStream
  , ClientStream
  , BidiStream
    -- * Response status code annotations
  , RespondsWith
  , PostCreated
  , DeleteNoContent
  ) where

import Data.Kind (Type, Constraint)
import Data.Text (Text)
import GHC.TypeLits (Symbol, KnownSymbol, Nat)

import Servant.Reimagined.Core.Endpoint (Endpoint, NoBody, Post, Delete)


-- | An endpoint that requires authentication.
--
-- @auth@ is the authentication result type (e.g., @AuthUser@).
-- @endpoint@ is the underlying endpoint type.
--
-- In the server package, 'Protected' endpoints require a different
-- handler binding function that enforces @auth@ as the first handler
-- argument. Using the normal @bind@ on a 'Protected' endpoint
-- produces a type error.
--
-- Composes with 'Requires':
--
-- @
-- Requires Auth (Protected AuthUser (Get '[ Lit "users" ] (Json [User])))
-- @
type Protected :: Type -> Type -> Type
data Protected (auth :: Type) (endpoint :: Type)


-- | An endpoint with compile-time request body validation.
--
-- @validator@ is a type implementing 'Validate' for the request body type.
-- @endpoint@ is the underlying endpoint type.
--
-- The framework deserializes the request body, runs the validator,
-- and only calls the handler if validation passes. Returns 422 on
-- failure.
--
-- @
-- data CreateUserValidator
-- instance Validate CreateUserValidator CreateUser where
--   validate _ = True  -- real validation logic in server package
--
-- Validated CreateUserValidator (Post '[ Lit "users" ] (Json CreateUser) (Json User))
-- @
type Validated :: Type -> Type -> Type
data Validated (validator :: Type) (endpoint :: Type)


-- | Class for request body validators.
--
-- At the core level this is just a marker — the actual validation
-- logic lives in the server package where IO is available.
-- The core defines the class so the API type can reference it.
class Validate (validator :: Type) (body :: Type) where
  -- | Runtime validation — implemented in the server package.
  -- Core only defines the class head.
  validate :: body -> Either String body


-- | An endpoint scoped to a specific API version.
--
-- The version prefix is prepended to the path at routing time.
--
-- @
-- data V1
-- instance ApiVersion V1 where versionPrefix = "v1"
--
-- Versioned V1 (Get '[ Lit "users" ] (Json [User]))
-- -- matches: /v1/users
-- @
type Versioned :: Type -> Type -> Type
data Versioned (version :: Type) (endpoint :: Type)


-- | Class for API version tags.
class ApiVersion (v :: Type) where
  versionPrefix :: Text


-- | Annotate an endpoint with a human-readable description.
-- Used by OpenAPI generation for endpoint summaries.
-- Transparent to routing — delegates all endpoint metadata to the inner type.
--
-- @
-- type API =
--   '[ Describe "Health check" (Get (At "health") Text)
--    , Describe "Get user by ID" (Get (Param "users" Int) (Json User))
--    ]
-- @
type Describe :: Symbol -> Type -> Type
data Describe (desc :: Symbol) (endpoint :: Type)


-- | Annotate an endpoint with its query parameters for documentation.
-- Transparent to routing — the actual extraction still happens in handlers.
-- Used by OpenAPI generation and client generation to document parameters.
--
-- @
-- type API =
--   '[ WithParams '[QP "page" Int, QP "limit" Int]
--        (Get (At "users") (Json [User]))
--    ]
-- @
type WithParams :: [Type] -> Type -> Type
data WithParams (params :: [Type]) (endpoint :: Type)

-- | A query parameter declaration (name + type).
type QP :: Symbol -> Type -> Type
data QP (name :: Symbol) (a :: Type)


-- | Annotate an endpoint with its required/optional headers.
--
-- @
-- type API =
--   '[ WithHeaders '[HH "Authorization" Text, HH "X-Request-Id" Text]
--        (Get (At "users") (Json [User]))
--    ]
-- @
type WithHeaders :: [Type] -> Type -> Type
data WithHeaders (headers :: [Type]) (endpoint :: Type)

-- | A header declaration (name + type).
type HH :: Symbol -> Type -> Type
data HH (name :: Symbol) (a :: Type)


-- | Mark an endpoint as a server-streaming RPC.
-- The server sends multiple response messages.
-- Used by .proto generation to emit @stream@ in the return type.
--
-- @
-- type API = '[ ServerStream (Get (At "events") (Json Event)) ]
-- @
type ServerStream :: Type -> Type
data ServerStream (endpoint :: Type)

-- | Mark an endpoint as a client-streaming RPC.
-- The client sends multiple request messages.
--
-- @
-- type API = '[ ClientStream (Post (At "upload") (Json Chunk) (Json Result)) ]
-- @
type ClientStream :: Type -> Type
data ClientStream (endpoint :: Type)

-- | Mark an endpoint as a bidirectional-streaming RPC.
-- Both client and server stream messages.
--
-- @
-- type API = '[ BidiStream (Post (At "chat") (Json Msg) (Json Msg)) ]
-- @
type BidiStream :: Type -> Type
data BidiStream (endpoint :: Type)


-- | Declare the expected response status code for documentation.
-- Transparent to routing. Used by OpenAPI generation.
--
-- @
-- type API =
--   '[ RespondsWith 201 (Post (At "users") (Json CreateUser) (Json User))
--    , RespondsWith 204 (Delete (Param "users" Int) (Json ()))
--    ]
-- @
type RespondsWith :: Nat -> Type -> Type
data RespondsWith (status :: Nat) (endpoint :: Type)

-- | Convenience: POST that returns 201 Created.
type PostCreated path req resp = RespondsWith 201 (Post path req resp)

-- | Convenience: DELETE that returns 204 No Content.
type DeleteNoContent path = RespondsWith 204 (Delete path NoBody)
