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
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, KnownSymbol)


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
  versionPrefix :: String
