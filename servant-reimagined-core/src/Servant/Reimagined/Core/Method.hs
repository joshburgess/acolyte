-- | HTTP method types, promoted to the kind level via DataKinds.
--
-- Each constructor becomes a type-level tag used to parameterize 'Endpoint'.
-- The 'SMethod' singleton allows demoting back to a runtime value when needed
-- (e.g., for routing or OpenAPI generation).
module Servant.Reimagined.Core.Method
  ( -- * Method kind
    Method (..)
    -- * Singleton for runtime access
  , SMethod (..)
  , KnownMethod (..)
  , methodVal
  ) where

import Data.Kind (Constraint, Type)

-- | HTTP methods, used at the type level via DataKinds.
data Method
  = GET
  | POST
  | PUT
  | DELETE
  | PATCH
  | HEAD
  | OPTIONS
  deriving stock (Eq, Ord, Show)

-- | Singleton witness for a promoted 'Method'.
--
-- Lets us recover the runtime method from a type-level tag
-- without carrying a dictionary everywhere.
type SMethod :: Method -> Type
data SMethod (m :: Method) where
  SGET     :: SMethod 'GET
  SPOST    :: SMethod 'POST
  SPUT     :: SMethod 'PUT
  SDELETE  :: SMethod 'DELETE
  SPATCH   :: SMethod 'PATCH
  SHEAD    :: SMethod 'HEAD
  SOPTIONS :: SMethod 'OPTIONS

-- | Class for recovering the singleton from a type-level method.
type KnownMethod :: Method -> Constraint
class KnownMethod (m :: Method) where
  sMethod :: SMethod m

instance KnownMethod 'GET     where sMethod = SGET
instance KnownMethod 'POST    where sMethod = SPOST
instance KnownMethod 'PUT     where sMethod = SPUT
instance KnownMethod 'DELETE  where sMethod = SDELETE
instance KnownMethod 'PATCH   where sMethod = SPATCH
instance KnownMethod 'HEAD    where sMethod = SHEAD
instance KnownMethod 'OPTIONS where sMethod = SOPTIONS

-- | Demote a type-level method to its runtime representation.
methodVal :: forall m. KnownMethod m => Method
methodVal = case sMethod @m of
  SGET     -> GET
  SPOST    -> POST
  SPUT     -> PUT
  SDELETE  -> DELETE
  SPATCH   -> PATCH
  SHEAD    -> HEAD
  SOPTIONS -> OPTIONS
