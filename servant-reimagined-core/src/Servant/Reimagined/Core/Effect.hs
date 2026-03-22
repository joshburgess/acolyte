-- | Type-level middleware effects.
--
-- Effects are type-level tags that declare what middleware an endpoint
-- requires. The effect system has two parts:
--
-- 1. 'Requires' wraps an endpoint in the API type to declare a requirement.
-- 2. 'AllEffectsProvided' checks at serve-time that every required effect
--    has been provided.
--
-- Effects are user-extensible: any type of kind 'Type' can be an effect.
-- Built-in effects ('Auth', 'Cors', etc.) are provided for convenience.
--
-- @
-- type MyAPI =
--   '[ Requires Auth (Get '[ Lit "users" ] (Json [User]))
--    , Requires Cors (Get '[ Lit "public" ] (Json Data))
--    , Get '[ Lit "health" ] String  -- no requirements
--    ]
-- @
module Servant.Reimagined.Core.Effect
  ( -- * Built-in effect tags
    Auth
  , Cors
  , RateLimit
  , Tracing
    -- * Declaring requirements
  , Requires
    -- * Checking requirements
  , HasEffect
  , RequiredEffects
  , AllEffectsProvided
  , Assert
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Data.Type.Bool (If)


-- ===================================================================
-- Built-in effect tags (user-extensible: any Type can be an effect)
-- ===================================================================

-- | Authentication middleware required.
data Auth

-- | CORS middleware required.
data Cors

-- | Rate limiting middleware required.
data RateLimit

-- | Tracing/observability middleware required.
data Tracing


-- ===================================================================
-- Requires: declare that an endpoint needs a middleware effect
-- ===================================================================

-- | Declare that an endpoint requires effect @e@.
--
-- @Requires@ is a type-level wrapper — it marks the endpoint but does
-- not change its runtime behavior. At serve-time, 'AllEffectsProvided'
-- checks that every @Requires e _@ in the API has its @e@ in the
-- provided list. If not, compilation fails.
--
-- Multiple effects can be nested:
--
-- @
-- Requires Auth (Requires RateLimit (Get path resp))
-- @
type Requires :: Type -> Type -> Type
data Requires (e :: Type) (endpoint :: Type)


-- ===================================================================
-- Type-level membership check
-- ===================================================================

-- | Check whether effect @e@ is in the provided list @es@.
--
-- Closed type family — O(n) in the length of the list, evaluated by
-- simple top-to-bottom matching with no backtracking.
type HasEffect :: Type -> [Type] -> Bool
type family HasEffect (e :: Type) (es :: [Type]) :: Bool where
  HasEffect _ '[]       = 'False
  HasEffect e (e ': _)  = 'True
  HasEffect e (_ ': es) = HasEffect e es


-- ===================================================================
-- Collect all required effects from an API
-- ===================================================================

-- | Collect all effects required by endpoints in an API.
--
-- Walks the API list and extracts the @e@ from every @Requires e _@.
-- Nested @Requires@ are flattened. Non-@Requires@ endpoints contribute
-- nothing.
type RequiredEffects :: [Type] -> [Type]
type family RequiredEffects (api :: [Type]) :: [Type] where
  RequiredEffects '[] = '[]
  RequiredEffects (Requires e inner ': rest) =
    e ': RequiredEffects ('[inner] ++ rest)
  RequiredEffects (_ ': rest) = RequiredEffects rest

-- | Type-level list append.
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)


-- ===================================================================
-- AllEffectsProvided: the compile-time completeness check
-- ===================================================================

-- | Assert with a custom error message.
type Assert :: Bool -> ErrorMessage -> Constraint
type family Assert (b :: Bool) (msg :: ErrorMessage) :: Constraint where
  Assert 'True  _   = ()
  Assert 'False msg = TypeError msg

-- | Verify that every effect required by the API is in the provided list.
--
-- Walks the API list. For each @Requires e _@, checks that @e@ is in
-- @provided@. If any effect is missing, produces a compile error
-- naming the missing effect.
--
-- Non-@Requires@ endpoints pass through without checks.
-- Nested @Requires@ are checked recursively.
--
-- @
-- -- Compiles: Auth and Cors are both provided.
-- example :: AllEffectsProvided MyAPI '[Auth, Cors] => ()
--
-- -- Fails: Auth is missing.
-- bad :: AllEffectsProvided MyAPI '[Cors] => ()
-- @
type AllEffectsProvided :: [Type] -> [Type] -> Constraint
type family AllEffectsProvided (api :: [Type]) (provided :: [Type]) :: Constraint where
  AllEffectsProvided '[] _ = ()

  AllEffectsProvided (Requires e inner ': rest) provided =
    ( Assert (HasEffect e provided)
        ( 'Text "Missing middleware effect: " ':<>: 'ShowType e
          ':$$: 'Text "This effect is required by an endpoint but was not provided."
          ':$$: 'Text "Add .provide @" ':<>: 'ShowType e
                ':<>: 'Text " to the server builder."
        )
    , AllEffectsProvided '[inner] provided
    , AllEffectsProvided rest provided
    )

  AllEffectsProvided (_ ': rest) provided =
    AllEffectsProvided rest provided
