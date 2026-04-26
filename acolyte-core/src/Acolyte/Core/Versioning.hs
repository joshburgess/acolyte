-- | Type-level API versioning with typed deltas.
--
-- Express API evolution as type-level change operations with compile-time
-- backward compatibility checking.
--
-- @
-- type V1 =
--   '[ Get '[ Lit "users" ] (Json [UserV1])
--    , Get '[ Lit "users", Capture Int ] (Json UserV1)
--    ]
--
-- type V2Changes =
--   '[ 'Added (Get '[ Lit "profiles", Capture Int ] (Json Profile))
--    , 'Replaced (Get '[ Lit "users", Capture Int ] (Json UserV1))
--                (Get '[ Lit "users", Capture Int ] (Json UserV2))
--    , 'Deprecated (Get '[ Lit "users" ] (Json [UserV1]))
--    ]
--
-- -- Compiles: no Removed changes, so backward compatible.
-- type V2 = VersionedApi V1 V2Changes (ApplyChanges V1 V2Changes)
-- @
module Acolyte.Core.Versioning
  ( -- * Change kind
    Change (..)
    -- * Versioned API type
  , VersionedApi
    -- * Type families
  , ApplyChanges
  , IsBackwardCompatible
    -- * Constraint alias
  , BackwardCompatible
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (TypeError, ErrorMessage (..))

import Acolyte.Core.API (type (++))
import Acolyte.Core.Effect (Assert)


-- | A change to an API between versions.
data Change
  = Added Type        -- ^ New endpoint added
  | Removed Type      -- ^ Endpoint removed (breaking!)
  | Replaced Type Type -- ^ Endpoint replaced with a new version
  | Deprecated Type   -- ^ Endpoint marked for future removal (still works)


-- | A versioned API with its base, changes, and resolved endpoint list.
--
-- The three parameters enable:
-- * @base@ — the previous version's API type
-- * @changes@ — the list of changes applied
-- * @resolved@ — the resulting API type (computed via 'ApplyChanges')
type VersionedApi :: [Type] -> [Change] -> [Type] -> Type
data VersionedApi (base :: [Type]) (changes :: [Change]) (resolved :: [Type])


-- | Apply a list of changes to a base API, producing the new API.
--
-- * @Added e@ — appends @e@ to the API
-- * @Removed e@ — removes @e@ from the API (via 'Remove')
-- * @Replaced old new@ — removes @old@, appends @new@
-- * @Deprecated e@ — no change to the endpoint list (it still works)
type ApplyChanges :: [Type] -> [Change] -> [Type]
type family ApplyChanges (base :: [Type]) (changes :: [Change]) :: [Type] where
  ApplyChanges base '[] = base
  ApplyChanges base ('Added e       ': rest) = ApplyChanges (base ++ '[e]) rest
  ApplyChanges base ('Removed e     ': rest) = ApplyChanges (Remove e base) rest
  ApplyChanges base ('Replaced o n  ': rest) = ApplyChanges (Remove o base ++ '[n]) rest
  ApplyChanges base ('Deprecated _  ': rest) = ApplyChanges base rest


-- | Remove a type from a type-level list (first occurrence).
type Remove :: Type -> [Type] -> [Type]
type family Remove (x :: Type) (xs :: [Type]) :: [Type] where
  Remove _ '[]       = '[]
  Remove x (x ': ys) = ys
  Remove x (y ': ys) = y ': Remove x ys


-- | Check whether a list of changes is backward compatible.
--
-- A change set is backward compatible if it contains no 'Removed' changes.
-- Added, Replaced, and Deprecated are all considered backward compatible.
type IsBackwardCompatible :: [Change] -> Bool
type family IsBackwardCompatible (changes :: [Change]) :: Bool where
  IsBackwardCompatible '[]                    = 'True
  IsBackwardCompatible ('Removed _ ': _)      = 'False
  IsBackwardCompatible (_          ': rest)   = IsBackwardCompatible rest


-- | Constraint that a change set is backward compatible.
--
-- Produces a clear type error if any 'Removed' change is present.
type BackwardCompatible :: [Change] -> Constraint
type family BackwardCompatible (changes :: [Change]) :: Constraint where
  BackwardCompatible changes =
    Assert (IsBackwardCompatible changes)
      ( 'Text "API version change is not backward compatible."
        ':$$: 'Text "The change set contains a 'Removed' endpoint."
        ':$$: 'Text "Remove the 'Removed' change or use 'Deprecated' instead."
      )


-- Assert imported from Acolyte.Core.Effect
