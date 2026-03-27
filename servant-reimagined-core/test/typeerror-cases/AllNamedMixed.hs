{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, AllowAmbiguousTypes #-}
-- | NEGATIVE TEST: AllNamed rejects a mixed API (some endpoints not Named).
-- Expected error: "Expected a Named endpoint, but got:"
module AllNamedMixed where
import Servant.Reimagined.Core
import Data.Text (Text)
import Data.Kind (Constraint)

data User; data Json a

type MixedAPI =
  '[ Named "health" (Get (At "health") Text)
   , Get (Param "users" Int) (Json User)     -- not Named!
   ]

-- Force GHC to solve the constraint (no constraint on the signature)
test :: ()
test = f ()
  where
    f :: AllNamed MixedAPI => () -> ()
    f x = x
