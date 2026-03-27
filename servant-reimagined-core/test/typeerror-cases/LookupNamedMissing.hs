{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
-- | NEGATIVE TEST: LookupNamed rejects a name not present in the API.
-- Expected error: "not found in API"
module LookupNamedMissing where
import Servant.Reimagined.Core
import Data.Text (Text)

type TestApi = '[ Named "health" (Get (At "health") Text) ]

-- "bogus" doesn't exist in TestApi - should fail with "not found in API"
-- We use a type equality constraint and actually call the function to force
-- GHC to solve the constraint.
test :: ()
test = f ()
  where
    f :: (LookupNamed "bogus" TestApi ~ Named "bogus" (Get (At "bogus") Text)) => () -> ()
    f x = x
