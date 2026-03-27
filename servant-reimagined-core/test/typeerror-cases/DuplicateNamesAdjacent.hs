{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, AllowAmbiguousTypes #-}
module DuplicateNamesAdjacent where
import Servant.Reimagined.Core

test :: ()
test = f ()
  where
    f :: NoDuplicateNames '["foo", "foo"] => () -> ()
    f x = x
