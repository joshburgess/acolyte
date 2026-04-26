{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, AllowAmbiguousTypes #-}
module DuplicateNamesAdjacent where
import Acolyte.Core

test :: ()
test = f ()
  where
    f :: NoDuplicateNames '["foo", "foo"] => () -> ()
    f x = x
