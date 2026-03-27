{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, AllowAmbiguousTypes #-}
module DuplicateNames where
import Servant.Reimagined.Core

test :: ()
test = f ()
  where
    f :: NoDuplicateNames '["health", "getUser", "health"] => () -> ()
    f x = x
