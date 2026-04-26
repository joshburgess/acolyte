{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, AllowAmbiguousTypes #-}
module NamedArityMismatch where
import Acolyte.Core
import Data.Text (Text)

data User; data H1; data H2; data H3

type NamedAPI =
  '[ Named "health" (Get (At "health") Text)
   , Named "getUser" (Get (Param "users" Int) (Json User))
   ]

test :: ()
test = f ()
  where
    f :: Serves NamedAPI (H1, H2, H3) => () -> ()
    f x = x
