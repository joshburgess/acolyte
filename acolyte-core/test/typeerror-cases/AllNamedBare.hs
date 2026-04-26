{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, AllowAmbiguousTypes #-}
module AllNamedBare where
import Acolyte.Core
import Data.Text (Text)

type BareAPI = '[ Get (At "health") Text ]

test :: ()
test = f ()
  where
    f :: AllNamed BareAPI => () -> ()
    f x = x
