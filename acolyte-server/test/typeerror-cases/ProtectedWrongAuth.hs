{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, TypeApplications, AllowAmbiguousTypes, OverloadedStrings #-}
module ProtectedWrongAuth where
import Data.ByteString (ByteString)
import Data.Text (Text)
import Spire.Service (Service)
import Http.Core (Request, Response)
import Acolyte.Core
import Acolyte.Server

data AuthUser

type MyAPI = '[ Protected AuthUser (Get (At "secret") Text) ]

-- Handler does NOT take AuthUser as first arg — should fail
test :: Service IO (Request ByteString) (Response ByteString)
test = mkApi @MyAPI (pure "secret" :: IO Text)
