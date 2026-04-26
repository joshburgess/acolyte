{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, TypeApplications, AllowAmbiguousTypes #-}
module RecordApiBare where
import Data.ByteString (ByteString)
import Data.Text (Text)
import Spire.Service (Service)
import Http.Core (Request, Response)
import Acolyte.Core
import Acolyte.Server

type BareAPI = '[ Get (At "health") Text ]

data Handlers = Handlers { health :: IO Text }

test :: Service IO (Request ByteString) (Response ByteString)
test = mkRecordApi @BareAPI Handlers { health = pure "ok" }
