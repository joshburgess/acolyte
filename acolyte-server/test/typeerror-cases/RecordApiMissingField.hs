{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, TypeApplications, AllowAmbiguousTypes, OverloadedStrings #-}
module RecordApiMissingField where
import Data.ByteString (ByteString)
import Data.Text (Text)
import Spire.Service (Service)
import Http.Core (Request, Response)
import Acolyte.Core
import Acolyte.Server

type TestAPI =
  '[ Named "health"  (Get (At "health") Text)
   , Named "getUser" (Get (Param "users" Int) (Json Text))
   ]

data WrongHandlers = WrongHandlers { wrongName :: IO Text }

test :: Service IO (Request ByteString) (Response ByteString)
test = mkRecordApi @TestAPI WrongHandlers { wrongName = pure ("ok" :: Text) }
