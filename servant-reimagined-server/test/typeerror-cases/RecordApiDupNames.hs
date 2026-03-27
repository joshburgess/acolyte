{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, TypeApplications, AllowAmbiguousTypes #-}
module RecordApiDupNames where
import Data.ByteString (ByteString)
import Data.Text (Text)
import Tower.Service (Service)
import Http.Core (Request, Response)
import Servant.Reimagined.Core
import Servant.Reimagined.Server

type DupAPI =
  '[ Named "health" (Get (At "health") Text)
   , Named "health" (Get (Param "users" Int) (Json Text))
   ]

data Handlers = Handlers { health :: IO Text }

test :: Service IO (Request ByteString) (Response ByteString)
test = mkRecordApi @DupAPI Handlers { health = pure "ok" }
