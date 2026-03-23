-- | Content negotiation example.
--
-- Returns JSON or plain text depending on the Accept header.
--
-- Run:  cabal run negotiate
-- Test: curl -H "Accept: application/json" http://localhost:3000/users
--       curl -H "Accept: text/plain" http://localhost:3000/users
--       curl -H "Accept: text/html" http://localhost:3000/users   (406)
module Main (main) where

import Data.Aeson (ToJSON (..), (.=), object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

import Servant.Reimagined.Prelude


-- ===================================================================
-- Domain types
-- ===================================================================

data User = User { userId :: Int, userName :: Text, userEmail :: Text }
  deriving (Show)

instance ToJSON User where
  toJSON u = object
    [ "id"    .= userId u
    , "name"  .= userName u
    , "email" .= userEmail u
    ]

-- | Render users as plain text (one per line).
usersToText :: [User] -> Text
usersToText = T.intercalate "\n" . map fmt
  where fmt u = T.pack (show (userId u)) <> ". " <> userName u
                <> " <" <> userEmail u <> ">"

sampleUsers :: [User]
sampleUsers =
  [ User 1 "Alice" "alice@example.com"
  , User 2 "Bob"   "bob@example.com"
  , User 3 "Carol" "carol@example.com"
  ]


-- ===================================================================
-- Available formats for content negotiation
-- ===================================================================

-- | The format table: content-type paired with its encoder.
-- Uses FormatEncoder instances from the server package:
--   FormatEncoder JsonFormat a  (via aeson ToJSON)
--   FormatEncoder TextFormat Text  (via text encoding)
userFormats :: [(Text, [User] -> ByteString)]
userFormats =
  [ ("application/json", formatEncode @JsonFormat)
  , ("text/plain",       formatEncode @TextFormat . usersToText)
  ]


-- ===================================================================
-- API + Handler
-- ===================================================================

type NegotiateAPI =
  '[ Describe "List users (content-negotiated)"
       (Get (At "users") (Response ByteString))
   ]

-- | Handler that inspects the Accept header and uses 'negotiate'
-- to pick the right serialization format.
--
-- 'Optional (ReqHeader "Accept")' gracefully handles missing headers.
-- 'negotiate' from Servant.Reimagined.Server.Negotiate does the matching:
--   - parseAccept parses "application/json, text/plain;q=0.9" into ranked list
--   - matchFormat finds the best encoder from userFormats
--   - Returns 406 Not Acceptable if nothing matches
getUsers :: Optional (ReqHeader "Accept") -> IO (Response ByteString)
getUsers (Optional mAccept) =
  let acceptHdr = maybe BS.empty unReqHeader mAccept
  in pure (negotiate acceptHdr userFormats sampleUsers)


-- ===================================================================
-- Server + main
-- ===================================================================

server :: Service IO (Request ByteString) (Response ByteString)
server = mkApi @NegotiateAPI getUsers

main :: IO ()
main = do
  putStrLn "Negotiate example on http://localhost:3000"
  putStrLn "  GET /users  - returns JSON or plain text based on Accept header"
  putStrLn ""
  putStrLn "Try:"
  putStrLn "  curl -H 'Accept: application/json' http://localhost:3000/users"
  putStrLn "  curl -H 'Accept: text/plain' http://localhost:3000/users"
  putStrLn "  curl -H 'Accept: text/html' http://localhost:3000/users  (406)"
  runServerBS 3000 server
