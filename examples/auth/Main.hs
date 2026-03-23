-- | Authentication example: custom extractors for auth.
--
-- Run:  cabal run auth
-- Test: curl http://localhost:3000/public
--       curl -H 'Authorization: Bearer user-token' http://localhost:3000/me
--       curl -H 'Authorization: Bearer admin-token' http://localhost:3000/admin
--       curl http://localhost:3000/me   (401)
--       curl -H 'Authorization: Bearer user-token' http://localhost:3000/admin  (403)
module Main (main) where

import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (statusCode)

import Servant.Reimagined.Prelude


-- ===================================================================
-- Custom AuthUser extractor (FromRequestParts)
-- ===================================================================

data AuthUser = AuthUser
  { auName :: Text
  , auRole :: Text  -- "user" | "admin"
  } deriving (Show)

-- | Extract AuthUser from Bearer token in Authorization header.
-- Returns 401 if missing/invalid.
instance FromRequestParts AuthUser where
  fromRequestParts parts =
    case lookup "authorization" (rpHeaders parts) of
      Just val | "Bearer " `BS.isPrefixOf` val ->
        let token = TE.decodeUtf8 (BS.drop 7 val)
        in pure $ case validateToken token of
          Just user -> Right user
          Nothing   -> Left (mkError status401 "Invalid token")
      _ -> pure $ Left (mkError status401 "Missing Authorization header")

-- | Fake token validation — in real code, verify JWT or session.
validateToken :: Text -> Maybe AuthUser
validateToken "user-token"  = Just (AuthUser "alice" "user")
validateToken "admin-token" = Just (AuthUser "bob" "admin")
validateToken _             = Nothing

-- | Helper: require admin role, returning 403 if not.
requireAdmin :: AuthUser -> IO (Either ServerError ())
requireAdmin user
  | auRole user == "admin" = pure (Right ())
  | otherwise = pure (Left (mkError status403 "Admin access required"))


-- ===================================================================
-- API type
-- ===================================================================

type AuthAPI =
  '[ Get (At "public") Text                       -- GET /public (no auth)
   , Get (At "me") (Json AuthUser)                 -- GET /me     (auth required)
   , Get (At "admin") (Json AuthUser)              -- GET /admin  (admin required)
   ]

instance Aeson.ToJSON AuthUser where
  toJSON u = object ["name" .= auName u, "role" .= auRole u]


-- ===================================================================
-- Handlers
-- ===================================================================

publicHandler :: IO Text
publicHandler = pure "This endpoint is public. No auth needed."

meHandler :: AuthUser -> IO (Json AuthUser)
meHandler user = pure (Json user)

adminHandler :: AuthUser -> IO (Either ServerError (Json AuthUser))
adminHandler user = do
  result <- requireAdmin user
  pure $ case result of
    Left err -> Left err
    Right () -> Right (Json user)


-- ===================================================================
-- Server + main
-- ===================================================================

server :: Service IO (Request ByteString) (Response ByteString)
server = mkApi @AuthAPI (publicHandler, meHandler, adminHandler)

main :: IO ()
main = do
  putStrLn "Auth example on http://localhost:3000"
  putStrLn ""
  putStrLn "Tokens:"
  putStrLn "  Bearer user-token   -> alice (user)"
  putStrLn "  Bearer admin-token  -> bob (admin)"
  putStrLn ""
  putStrLn "Endpoints:"
  putStrLn "  GET /public  - no auth"
  putStrLn "  GET /me      - requires valid token"
  putStrLn "  GET /admin   - requires admin token"
  runServerBS 3000 server
