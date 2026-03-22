{-# LANGUAGE OverloadedStrings #-}
-- | End-to-end example: define an API, write handlers, stack middleware,
-- run on tower-server.
--
-- Run: cabal run hello-world
-- Test: curl http://localhost:3000/health
--       curl http://localhost:3000/users
--       curl http://localhost:3000/users/42
--       curl -X POST http://localhost:3000/users -d '{"name":"alice"}' -H 'Content-Type: application/json'
--       curl -X OPTIONS http://localhost:3000/users  (preflight)
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Network.HTTP.Types (status200, status400, status500)

import Tower
import Tower.Service (Service (..))
import Tower.Http
import Tower.Server (runServerBS)
import Http.Core

import Servant.Reimagined.Core
import Servant.Reimagined.Server


-- ===================================================================
-- 1. Define the API as types
-- ===================================================================

type HealthPath   = '[ 'Lit "health" ]
type UsersPath    = '[ 'Lit "users" ]
type UserByIdPath = '[ 'Lit "users", 'Capture Int ]

-- The API type: endpoints with effects
type MyAPI =
  '[ Get  HealthPath   Text                         -- GET /health (public)
   , Requires Auth (Get UsersPath (Json [Text]))     -- GET /users (needs auth)
   , Get  UserByIdPath (Json Text)                   -- GET /users/:id
   ]


-- ===================================================================
-- 2. Write handlers
-- ===================================================================

-- Health check: no extractors, returns plain text
healthHandler :: HandlerFn
healthHandler _parts _body = pure $ intoResponse ("ok" :: Text)

-- List users: returns JSON array
listUsersHandler :: HandlerFn
listUsersHandler _parts _body =
  pure $ intoResponse (Json (["alice", "bob", "charlie"] :: [Text]))

-- Get user by ID: reads capture from extensions
getUserHandler :: HandlerFn
getUserHandler parts _body = do
  mCaps <- lookupExtension @CaptureList (rpExtensions parts)
  case mCaps of
    Just (CaptureList (idText : _)) ->
      case parseCapture @Int idText of
        Just n  -> pure $ intoResponse (Json (T.pack ("user-" ++ show n)))
        Nothing -> pure $ intoResponse (mkError status400 "invalid user ID")
    _ -> pure $ intoResponse (mkError status500 "missing path capture")


-- ===================================================================
-- 3. Build server with typed effect tracking
-- ===================================================================

-- Wire handlers using mkServer (compile-time checked: 3 endpoints, 3 handlers)
apiService :: Service IO (Request ByteString) (Response ByteString)
apiService = run
  $ provide @Auth authMiddleware
  $ effectfulServer @MyAPI
      ( wrapHandler @(Get HealthPath Text) healthHandler
      , wrapHandler @(Requires Auth (Get UsersPath (Json [Text]))) listUsersHandler
      , wrapHandler @(Get UserByIdPath (Json Text)) getUserHandler
      )

-- A placeholder auth middleware (in real code, verify JWT/session)
authMiddleware :: Middleware IO (Request ByteString) (Response ByteString)
authMiddleware = before $ \_ -> pure ()  -- no-op for this example


-- ===================================================================
-- 4. Stack tower middleware
-- ===================================================================

app :: IO (Service IO (Request ByteString) (Response ByteString))
app = do
  ridLayer <- requestIdLayer
  let traceFn entry = BS8.putStrLn $
        traceMethod entry <> " " <> tracePath entry
        <> " -> " <> BS8.pack (show (traceStatus entry))

  pure $ apiService
    |> ridLayer
    |> traceLayer traceFn
    |> corsLayer permissiveCors
    |> secureHeadersLayer defaultSecureHeaders


-- ===================================================================
-- 5. Run on tower-server (zero WAI dependency)
-- ===================================================================

main :: IO ()
main = do
  putStrLn "Starting servant-reimagined example on http://localhost:3000"
  putStrLn ""
  putStrLn "Endpoints:"
  putStrLn "  GET  /health      -> health check"
  putStrLn "  GET  /users       -> list users (auth required)"
  putStrLn "  GET  /users/:id   -> get user by ID"
  putStrLn ""
  putStrLn "Middleware stack:"
  putStrLn "  - Secure headers (OWASP)"
  putStrLn "  - CORS (permissive)"
  putStrLn "  - Request tracing"
  putStrLn "  - Request ID"
  putStrLn ""

  svc <- app
  runServerBS 3000 svc
