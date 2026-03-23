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
import Data.Text (Text)
import qualified Data.Text as T

import Servant.Reimagined.Prelude


-- ===================================================================
-- 1. Define the API as types (using path helpers)
-- ===================================================================

-- The API type: endpoints with effects
type MyAPI =
  '[ Get  (At "health")     Text                         -- GET /health (public)
   , Requires Auth (Get (At "users") (Json [Text]))      -- GET /users (needs auth)
   , Get  (Param "users" Int) (Json Text)                -- GET /users/:id
   ]


-- ===================================================================
-- 2. Write handlers (ergonomic style using ToHandler)
-- ===================================================================

-- Health check: no extractors, returns plain text
healthHandler :: IO Text
healthHandler = pure "ok"

-- List users: returns JSON array
listUsersHandler :: IO (Json [Text])
listUsersHandler = pure (Json ["alice", "bob", "charlie"])

-- Get user by ID: PathCapture extracts and parses the capture automatically
getUserHandler :: PathCapture Int -> IO (Json Text)
getUserHandler (PathCapture n) = pure (Json (T.pack ("user-" ++ show n)))


-- ===================================================================
-- 3. Build server with typed effect tracking
-- ===================================================================

-- Wire handlers positionally — no wrapHandler, no toHandler, no type annotations.
-- effectfulApi + provide + run tracks middleware effects at compile time.
apiService :: Service IO (Request ByteString) (Response ByteString)
apiService = run
  $ provide @Auth authMiddleware
  $ effectfulApi @MyAPI (healthHandler, listUsersHandler, getUserHandler)

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
