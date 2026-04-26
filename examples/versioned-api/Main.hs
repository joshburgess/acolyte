{-# LANGUAGE OverloadedStrings #-}
-- | API versioning with compile-time backward compatibility checking.
--
-- Demonstrates:
-- - Defining V1 and V2 of an API using type-level change operations
-- - ApplyChanges to compute the V2 API from V1 + changes
-- - BackwardCompatible constraint that compiles only when safe
-- - Requires Auth on V2 endpoints
-- - effectfulApi + provide for typed middleware tracking
--
-- What happens if you add a Removed change:
--
-- @
-- type V2BreakingChanges =
--   '[ 'Added    (Get (At "profiles") (Json Text))
--    , 'Removed  (Get (At "users") (Json Text))       -- BREAKING!
--    ]
-- @
--
-- Expected compile error:
--   API version change is not backward compatible.
--   The change set contains a 'Removed' endpoint.
--   Remove the 'Removed' change or use 'Deprecated' instead.
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

import Acolyte.Prelude


-- ===================================================================
-- 1. V1 API: two simple endpoints
-- ===================================================================

type V1 =
  '[ Get (At "users")  (Json Text)     -- GET /users
   , Get (Param "users" Int) (Json Text)  -- GET /users/:id
   ]


-- ===================================================================
-- 2. V2 changes: add, replace, deprecate (all backward compatible)
-- ===================================================================

type V2Changes =
  '[ 'Added    (Requires Auth (Get (At "profiles") (Json Text)))
   , 'Replaced (Get (Param "users" Int) (Json Text))
               (Requires Auth (Get (Param "users" Int) (Json Text)))
   , 'Deprecated (Get (At "users") (Json Text))
   ]


-- ===================================================================
-- 3. Compute V2 and assert backward compatibility
-- ===================================================================

-- ApplyChanges computes the new endpoint list:
--   Added    -> appends the new endpoint
--   Replaced -> removes old, appends new
--   Deprecated -> no change (endpoint stays)
type V2 = ApplyChanges V1 V2Changes

-- This compiles because V2Changes has no 'Removed entries.
-- If you add a 'Removed, the BackwardCompatible constraint fails.
type V2Safe = BackwardCompatible V2Changes


-- ===================================================================
-- 4. V2 handlers
-- ===================================================================

-- GET /users (deprecated but still works)
listUsersHandler :: IO (Json Text)
listUsersHandler = pure (Json "alice, bob, charlie (deprecated)")

-- GET /profiles (new in V2, requires Auth)
listProfilesHandler :: IO (Json Text)
listProfilesHandler = pure (Json "profile:alice, profile:bob")

-- GET /users/:id (replaced in V2, now requires Auth)
getUserHandler :: PathCapture Int -> IO (Json Text)
getUserHandler (PathCapture n) = pure (Json (T.pack ("user-v2-" ++ show n)))


-- ===================================================================
-- 5. Wire V2 with typed effect tracking
-- ===================================================================

-- The V2 type is:
--   '[ Get (At "users") (Json Text)                          -- deprecated, still served
--    , Requires Auth (Get (At "profiles") (Json Text))       -- new
--    , Requires Auth (Get (Param "users" Int) (Json Text))   -- replaced
--    ]
--
-- effectfulApi checks that every Requires Auth has a matching provide.
v2Service :: V2Safe => Service IO (Request ByteString) (Response ByteString)
v2Service = run
  $ provide @Auth authMiddleware
  $ effectfulApi @V2
    ( listUsersHandler
    , listProfilesHandler
    , getUserHandler
    )

authMiddleware :: Middleware IO (Request ByteString) (Response ByteString)
authMiddleware = before $ \_ -> pure ()  -- placeholder


-- ===================================================================
-- 6. Run
-- ===================================================================

main :: IO ()
main = do
  putStrLn "API versioning demo"
  putStrLn "==================="
  putStrLn ""
  putStrLn "V1 endpoints:"
  putStrLn "  GET /users       -> list users"
  putStrLn "  GET /users/:id   -> get user by ID"
  putStrLn ""
  putStrLn "V2 changes applied:"
  putStrLn "  Added:      GET /profiles  (requires Auth)"
  putStrLn "  Replaced:   GET /users/:id (now requires Auth)"
  putStrLn "  Deprecated: GET /users     (still served)"
  putStrLn ""
  putStrLn "BackwardCompatible: PASSED (no Removed changes)"
  putStrLn ""
  putStrLn "V2 endpoints (computed by ApplyChanges):"
  putStrLn "  GET /users       -> list users (deprecated)"
  putStrLn "  GET /profiles    -> list profiles (auth)"
  putStrLn "  GET /users/:id   -> get user v2 (auth)"
  putStrLn ""

  -- Demonstrate that the service compiles and runs
  putStrLn "Starting V2 server on http://localhost:3000"
  runServerBS 3000 v2Service

  -- ---------------------------------------------------------------
  -- BREAKING CHANGE: uncomment the following to see the compile error
  -- ---------------------------------------------------------------
  -- type V2BreakingChanges =
  --   '[ 'Added    (Get (At "profiles") (Json Text))
  --    , 'Removed  (Get (At "users") (Json Text))
  --    ]
  --
  -- type V2Breaking = ApplyChanges V1 V2BreakingChanges
  -- type V2BreakingSafe = BackwardCompatible V2BreakingChanges
  --
  -- brokenService :: V2BreakingSafe => Service IO (Request ByteString) (Response ByteString)
  -- brokenService = mkApi @V2Breaking (listProfilesHandler, getUserHandler)
  --
  -- Expected error:
  --   API version change is not backward compatible.
  --   The change set contains a 'Removed' endpoint.
  --   Remove the 'Removed' change or use 'Deprecated' instead.
