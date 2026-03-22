{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status400, status500)

import Tower
import Tower.Service (Service (..))
import Http.Core
import Servant.Reimagined.Core
import Servant.Reimagined.Server
import Servant.Reimagined.Test


-- ===================================================================
-- Test API and server
-- ===================================================================

type HealthPath   = '[ 'Lit "health" ]
type UsersPath    = '[ 'Lit "users" ]
type UserByIdPath = '[ 'Lit "users", 'Capture Int ]

type TestAPI =
  '[ Get HealthPath   Text
   , Get UsersPath    (Json [Text])
   , Get UserByIdPath (Json Text)
   ]

testServer :: Service IO (Request ByteString) (Response ByteString)
testServer = mkServer @TestAPI
  ( wrapHandler @(Get HealthPath Text)
      (mkHandler0 (pure ("ok" :: Text)))
  , wrapHandler @(Get UsersPath (Json [Text]))
      (mkHandler0 (pure (Json (["alice", "bob"] :: [Text]))))
  , wrapHandler @(Get UserByIdPath (Json Text))
      (\parts _body -> do
        mCaps <- lookupExtension @CaptureList (rpExtensions parts)
        case mCaps of
          Just (CaptureList (idT : _)) ->
            case parseCapture @Int idT of
              Just n  -> pure $ intoResponse (Json (T.pack ("user-" ++ show n)))
              Nothing -> pure $ intoResponse (mkError status400 "bad id")
          _ -> pure $ intoResponse (mkError status500 "no caps")
      )
  )


-- ===================================================================
-- Tests
-- ===================================================================

main :: IO ()
main = do
  putStrLn "servant-reimagined-test tests:"
  putStrLn ""

  putStrLn "get helper:"
  resp1 <- get testServer "/health"
  resp1 `shouldHaveStatus` 200
  resp1 `shouldHaveBody` "ok"
  putStrLn "  OK: GET /health -> 200, body ok"

  putStrLn ""
  putStrLn "JSON response:"
  resp2 <- get testServer "/users"
  resp2 `shouldHaveStatus` 200
  resp2 `shouldHaveJsonBody` (["alice", "bob"] :: [Text])
  putStrLn "  OK: GET /users -> 200, JSON body"

  putStrLn ""
  putStrLn "Path captures:"
  resp3 <- get testServer "/users/42"
  resp3 `shouldHaveStatus` 200
  resp3 `shouldHaveJsonBody` ("user-42" :: Text)
  putStrLn "  OK: GET /users/42 -> 200, user-42"

  putStrLn ""
  putStrLn "404:"
  resp4 <- get testServer "/nope"
  resp4 `shouldHaveStatus` 404
  putStrLn "  OK: GET /nope -> 404"

  putStrLn ""
  putStrLn "405:"
  resp5 <- request testServer "POST" "/health" [] ""
  resp5 `shouldHaveStatus` 405
  putStrLn "  OK: POST /health -> 405"

  putStrLn ""
  putStrLn "Middleware:"
  let svc = testServer |> middleware (\inner -> Service $ \req -> do
              resp <- runService inner req
              pure resp { responseHeaders = ("X-Test", "yes") : responseHeaders resp })
  resp6 <- get svc "/health"
  resp6 `shouldHaveStatus` 200
  resp6 `shouldHaveHeader` "X-Test" $ "yes"
  putStrLn "  OK: middleware header present"

  putStrLn ""
  putStrLn "All servant-reimagined-test tests passed."
