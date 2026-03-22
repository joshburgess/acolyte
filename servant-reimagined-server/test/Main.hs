{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Network.HTTP.Types

import Tower
import Tower.Service (Service (..))
import Http.Core
import Servant.Reimagined.Core
import Servant.Reimagined.Server


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- API definition
-- ===================================================================

type HealthPath   = '[ 'Lit "health" ]
type UsersPath    = '[ 'Lit "users" ]
type UserByIdPath = '[ 'Lit "users", 'Capture Int ]

type TestAPI =
  '[ Get HealthPath   Text
   , Get UsersPath    (Json [Text])
   , Get UserByIdPath (Json Text)
   ]


-- ===================================================================
-- Request builder
-- ===================================================================

mkReq :: ByteString -> [Text] -> ByteString -> ByteString -> IO (Request ByteString)
mkReq meth path rawPath body = do
  exts <- emptyExtensions
  pure Request
    { requestMethod     = meth
    , requestPathRaw    = rawPath
    , requestPath       = path
    , requestQuery      = []
    , requestHeaders    = [("Content-Type", "application/json")]
    , requestBody       = body
    , requestExtensions = exts
    }


-- ===================================================================
-- Test 1: mkServer with automatic wiring
-- ===================================================================

testMkServer :: IO ()
testMkServer = do
  let svc = mkServer @TestAPI
        ( wrapHandler @(Get HealthPath Text)
            (mkHandler0 (pure ("ok" :: Text)))
        , wrapHandler @(Get UsersPath (Json [Text]))
            (mkHandler0 (pure (Json (["alice", "bob"] :: [Text]))))
        , wrapHandler @(Get UserByIdPath (Json Text))
            (\parts body -> do
              mCaps <- lookupExtension @CaptureList (rpExtensions parts)
              case mCaps of
                Just (CaptureList (idT : _)) ->
                  case parseCapture @Int idT of
                    Just n  -> pure $ intoResponse (Json (T.pack ("user-" ++ show n)))
                    Nothing -> pure $ intoResponse (mkError status400 "bad id")
                _ -> pure $ intoResponse (mkError status500 "no captures")
            )
        )

  -- GET /health
  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "mkServer: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "mkServer: GET /health -> 'ok'" (responseBody resp1 == "ok")

  -- GET /users
  req2 <- mkReq "GET" ["users"] "/users" ""
  resp2 <- runService svc req2
  assert "mkServer: GET /users -> 200" (statusCode (responseStatus resp2) == 200)
  let decoded = Aeson.decode (LBS.fromStrict (responseBody resp2)) :: Maybe [Text]
  assert "mkServer: GET /users -> JSON" (decoded == Just ["alice", "bob"])

  -- GET /users/7
  req3 <- mkReq "GET" ["users", "7"] "/users/7" ""
  resp3 <- runService svc req3
  assert "mkServer: GET /users/7 -> 200" (statusCode (responseStatus resp3) == 200)
  let decoded3 = Aeson.decode (LBS.fromStrict (responseBody resp3)) :: Maybe Text
  assert "mkServer: GET /users/7 -> 'user-7'" (decoded3 == Just "user-7")

  -- 404
  req4 <- mkReq "GET" ["nope"] "/nope" ""
  resp4 <- runService svc req4
  assert "mkServer: GET /nope -> 404" (statusCode (responseStatus resp4) == 404)

  -- 405
  req5 <- mkReq "POST" ["health"] "/health" ""
  resp5 <- runService svc req5
  assert "mkServer: POST /health -> 405" (statusCode (responseStatus resp5) == 405)


-- ===================================================================
-- Test 2: mkServer composes with tower middleware
-- ===================================================================

testWithMiddleware :: IO ()
testWithMiddleware = do
  let svc = mkServer @TestAPI
        ( wrapHandler @(Get HealthPath Text)
            (mkHandler0 (pure ("ok" :: Text)))
        , wrapHandler @(Get UsersPath (Json [Text]))
            (mkHandler0 (pure (Json (["alice"] :: [Text]))))
        , wrapHandler @(Get UserByIdPath (Json Text))
            (mkHandler0 (pure (Json ("user" :: Text))))
        )

  -- Add a header via tower middleware
  let addHeader :: Middleware IO (Request ByteString) (Response ByteString)
      addHeader = middleware $ \inner -> Service $ \req -> do
        resp <- runService inner req
        pure resp { responseHeaders = ("X-Test", "present") : responseHeaders resp }

  let svc' = svc |> addHeader

  req <- mkReq "GET" ["health"] "/health" ""
  resp <- runService svc' req
  assert "middleware: handler still works" (responseBody resp == "ok")
  assert "middleware: header added" (lookup "X-Test" (responseHeaders resp) == Just "present")


-- ===================================================================
-- Test 3: EffectfulServer with effect tracking
-- ===================================================================

-- API with effects
type EffectAPI =
  '[ Requires Auth (Get UserByIdPath (Json Text))
   , Get HealthPath Text
   ]

testEffectfulServer :: IO ()
testEffectfulServer = do
  -- Build with all effects provided
  let authMw :: Middleware IO (Request ByteString) (Response ByteString)
      authMw = middleware $ \inner -> Service $ \req -> do
        resp <- runService inner req
        pure resp { responseHeaders = ("X-Auth", "checked") : responseHeaders resp }

  let svc = run
        $ provide @Auth authMw
        $ effectfulServer @EffectAPI
            ( wrapHandler @(Requires Auth (Get UserByIdPath (Json Text)))
                (\parts body -> do
                  mCaps <- lookupExtension @CaptureList (rpExtensions parts)
                  case mCaps of
                    Just (CaptureList (idT : _)) ->
                      pure $ intoResponse (Json ("user-" <> idT))
                    _ -> pure $ intoResponse (Json ("unknown" :: Text))
                )
            , wrapHandler @(Get HealthPath Text)
                (mkHandler0 (pure ("ok" :: Text)))
            )

  -- GET /health (no auth required)
  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "effectful: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "effectful: auth middleware ran" (lookup "X-Auth" (responseHeaders resp1) == Just "checked")

  -- GET /users/5 (auth required — verified at compile time)
  req2 <- mkReq "GET" ["users", "5"] "/users/5" ""
  resp2 <- runService svc req2
  assert "effectful: GET /users/5 -> 200" (statusCode (responseStatus resp2) == 200)


-- NEGATIVE TEST (uncomment to verify compile error):
-- "Missing middleware effect: Auth"
--
-- testMissingEffect :: IO ()
-- testMissingEffect = do
--   let svc = run  -- compile error: Auth not provided
--         $ effectfulServer @EffectAPI
--             ( wrapHandler @(Requires Auth (Get UserByIdPath (Json Text)))
--                 (mkHandler0 (pure (Json ("user" :: Text))))
--             , wrapHandler @(Get HealthPath Text)
--                 (mkHandler0 (pure ("ok" :: Text)))
--             )
--   pure ()


-- ===================================================================
-- Test 4: endpoint metadata reflection
-- ===================================================================

testMetadata :: IO ()
testMetadata = do
  assert "method GET" (endpointMethod @(Get HealthPath Text) == "GET")
  assert "method POST" (endpointMethod @(Post UsersPath (Json Text) (Json Text)) == "POST")
  assert "pattern /health" (endpointPattern @(Get HealthPath Text) == "/health")
  assert "pattern /users/{capture}" (endpointPattern @(Get UserByIdPath (Json Text)) == "/users/{capture}")


-- ===================================================================
-- Test 5: IntoResponse instances
-- ===================================================================

testResponses :: IO ()
testResponses = do
  let r1 = intoResponse ("hello" :: Text)
  assert "Text -> 200" (statusCode (responseStatus r1) == 200)

  let r2 = intoResponse (Json (42 :: Int))
  assert "Json Int -> 200" (statusCode (responseStatus r2) == 200)
  assert "Json Int -> json content-type" (lookup "Content-Type" (responseHeaders r2) == Just "application/json")

  let r3 = intoResponse (Left (mkError status403 "forbidden") :: Either ServerError (Json Int))
  assert "Left err -> 403" (statusCode (responseStatus r3) == 403)

  let r4 = intoResponse (jsonError status422 "bad input")
  assert "JsonError -> 422" (statusCode (responseStatus r4) == 422)


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "servant-reimagined-server tests:"
  putStrLn ""
  putStrLn "Endpoint metadata:"
  testMetadata
  putStrLn ""
  putStrLn "IntoResponse:"
  testResponses
  putStrLn ""
  putStrLn "mkServer (automatic wiring):"
  testMkServer
  putStrLn ""
  putStrLn "Tower middleware composition:"
  testWithMiddleware
  putStrLn ""
  putStrLn "EffectfulServer (typed middleware tracking):"
  testEffectfulServer
  putStrLn ""
  putStrLn "All servant-reimagined-server tests passed."
