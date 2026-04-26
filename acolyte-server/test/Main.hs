{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.IORef
import Network.HTTP.Types

import Network.HTTP.Types (status500)

import Tower
import Tower.Service (Service (..))
import Http.Core
import Acolyte.Core
import Acolyte.Server


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
-- Test 6: Sub-API composition via combineServer
-- ===================================================================

-- Two separate sub-APIs
type SubAPI1 = '[ Get HealthPath Text ]
type SubAPI2 = '[ Get UsersPath (Json [Text]), Get UserByIdPath (Json Text) ]

-- Combined type for OpenAPI / client (type-level only)
type CombinedAPI = SubAPI1 ++ SubAPI2

testCombineServer :: IO ()
testCombineServer = do
  let svc = combineServer2 @SubAPI1 @SubAPI2
        -- SubAPI1 handlers
        ( wrapHandler @(Get HealthPath Text)
            (mkHandler0 (pure ("ok" :: Text)))
        )
        -- SubAPI2 handlers
        ( wrapHandler @(Get UsersPath (Json [Text]))
            (mkHandler0 (pure (Json (["alice"] :: [Text]))))
        , wrapHandler @(Get UserByIdPath (Json Text))
            (\parts _body -> do
              mCaps <- lookupExtension @CaptureList (rpExtensions parts)
              case mCaps of
                Just (CaptureList (idT : _)) ->
                  pure $ intoResponse (Json ("user-" <> idT))
                _ -> pure $ intoResponse (mkError status500 "no caps")
            )
        )

  -- Test SubAPI1
  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "combined: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "combined: GET /health body" (responseBody resp1 == "ok")

  -- Test SubAPI2
  req2 <- mkReq "GET" ["users"] "/users" ""
  resp2 <- runService svc req2
  assert "combined: GET /users -> 200" (statusCode (responseStatus resp2) == 200)

  req3 <- mkReq "GET" ["users", "7"] "/users/7" ""
  resp3 <- runService svc req3
  assert "combined: GET /users/7 -> 200" (statusCode (responseStatus resp3) == 200)

  -- Test 404 still works
  req4 <- mkReq "GET" ["nope"] "/nope" ""
  resp4 <- runService svc req4
  assert "combined: GET /nope -> 404" (statusCode (responseStatus resp4) == 404)


-- ===================================================================
-- Test 7: Combined effectful server
-- ===================================================================

type EffectSubAPI1 = '[ Get HealthPath Text ]
type EffectSubAPI2 = '[ Requires Auth (Get UsersPath (Json [Text])) ]
type EffectFullAPI = EffectSubAPI1 ++ EffectSubAPI2

testCombinedEffects :: IO ()
testCombinedEffects = do
  let router = subRouter @EffectSubAPI2
                 ( wrapHandler @(Requires Auth (Get UsersPath (Json [Text])))
                     (mkHandler0 (pure (Json (["bob"] :: [Text]))))
                 )
               $ subRouter @EffectSubAPI1
                   ( wrapHandler @(Get HealthPath Text)
                       (mkHandler0 (pure ("ok" :: Text)))
                   )
               $ emptyRouter

      authMw :: Middleware IO (Request ByteString) (Response ByteString)
      authMw = before $ \_ -> pure ()

      svc = runCombined @EffectFullAPI
          $ provideEffect @Auth authMw
          $ combinedFromRouter @EffectFullAPI router

  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "combined+effects: GET /health -> 200" (statusCode (responseStatus resp1) == 200)

  req2 <- mkReq "GET" ["users"] "/users" ""
  resp2 <- runService svc req2
  assert "combined+effects: GET /users -> 200" (statusCode (responseStatus resp2) == 200)

  -- COMPILE-TIME CHECK: if we remove `provideEffect @Auth`, this won't compile
  -- because EffectFullAPI contains Requires Auth and runCombined checks AllEffectsProvided.


-- ===================================================================
-- Test 8: ToHandler ergonomic handlers
-- ===================================================================

-- Ergonomic handler: no arguments
healthErgo :: IO Text
healthErgo = pure "ok"

-- Ergonomic handler: PathCapture
getUserErgo :: PathCapture Int -> IO (Json Text)
getUserErgo (PathCapture n) = pure (Json (T.pack ("user-" ++ show n)))

type ErgoAPI =
  '[ Get HealthPath Text
   , Get UserByIdPath (Json Text)
   ]

testToHandler :: IO ()
testToHandler = do
  let svc = mkServer @ErgoAPI
        ( wrapHandler @(Get HealthPath Text) (toHandler healthErgo)
        , wrapHandler @(Get UserByIdPath (Json Text)) (toHandler getUserErgo)
        )

  -- GET /health via ergonomic handler
  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "toHandler: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "toHandler: GET /health -> 'ok'" (responseBody resp1 == "ok")

  -- GET /users/42 via PathCapture ergonomic handler
  req2 <- mkReq "GET" ["users", "42"] "/users/42" ""
  resp2 <- runService svc req2
  assert "toHandler: GET /users/42 -> 200" (statusCode (responseStatus resp2) == 200)
  let decoded2 = Aeson.decode (LBS.fromStrict (responseBody resp2)) :: Maybe Text
  assert "toHandler: GET /users/42 -> 'user-42'" (decoded2 == Just "user-42")

  -- GET /users/bad -> 400 (invalid capture)
  req3 <- mkReq "GET" ["users", "bad"] "/users/bad" ""
  resp3 <- runService svc req3
  assert "toHandler: GET /users/bad -> 400" (statusCode (responseStatus resp3) == 400)


-- ===================================================================
-- Test 8b: mkApi — ergonomic server construction (no wrapHandler)
-- ===================================================================

testMkApi :: IO ()
testMkApi = do
  -- mkApi @ErgoAPI directly accepts handler functions — no wrapHandler, no toHandler
  let svc = mkApi @ErgoAPI (healthErgo, getUserErgo)

  -- GET /health
  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "mkApi: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "mkApi: GET /health -> 'ok'" (responseBody resp1 == "ok")

  -- GET /users/42 via PathCapture
  req2 <- mkReq "GET" ["users", "42"] "/users/42" ""
  resp2 <- runService svc req2
  assert "mkApi: GET /users/42 -> 200" (statusCode (responseStatus resp2) == 200)
  let decoded2 = Aeson.decode (LBS.fromStrict (responseBody resp2)) :: Maybe Text
  assert "mkApi: GET /users/42 -> 'user-42'" (decoded2 == Just "user-42")

  -- GET /users/bad -> 400 (invalid capture)
  req3 <- mkReq "GET" ["users", "bad"] "/users/bad" ""
  resp3 <- runService svc req3
  assert "mkApi: GET /users/bad -> 400" (statusCode (responseStatus resp3) == 400)

  -- 404
  req4 <- mkReq "GET" ["nope"] "/nope" ""
  resp4 <- runService svc req4
  assert "mkApi: GET /nope -> 404" (statusCode (responseStatus resp4) == 404)

  -- 405
  req5 <- mkReq "POST" ["health"] "/health" ""
  resp5 <- runService svc req5
  assert "mkApi: POST /health -> 405" (statusCode (responseStatus resp5) == 405)


-- ===================================================================
-- Test 9: PathCapture FromRequestParts
-- ===================================================================

testPathCapture :: IO ()
testPathCapture = do
  -- Build a request with CaptureList in extensions
  exts <- emptyExtensions
  insertExtension (CaptureList ["99"]) exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/users/99"
        , rpPath       = ["users", "99"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }

  -- Extract PathCapture Int
  result <- fromRequestParts @(PathCapture Int) parts
  case result of
    Right (PathCapture n) -> assert "PathCapture: parses Int from CaptureList" (n == 99)
    Left _                -> assert "PathCapture: parses Int from CaptureList" False

  -- Extract with bad data
  exts2 <- emptyExtensions
  insertExtension (CaptureList ["notanumber"]) exts2
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @(PathCapture Int) parts2
  case result2 of
    Left _  -> assert "PathCapture: bad parse -> Left" True
    Right _ -> assert "PathCapture: bad parse -> Left" False

  -- Extract with no CaptureList
  exts3 <- emptyExtensions
  let parts3 = parts { rpExtensions = exts3 }
  result3 <- fromRequestParts @(PathCapture Int) parts3
  case result3 of
    Left _  -> assert "PathCapture: missing CaptureList -> Left" True
    Right _ -> assert "PathCapture: missing CaptureList -> Left" False


-- ===================================================================
-- Test 10: QueryParam extraction
-- ===================================================================

type SearchPath = '[ 'Lit "search" ]
type SearchAPI = '[ Get SearchPath (Json Text) ]

-- Ergonomic handler with QueryParam
searchHandler :: QueryParam "page" Int -> IO (Json Text)
searchHandler (QueryParam page) = pure (Json (T.pack ("page-" ++ show page)))

mkReqWithQuery :: ByteString -> [Text] -> ByteString -> ByteString
              -> [(ByteString, Maybe ByteString)] -> IO (Request ByteString)
mkReqWithQuery meth path rawPath body qs = do
  exts <- emptyExtensions
  pure Request
    { requestMethod     = meth
    , requestPathRaw    = rawPath
    , requestPath       = path
    , requestQuery      = qs
    , requestHeaders    = [("Content-Type", "application/json")]
    , requestBody       = body
    , requestExtensions = exts
    }

testQueryParam :: IO ()
testQueryParam = do
  let svc = mkServer @SearchAPI
        ( wrapHandler @(Get SearchPath (Json Text)) (toHandler searchHandler)
        )

  -- GET /search?page=5 -> success
  req1 <- mkReqWithQuery "GET" ["search"] "/search" "" [("page", Just "5")]
  resp1 <- runService svc req1
  assert "QueryParam: GET /search?page=5 -> 200" (statusCode (responseStatus resp1) == 200)
  let decoded1 = Aeson.decode (LBS.fromStrict (responseBody resp1)) :: Maybe Text
  assert "QueryParam: GET /search?page=5 -> 'page-5'" (decoded1 == Just "page-5")

  -- GET /search (missing page) -> 400
  req2 <- mkReqWithQuery "GET" ["search"] "/search" "" []
  resp2 <- runService svc req2
  assert "QueryParam: GET /search (missing param) -> 400" (statusCode (responseStatus resp2) == 400)

  -- GET /search?page=abc (invalid) -> 400
  req3 <- mkReqWithQuery "GET" ["search"] "/search" "" [("page", Just "abc")]
  resp3 <- runService svc req3
  assert "QueryParam: GET /search?page=abc -> 400" (statusCode (responseStatus resp3) == 400)


-- ===================================================================
-- Test 11: JsonBody via FromRequestParts (ergonomic handler)
-- ===================================================================

data CreateUser = CreateUser
  { userName :: Text
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON CreateUser
instance Aeson.ToJSON CreateUser

type CreateUserPath = '[ 'Lit "users" ]
type CreateUserAPI = '[ Post CreateUserPath (Json CreateUser) (Json Text) ]

createUserHandler :: JsonBody CreateUser -> IO (Json Text)
createUserHandler (JsonBody cu) = pure (Json ("created: " <> userName cu))

testJsonBody :: IO ()
testJsonBody = do
  let svc = mkServer @CreateUserAPI
        ( wrapHandler @(Post CreateUserPath (Json CreateUser) (Json Text))
            (toHandler createUserHandler)
        )

  -- POST /users with valid JSON
  let body1 = LBS.toStrict (Aeson.encode (CreateUser "alice"))
  req1 <- mkReq "POST" ["users"] "/users" body1
  resp1 <- runService svc req1
  assert "JsonBody: POST /users -> 200" (statusCode (responseStatus resp1) == 200)
  let decoded1 = Aeson.decode (LBS.fromStrict (responseBody resp1)) :: Maybe Text
  assert "JsonBody: POST /users -> 'created: alice'" (decoded1 == Just "created: alice")

  -- POST /users with invalid JSON
  req2 <- mkReq "POST" ["users"] "/users" "not json"
  resp2 <- runService svc req2
  assert "JsonBody: POST /users (bad JSON) -> 422" (statusCode (responseStatus resp2) == 422)

  -- POST /users with empty body
  req3 <- mkReq "POST" ["users"] "/users" ""
  resp3 <- runService svc req3
  assert "JsonBody: POST /users (empty body) -> 422" (statusCode (responseStatus resp3) == 422)


-- ===================================================================
-- Test 12: HeaderMap extraction
-- ===================================================================

testHeaderMap :: IO ()
testHeaderMap = do
  exts <- emptyExtensions
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = [("Content-Type", "application/json"), ("X-Custom", "hello")]
        , rpExtensions = exts
        }
  result <- fromRequestParts @HeaderMap parts
  case result of
    Right (HeaderMap hdrs) -> do
      assert "HeaderMap: has Content-Type" (lookup "Content-Type" hdrs == Just "application/json")
      assert "HeaderMap: has X-Custom" (lookup "X-Custom" hdrs == Just "hello")
      assert "HeaderMap: correct count" (length hdrs == 2)
    Left _ -> assert "HeaderMap: extraction succeeded" False


-- ===================================================================
-- Test 13: FullRequest extraction
-- ===================================================================

testFullRequest :: IO ()
testFullRequest = do
  exts <- emptyExtensions
  let parts = RequestParts
        { rpMethod     = "POST"
        , rpPathRaw    = "/api/users"
        , rpPath       = ["api", "users"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @FullRequest parts
  case result of
    Right (FullRequest fp) -> do
      assert "FullRequest: correct method" (rpMethod fp == "POST")
      assert "FullRequest: correct path" (rpPath fp == ["api", "users"])
      assert "FullRequest: correct rawPath" (rpPathRaw fp == "/api/users")
    Left _ -> assert "FullRequest: extraction succeeded" False


-- ===================================================================
-- Test 14: RawQuery extraction
-- ===================================================================

testRawQuery :: IO ()
testRawQuery = do
  exts <- emptyExtensions
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/foo?bar=baz"
        , rpPath       = ["foo"]
        , rpQuery      = [("bar", Just "baz")]
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @RawQuery parts
  case result of
    Right (RawQuery q) -> assert "RawQuery: extracts query string" (q == "bar=baz")
    Left _ -> assert "RawQuery: extraction succeeded" False

  -- Test with no query string
  let parts2 = parts { rpPathRaw = "/foo" }
  result2 <- fromRequestParts @RawQuery parts2
  case result2 of
    Right (RawQuery q2) -> assert "RawQuery: empty when no '?'" (BS.null q2)
    Left _ -> assert "RawQuery: extraction succeeded (no query)" False


-- ===================================================================
-- Test 15: RawPathParams extraction
-- ===================================================================

testRawPathParams :: IO ()
testRawPathParams = do
  exts <- emptyExtensions
  insertExtension (CaptureList ["42", "hello"]) exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/users/42/hello"
        , rpPath       = ["users", "42", "hello"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @RawPathParams parts
  case result of
    Right (RawPathParams segs) -> do
      assert "RawPathParams: correct segments" (segs == ["42", "hello"])
      assert "RawPathParams: correct count" (length segs == 2)
    Left _ -> assert "RawPathParams: extraction succeeded" False

  -- Test with no CaptureList -> empty list
  exts2 <- emptyExtensions
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @RawPathParams parts2
  case result2 of
    Right (RawPathParams segs2) -> assert "RawPathParams: empty when no CaptureList" (null segs2)
    Left _ -> assert "RawPathParams: extraction succeeded (empty)" False


-- ===================================================================
-- Test 16: StringBody extraction
-- ===================================================================

testStringBody :: IO ()
testStringBody = do
  exts <- emptyExtensions
  insertExtension (BodyBytes "hello world") exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @StringBody parts
  case result of
    Right (StringBody txt) -> assert "StringBody: correct text" (txt == "hello world")
    Left _ -> assert "StringBody: extraction succeeded" False

  -- Test with no BodyBytes -> Left
  exts2 <- emptyExtensions
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @StringBody parts2
  case result2 of
    Left _  -> assert "StringBody: Left when no BodyBytes" True
    Right _ -> assert "StringBody: Left when no BodyBytes" False


-- ===================================================================
-- Test 17: MatchedPath extraction
-- ===================================================================

testMatchedPathExtract :: IO ()
testMatchedPathExtract = do
  exts <- emptyExtensions
  insertExtension (MatchedPath "/users/{capture}") exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/users/42"
        , rpPath       = ["users", "42"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @MatchedPath parts
  case result of
    Right (MatchedPath pat) -> assert "MatchedPath: correct pattern" (pat == "/users/{capture}")
    Left _ -> assert "MatchedPath: extraction succeeded" False

  -- Test without MatchedPath in extensions -> Left
  exts2 <- emptyExtensions
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @MatchedPath parts2
  case result2 of
    Left _  -> assert "MatchedPath: Left when not set" True
    Right _ -> assert "MatchedPath: Left when not set" False


-- ===================================================================
-- Test 18: OriginalUri extraction
-- ===================================================================

testOriginalUri :: IO ()
testOriginalUri = do
  -- With OriginalUri stored in extensions
  exts <- emptyExtensions
  insertExtension (OriginalUri "/api/v1/users?page=1") exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/users"
        , rpPath       = ["users"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @OriginalUri parts
  case result of
    Right (OriginalUri uri) -> assert "OriginalUri: from extensions" (uri == "/api/v1/users?page=1")
    Left _ -> assert "OriginalUri: extraction succeeded" False

  -- Without OriginalUri in extensions -> falls back to rpPathRaw
  exts2 <- emptyExtensions
  let parts2 = parts { rpExtensions = exts2, rpPathRaw = "/fallback/path" }
  result2 <- fromRequestParts @OriginalUri parts2
  case result2 of
    Right (OriginalUri uri2) -> assert "OriginalUri: fallback to rpPathRaw" (uri2 == "/fallback/path")
    Left _ -> assert "OriginalUri: fallback extraction succeeded" False


-- ===================================================================
-- Test 19: NestedPath extraction
-- ===================================================================

testNestedPath :: IO ()
testNestedPath = do
  -- Without NestedPath in extensions -> empty list
  exts <- emptyExtensions
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @NestedPath parts
  case result of
    Right (NestedPath segs) -> assert "NestedPath: empty when not set" (null segs)
    Left _ -> assert "NestedPath: extraction succeeded" False

  -- With NestedPath stored
  exts2 <- emptyExtensions
  insertExtension (NestedPath ["api", "v1"]) exts2
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @NestedPath parts2
  case result2 of
    Right (NestedPath segs2) -> assert "NestedPath: correct prefix" (segs2 == ["api", "v1"])
    Left _ -> assert "NestedPath: extraction with value succeeded" False


-- ===================================================================
-- Test 20: RawForm extraction
-- ===================================================================

testRawForm :: IO ()
testRawForm = do
  exts <- emptyExtensions
  insertExtension (BodyBytes "user=alice&age=30") exts
  let parts = RequestParts
        { rpMethod     = "POST"
        , rpPathRaw    = "/login"
        , rpPath       = ["login"]
        , rpQuery      = []
        , rpHeaders    = [("Content-Type", "application/x-www-form-urlencoded")]
        , rpExtensions = exts
        }
  result <- fromRequestParts @RawForm parts
  case result of
    Right (RawForm pairs) -> do
      assert "RawForm: has user field" (lookup "user" pairs == Just "alice")
      assert "RawForm: has age field" (lookup "age" pairs == Just "30")
      assert "RawForm: correct pair count" (length pairs == 2)
    Left _ -> assert "RawForm: extraction succeeded" False


-- ===================================================================
-- Test 21: Form extraction with FromForm instance
-- ===================================================================

data SimpleForm = SimpleForm Text Int
  deriving (Show, Eq)

instance FromForm SimpleForm where
  fromForm pairs = case (lookup "user" pairs, lookup "age" pairs) of
    (Just u, Just a) -> case parseCapture (TE.decodeUtf8 a) of
      Just n  -> Right (SimpleForm (TE.decodeUtf8 u) n)
      Nothing -> Left "invalid age"
    _ -> Left "missing fields"

testForm :: IO ()
testForm = do
  exts <- emptyExtensions
  insertExtension (BodyBytes "user=alice&age=30") exts
  let parts = RequestParts
        { rpMethod     = "POST"
        , rpPathRaw    = "/register"
        , rpPath       = ["register"]
        , rpQuery      = []
        , rpHeaders    = [("Content-Type", "application/x-www-form-urlencoded")]
        , rpExtensions = exts
        }
  result <- fromRequestParts @(Form SimpleForm) parts
  case result of
    Right (Form (SimpleForm user age)) -> do
      assert "Form: correct user" (user == "alice")
      assert "Form: correct age" (age == 30)
    Left _ -> assert "Form: extraction succeeded" False

  -- Invalid form data
  exts2 <- emptyExtensions
  insertExtension (BodyBytes "user=alice&age=notanumber") exts2
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @(Form SimpleForm) parts2
  case result2 of
    Left _  -> assert "Form: Left on invalid data" True
    Right _ -> assert "Form: Left on invalid data" False

  -- Missing fields
  exts3 <- emptyExtensions
  insertExtension (BodyBytes "user=alice") exts3
  let parts3 = parts { rpExtensions = exts3 }
  result3 <- fromRequestParts @(Form SimpleForm) parts3
  case result3 of
    Left _  -> assert "Form: Left on missing fields" True
    Right _ -> assert "Form: Left on missing fields" False


-- ===================================================================
-- Test 22: Multipart extraction
-- ===================================================================

testMultipart :: IO ()
testMultipart = do
  exts <- emptyExtensions
  let multipartBody = BS.concat
        [ "--boundary123\r\n"
        , "Content-Disposition: form-data; name=\"file\"; filename=\"test.txt\"\r\n"
        , "Content-Type: text/plain\r\n"
        , "\r\n"
        , "hello world\r\n"
        , "--boundary123--"
        ]
  insertExtension (BodyBytes multipartBody) exts
  let parts = RequestParts
        { rpMethod     = "POST"
        , rpPathRaw    = "/upload"
        , rpPath       = ["upload"]
        , rpQuery      = []
        , rpHeaders    = [(CI.mk "content-type", "multipart/form-data; boundary=boundary123")]
        , rpExtensions = exts
        }
  result <- fromRequestParts @Multipart parts
  case result of
    Right (Multipart fileParts) -> do
      assert "Multipart: has one part" (length fileParts == 1)
      case fileParts of
        (fp : _) -> do
          assert "Multipart: field name is 'file'" (fpFieldName fp == "file")
          assert "Multipart: filename is 'test.txt'" (fpFileName fp == Just "test.txt")
          assert "Multipart: body is 'hello world'" (fpBody fp == "hello world")
        [] -> assert "Multipart: has parts" False
    Left err -> do
      assert ("Multipart: extraction succeeded (got: " ++ show (seMessage err) ++ ")") False


-- ===================================================================
-- Test 23: ConnectInfo extractor
-- ===================================================================

testConnectInfo :: IO ()
testConnectInfo = do
  -- With ConnectInfo in extensions
  exts <- emptyExtensions
  insertExtension (ConnectInfo "127.0.0.1" 8080) exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @ConnectInfo parts
  case result of
    Right ci -> do
      assert "ConnectInfo: host is 127.0.0.1" (ciHost ci == "127.0.0.1")
      assert "ConnectInfo: port is 8080" (ciPort ci == 8080)
    Left _ -> assert "ConnectInfo: extraction succeeded" False

  -- Without ConnectInfo -> Left
  exts2 <- emptyExtensions
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @ConnectInfo parts2
  case result2 of
    Left _  -> assert "ConnectInfo: Left when missing" True
    Right _ -> assert "ConnectInfo: Left when missing" False


-- ===================================================================
-- Test 24: ReqMethod extractor
-- ===================================================================

testReqMethod :: IO ()
testReqMethod = do
  exts <- emptyExtensions
  let parts = RequestParts
        { rpMethod     = "POST"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @ReqMethod parts
  case result of
    Right (ReqMethod m) -> assert "ReqMethod: is POST" (m == "POST")
    Left _ -> assert "ReqMethod: extraction succeeded" False


-- ===================================================================
-- Test 25: Extension extractor
-- ===================================================================

newtype MyExt = MyExt Int deriving (Show, Eq, Typeable)

testExtension :: IO ()
testExtension = do
  -- With Extension present
  exts <- emptyExtensions
  insertExtension (MyExt 42) exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @(Extension MyExt) parts
  case result of
    Right (Extension (MyExt n)) -> assert "Extension MyExt: value is 42" (n == 42)
    Left _ -> assert "Extension MyExt: extraction succeeded" False

  -- Without Extension -> Left
  exts2 <- emptyExtensions
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @(Extension MyExt) parts2
  case result2 of
    Left _  -> assert "Extension MyExt: Left when missing" True
    Right _ -> assert "Extension MyExt: Left when missing" False


-- ===================================================================
-- Test 26: Optional extractor
-- ===================================================================

testOptional :: IO ()
testOptional = do
  -- Without Authorization header -> Optional Nothing
  exts <- emptyExtensions
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @(Optional (ReqHeader "Authorization")) parts
  case result of
    Right (Optional Nothing) -> assert "Optional: Nothing when header missing" True
    Right (Optional (Just _)) -> assert "Optional: Nothing when header missing" False
    Left _ -> assert "Optional: extraction succeeded" False

  -- With Authorization header -> Optional (Just ...)
  let parts2 = parts { rpHeaders = [("Authorization", "Bearer token123")] }
  result2 <- fromRequestParts @(Optional (ReqHeader "Authorization")) parts2
  case result2 of
    Right (Optional (Just (ReqHeader val))) ->
      assert "Optional: Just when header present" (val == "Bearer token123")
    Right (Optional Nothing) -> assert "Optional: Just when header present" False
    Left _ -> assert "Optional: extraction succeeded" False


-- ===================================================================
-- Test 27: AppState extractor
-- ===================================================================

testAppState :: IO ()
testAppState = do
  -- With AppState in extensions
  exts <- emptyExtensions
  insertExtension (AppState (42 :: Int)) exts
  let parts = RequestParts
        { rpMethod     = "GET"
        , rpPathRaw    = "/test"
        , rpPath       = ["test"]
        , rpQuery      = []
        , rpHeaders    = []
        , rpExtensions = exts
        }
  result <- fromRequestParts @(AppState Int) parts
  case result of
    Right (AppState n) -> assert "AppState: value is 42" (n == 42)
    Left _ -> assert "AppState: extraction succeeded" False

  -- Without AppState -> Left
  exts2 <- emptyExtensions
  let parts2 = parts { rpExtensions = exts2 }
  result2 <- fromRequestParts @(AppState Int) parts2
  case result2 of
    Left _  -> assert "AppState: Left when missing" True
    Right _ -> assert "AppState: Left when missing" False


-- ===================================================================
-- Test 28: ToHandler with two extractors
-- ===================================================================

type TwoExtPath = '[ 'Lit "items", 'Capture Int ]
type TwoExtAPI  = '[ Get TwoExtPath (Json Text) ]

twoArgHandler :: PathCapture Int -> QueryParam "name" Text -> IO (Json Text)
twoArgHandler (PathCapture n) (QueryParam name) =
  pure (Json (T.pack (show n) <> "-" <> name))

testToHandlerTwoArgs :: IO ()
testToHandlerTwoArgs = do
  let svc = mkServer @TwoExtAPI
        ( wrapHandler @(Get TwoExtPath (Json Text)) (toHandler twoArgHandler)
        )

  req <- mkReqWithQuery "GET" ["items", "7"] "/items/7" "" [("name", Just "widget")]
  resp <- runService svc req
  assert "ToHandler 2-arg: status 200" (statusCode (responseStatus resp) == 200)
  let decoded = Aeson.decode (LBS.fromStrict (responseBody resp)) :: Maybe Text
  assert "ToHandler 2-arg: correct body" (decoded == Just "7-widget")


-- ===================================================================
-- Test 29: ToHandler with body + parts
-- ===================================================================

type BodyPartsPath = '[ 'Lit "items", 'Capture Int ]
type BodyPartsAPI  = '[ Post BodyPartsPath (Json CreateUser) (Json Text) ]

bodyAndPartsHandler :: PathCapture Int -> JsonBody CreateUser -> IO (Json Text)
bodyAndPartsHandler (PathCapture n) (JsonBody cu) =
  pure (Json (T.pack (show n) <> "-" <> userName cu))

testToHandlerBodyParts :: IO ()
testToHandlerBodyParts = do
  let svc = mkServer @BodyPartsAPI
        ( wrapHandler @(Post BodyPartsPath (Json CreateUser) (Json Text))
            (toHandler bodyAndPartsHandler)
        )

  let body = LBS.toStrict (Aeson.encode (CreateUser "alice"))
  req <- mkReq "POST" ["items", "5"] "/items/5" body
  resp <- runService svc req
  assert "ToHandler body+parts: status 200" (statusCode (responseStatus resp) == 200)
  let decoded = Aeson.decode (LBS.fromStrict (responseBody resp)) :: Maybe Text
  assert "ToHandler body+parts: correct body" (decoded == Just "5-alice")


-- ===================================================================
-- Test 30: ToHandler error propagation
-- ===================================================================

testToHandlerErrorProp :: IO ()
testToHandlerErrorProp = do
  -- Reuse the ergonomic handler from Test 8 that requires PathCapture Int.
  -- Send a bad capture value to trigger extractor failure.
  handlerRan <- newIORef False
  let badHandler :: PathCapture Int -> IO (Json Text)
      badHandler (PathCapture n) = do
        writeIORef handlerRan True
        pure (Json (T.pack (show n)))

  let svc = mkServer @ErgoAPI
        ( wrapHandler @(Get HealthPath Text) (toHandler healthErgo)
        , wrapHandler @(Get UserByIdPath (Json Text)) (toHandler badHandler)
        )

  -- GET /users/notanumber -> bad capture -> 400
  req <- mkReq "GET" ["users", "notanumber"] "/users/notanumber" ""
  resp <- runService svc req
  assert "ToHandler error prop: status 400" (statusCode (responseStatus resp) == 400)
  ran <- readIORef handlerRan
  assert "ToHandler error prop: handler did not run" (not ran)


-- ===================================================================
-- Test: Named routes (record-based handler registration)
-- ===================================================================

-- | A named handler record for ErgoAPI.
data NamedHandlers = NamedHandlers
  { nhHealth  :: IO Text
  , nhGetUser :: PathCapture Int -> IO (Json Text)
  }

instance NamedApi ErgoAPI NamedHandlers (IO Text, PathCapture Int -> IO (Json Text)) where
  toHandlerTuple h = (nhHealth h, nhGetUser h)

testNamedRoutes :: IO ()
testNamedRoutes = do
  let svc = mkNamedApi @ErgoAPI NamedHandlers
        { nhHealth  = pure "ok"
        , nhGetUser = \(PathCapture n) -> pure (Json (T.pack ("user-" ++ show n)))
        }

  -- GET /health
  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "named: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "named: GET /health -> 'ok'" (responseBody resp1 == "ok")

  -- GET /users/42 via PathCapture
  req2 <- mkReq "GET" ["users", "42"] "/users/42" ""
  resp2 <- runService svc req2
  assert "named: GET /users/42 -> 200" (statusCode (responseStatus resp2) == 200)
  let decoded2 = Aeson.decode (LBS.fromStrict (responseBody resp2)) :: Maybe Text
  assert "named: GET /users/42 -> 'user-42'" (decoded2 == Just "user-42")

  -- 404
  req3 <- mkReq "GET" ["nope"] "/nope" ""
  resp3 <- runService svc req3
  assert "named: GET /nope -> 404" (statusCode (responseStatus resp3) == 404)

  -- 405
  req4 <- mkReq "POST" ["health"] "/health" ""
  resp4 <- runService svc req4
  assert "named: POST /health -> 405" (statusCode (responseStatus resp4) == 405)


-- ===================================================================
-- Test: Content negotiation — parseAccept
-- ===================================================================

testParseAccept :: IO ()
testParseAccept = do
  -- Basic parsing with quality values
  let parsed1 = parseAccept "application/json, text/plain;q=0.9, application/xml;q=0.8"
  assert "parseAccept: 3 entries" (length parsed1 == 3)
  assert "parseAccept: json first (q=1.0)" (fst (head parsed1) == "application/json")
  assert "parseAccept: json quality 1.0" (snd (head parsed1) == 1.0)
  assert "parseAccept: text/plain q=0.9" (snd (parsed1 !! 1) == 0.9)
  assert "parseAccept: application/xml q=0.8" (snd (parsed1 !! 2) == 0.8)

  -- Missing quality defaults to 1.0
  let parsed2 = parseAccept "text/html"
  assert "parseAccept: missing q defaults to 1.0" (parsed2 == [("text/html", 1.0)])

  -- Empty header
  let parsed3 = parseAccept ""
  assert "parseAccept: empty header -> []" (null parsed3)

  -- Wildcard
  let parsed4 = parseAccept "*/*"
  assert "parseAccept: wildcard" (parsed4 == [("*/*", 1.0)])

  -- Quality ordering: lower q comes later
  let parsed5 = parseAccept "text/plain;q=0.5, application/json;q=0.9"
  assert "parseAccept: sorted by quality desc" (fst (head parsed5) == "application/json")


-- ===================================================================
-- Test: Content negotiation — matchFormat
-- ===================================================================

testMatchFormat :: IO ()
testMatchFormat = do
  let jsonEnc :: Text -> ByteString
      jsonEnc = TE.encodeUtf8

      xmlEnc :: Text -> ByteString
      xmlEnc t = TE.encodeUtf8 ("<v>" <> t <> "</v>")

      formats :: [(Text, Text -> ByteString)]
      formats = [("application/json", jsonEnc), ("application/xml", xmlEnc)]

  -- Picks highest quality match
  case matchFormat "application/json, application/xml;q=0.8" formats of
    Just (ct, enc) -> do
      assert "matchFormat: picks json" (ct == "application/json")
      assert "matchFormat: encodes with json encoder" (enc "hello" == "hello")
    Nothing -> assert "matchFormat: picks json" False

  -- Picks xml when json not available
  let xmlOnly = [("application/xml", xmlEnc)]
  case matchFormat "application/json, application/xml;q=0.8" xmlOnly of
    Just (ct, _) -> assert "matchFormat: falls back to xml" (ct == "application/xml")
    Nothing      -> assert "matchFormat: falls back to xml" False

  -- Falls back to first available when no Accept match
  case matchFormat "text/csv" formats of
    Nothing -> assert "matchFormat: no match -> Nothing" True
    Just _  -> assert "matchFormat: no match -> Nothing" False

  -- Wildcard */* matches first available
  case matchFormat "*/*" formats of
    Just (ct, _) -> assert "matchFormat: wildcard picks first" (ct == "application/json")
    Nothing      -> assert "matchFormat: wildcard picks first" False

  -- Empty Accept header -> first available
  case matchFormat "" formats of
    Just (ct, _) -> assert "matchFormat: empty accept -> first" (ct == "application/json")
    Nothing      -> assert "matchFormat: empty accept -> first" False


-- ===================================================================
-- Test: Content negotiation — negotiate
-- ===================================================================

testNegotiate :: IO ()
testNegotiate = do
  let jsonEnc :: Text -> ByteString
      jsonEnc = TE.encodeUtf8

      xmlEnc :: Text -> ByteString
      xmlEnc t = TE.encodeUtf8 ("<v>" <> t <> "</v>")

      formats :: [(Text, Text -> ByteString)]
      formats = [("application/json", jsonEnc), ("application/xml", xmlEnc)]

  -- Successful negotiation
  let resp1 = negotiate "application/json" formats ("hello" :: Text)
  assert "negotiate: 200 for json match" (responseStatus resp1 == status200)
  assert "negotiate: body is json-encoded" (responseBody resp1 == "hello")
  assert "negotiate: Content-Type set"
    (lookup "Content-Type" (responseHeaders resp1) == Just "application/json")

  -- 406 when no match
  let resp2 = negotiate "text/csv" formats ("hello" :: Text)
  assert "negotiate: 406 for no match" (responseStatus resp2 == status406)


-- ===================================================================
-- Test: Path helpers compile-time checks
-- ===================================================================

-- These are compile-time tests: if they compile, the type equalities hold.
-- At runtime we just confirm they're reachable.

-- At "x" ~ '[ 'Lit "x" ]
type AtX = At "x"
type AtXExpected = '[ 'Lit "x" ]

-- Param "x" Int ~ '[ 'Lit "x", 'Capture Int ]
type ParamXInt = Param "x" Int
type ParamXIntExpected = '[ 'Lit "x", 'Capture Int ]

-- At2 "a" "b" ~ '[ 'Lit "a", 'Lit "b" ]
type At2AB = At2 "a" "b"
type At2ABExpected = '[ 'Lit "a", 'Lit "b" ]

-- The GHC constraint trick: if these don't hold, we get a compile error.
testPathHelpers :: (AtX ~ AtXExpected, ParamXInt ~ ParamXIntExpected, At2AB ~ At2ABExpected) => IO ()
testPathHelpers = do
  -- If we reach here, all three type equalities compiled successfully.
  assert "At \"x\" ~ '[ 'Lit \"x\" ]" True
  assert "Param \"x\" Int ~ '[ 'Lit \"x\", 'Capture Int ]" True
  assert "At2 \"a\" \"b\" ~ '[ 'Lit \"a\", 'Lit \"b\" ]" True

  -- Additional: verify At/Param/At2 work in endpoint definitions
  let meth = endpointMethod @(Get (At "health") Text)
  assert "At in endpoint: method GET" (meth == "GET")
  let pat = endpointPattern @(Get (At "health") Text)
  assert "At in endpoint: pattern /health" (pat == "/health")

  let pat2 = endpointPattern @(Get (Param "users" Int) (Json Text))
  assert "Param in endpoint: pattern /users/{capture}" (pat2 == "/users/{capture}")

  let pat3 = endpointPattern @(Get (At2 "api" "health") Text)
  assert "At2 in endpoint: pattern /api/health" (pat3 == "/api/health")


-- ===================================================================
-- Test: Named routes — additional coverage
-- ===================================================================

testNamedRoutesExtended :: IO ()
testNamedRoutesExtended = do
  let svc = mkNamedApi @ErgoAPI NamedHandlers
        { nhHealth  = pure "ok"
        , nhGetUser = \(PathCapture n) -> pure (Json (T.pack ("user-" ++ show n)))
        }

  -- Multiple user IDs work correctly
  req1 <- mkReq "GET" ["users", "1"] "/users/1" ""
  resp1 <- runService svc req1
  let decoded1 = Aeson.decode (LBS.fromStrict (responseBody resp1)) :: Maybe Text
  assert "named-ext: GET /users/1 -> 'user-1'" (decoded1 == Just "user-1")

  req2 <- mkReq "GET" ["users", "999"] "/users/999" ""
  resp2 <- runService svc req2
  let decoded2 = Aeson.decode (LBS.fromStrict (responseBody resp2)) :: Maybe Text
  assert "named-ext: GET /users/999 -> 'user-999'" (decoded2 == Just "user-999")

  -- DELETE /health -> 405 (only GET defined)
  req3 <- mkReq "DELETE" ["health"] "/health" ""
  resp3 <- runService svc req3
  assert "named-ext: DELETE /health -> 405" (statusCode (responseStatus resp3) == 405)

  -- PUT /users/1 -> 405
  req4 <- mkReq "PUT" ["users", "1"] "/users/1" ""
  resp4 <- runService svc req4
  assert "named-ext: PUT /users/1 -> 405" (statusCode (responseStatus resp4) == 405)

  -- 404 for deep unknown paths
  req5 <- mkReq "GET" ["a", "b", "c"] "/a/b/c" ""
  resp5 <- runService svc req5
  assert "named-ext: GET /a/b/c -> 404" (statusCode (responseStatus resp5) == 404)


-- ===================================================================
-- Test: Content negotiation — additional parseAccept tests
-- ===================================================================

testParseAcceptExtended :: IO ()
testParseAcceptExtended = do
  -- Multiple types with mixed quality
  let parsed1 = parseAccept "text/html;q=0.5, application/json;q=1.0, text/plain;q=0.7"
  assert "parseAccept-ext: 3 entries" (length parsed1 == 3)
  assert "parseAccept-ext: json first (highest q)" (fst (head parsed1) == "application/json")
  assert "parseAccept-ext: plain second" (fst (parsed1 !! 1) == "text/plain")
  assert "parseAccept-ext: html last" (fst (parsed1 !! 2) == "text/html")

  -- Multiple types with same quality: stable ordering
  let parsed2 = parseAccept "text/html, application/json"
  assert "parseAccept-ext: same q, 2 entries" (length parsed2 == 2)

  -- Whitespace handling
  let parsed3 = parseAccept " application/json , text/plain ; q=0.5 "
  assert "parseAccept-ext: whitespace stripped, 2 entries" (length parsed3 == 2)

  -- Wildcard */* with quality
  let parsed4 = parseAccept "*/*;q=0.1, application/json"
  assert "parseAccept-ext: wildcard with q" (length parsed4 == 2)
  assert "parseAccept-ext: json before wildcard" (fst (head parsed4) == "application/json")


-- ===================================================================
-- Test: negotiate — additional 406 edge cases
-- ===================================================================

testNegotiateExtended :: IO ()
testNegotiateExtended = do
  let jsonEnc :: Text -> ByteString
      jsonEnc = TE.encodeUtf8

      formats :: [(Text, Text -> ByteString)]
      formats = [("application/json", jsonEnc)]

  -- 406 with specific non-matching Accept
  let resp1 = negotiate "application/xml" formats ("hello" :: Text)
  assert "negotiate-ext: 406 for xml-only Accept" (responseStatus resp1 == status406)

  -- Success with exact match
  let resp2 = negotiate "application/json" formats ("world" :: Text)
  assert "negotiate-ext: 200 for exact match" (responseStatus resp2 == status200)
  assert "negotiate-ext: body correct" (responseBody resp2 == "world")

  -- Wildcard Accept matches
  let resp3 = negotiate "*/*" formats ("star" :: Text)
  assert "negotiate-ext: 200 for wildcard" (responseStatus resp3 == status200)

  -- Empty formats list -> 406
  let emptyFormats :: [(Text, Text -> ByteString)]
      emptyFormats = []
  let resp4 = negotiate "application/json" emptyFormats ("test" :: Text)
  assert "negotiate-ext: 406 for empty formats" (responseStatus resp4 == status406)


-- ===================================================================
-- Test: mkRecordApi (automatic record-based handler binding)
-- ===================================================================

-- | Named API: endpoints wrapped with Named for record-based binding.
-- Deliberately in a different order from the record fields to prove
-- that name matching, not position, determines the binding.
type RecordAPI =
  '[ Named "health"  (Get HealthPath Text)
   , Named "getUser" (Get UserByIdPath (Json Text))
   ]

-- | Handler record with fields in REVERSE order of the API.
data RecordHandlers = RecordHandlers
  { getUser :: PathCapture Int -> IO (Json Text)
  , health  :: IO Text
  }

testRecordApi :: IO ()
testRecordApi = do
  let svc = mkRecordApi @RecordAPI RecordHandlers
        { health  = pure "ok-record"
        , getUser = \(PathCapture n) -> pure (Json (T.pack ("rec-user-" ++ show n)))
        }

  -- GET /health
  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "recordApi: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "recordApi: GET /health -> 'ok-record'" (responseBody resp1 == "ok-record")

  -- GET /users/7
  req2 <- mkReq "GET" ["users", "7"] "/users/7" ""
  resp2 <- runService svc req2
  let decoded2 = Aeson.decode (LBS.fromStrict (responseBody resp2)) :: Maybe Text
  assert "recordApi: GET /users/7 -> 200" (statusCode (responseStatus resp2) == 200)
  assert "recordApi: GET /users/7 -> 'rec-user-7'" (decoded2 == Just "rec-user-7")

  -- 404
  req3 <- mkReq "GET" ["nope"] "/nope" ""
  resp3 <- runService svc req3
  assert "recordApi: GET /nope -> 404" (statusCode (responseStatus resp3) == 404)

  -- 405
  req4 <- mkReq "POST" ["health"] "/health" ""
  resp4 <- runService svc req4
  assert "recordApi: POST /health -> 405" (statusCode (responseStatus resp4) == 405)


-- | Named API + Describe wrapper stacking.
type DescribedRecordAPI =
  '[ Named "health" (Describe "Health check" (Get HealthPath Text))
   ]

data DescribedHandlers = DescribedHandlers
  { dHealth :: IO Text
  }

testRecordApiWithDescribe :: IO ()
testRecordApiWithDescribe = do
  -- Named composes with other wrappers (Describe in this case)
  -- We can't use 'health' as field name because it conflicts with RecordHandlers
  -- so we use a different API with different field names
  let svc = mkRecordApi @'[ Named "dHealth" (Describe "Health check" (Get HealthPath Text)) ]
        DescribedHandlers { dHealth = pure "described-ok" }

  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "recordApi+Describe: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "recordApi+Describe: GET /health -> 'described-ok'" (responseBody resp1 == "described-ok")


-- | Named API used with tuples (Named is transparent).
testNamedWithTuples :: IO ()
testNamedWithTuples = do
  let svc = mkApi @RecordAPI
        ( pure "tuple-health" :: IO Text
        , \(PathCapture n) -> pure (Json (T.pack ("tuple-user-" ++ show (n :: Int)))) :: IO (Json Text)
        )

  req1 <- mkReq "GET" ["health"] "/health" ""
  resp1 <- runService svc req1
  assert "namedTuple: GET /health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "namedTuple: GET /health -> 'tuple-health'" (responseBody resp1 == "tuple-health")

  req2 <- mkReq "GET" ["users", "3"] "/users/3" ""
  resp2 <- runService svc req2
  let decoded2 = Aeson.decode (LBS.fromStrict (responseBody resp2)) :: Maybe Text
  assert "namedTuple: GET /users/3 -> 'tuple-user-3'" (decoded2 == Just "tuple-user-3")


-- ===================================================================
-- Test: Versioned routing
-- ===================================================================

data V1
instance ApiVersion V1 where versionPrefix = "v1"

type VersionedHealthAPI = '[ Versioned V1 (Get HealthPath Text) ]

testVersionedRouting :: IO ()
testVersionedRouting = do
  let svc = mkApi @VersionedHealthAPI (pure "ok" :: IO Text)

  -- GET /v1/health -> 200
  req1 <- mkReq "GET" ["v1", "health"] "/v1/health" ""
  resp1 <- runService svc req1
  assert "versioned: GET /v1/health -> 200" (statusCode (responseStatus resp1) == 200)
  assert "versioned: GET /v1/health -> 'ok'" (responseBody resp1 == "ok")

  -- GET /health -> 404 (version prefix required)
  req2 <- mkReq "GET" ["health"] "/health" ""
  resp2 <- runService svc req2
  assert "versioned: GET /health -> 404" (statusCode (responseStatus resp2) == 404)


-- ===================================================================
-- Test: ValidatedBody extraction
-- ===================================================================

data NonEmptyValidator

instance Validate NonEmptyValidator Text where
  validate t
    | T.null t  = Left "must not be empty"
    | otherwise = Right t

testValidatedBody :: IO ()
testValidatedBody = do
  let handler :: ValidatedBody NonEmptyValidator Text -> IO (Json Text)
      handler (ValidatedBody t) = pure (Json t)
  let svc = mkApi @'[Post (At "items") (Json Text) (Json Text)] handler

  -- Valid body
  req1 <- mkReq "POST" ["items"] "/items" "\"hello\""
  resp1 <- runService svc req1
  assert "validated: valid body -> 200" (statusCode (responseStatus resp1) == 200)

  -- Invalid body (empty string)
  req2 <- mkReq "POST" ["items"] "/items" "\"\""
  resp2 <- runService svc req2
  assert "validated: empty body -> 422" (statusCode (responseStatus resp2) == 422)


-- ===================================================================
-- Test: SSE chunk encoding
-- ===================================================================

testSSEChunk :: IO ()
testSSEChunk = do
  let chunk = sseChunk (sseData ("hello" :: Text))
  assert "SSE: chunk contains data:" (BS.isInfixOf "data: " chunk)
  assert "SSE: chunk contains payload" (BS.isInfixOf "hello" chunk)
  assert "SSE: chunk ends with double newline" (BS.isSuffixOf "\n\n" chunk)

  -- With event type
  let chunk2 = sseChunk (sseEvent "update" ("world" :: Text))
  assert "SSE: event type present" (BS.isInfixOf "event: update" chunk2)


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "acolyte-server tests:"
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
  putStrLn "Sub-API composition (combineServer):"
  testCombineServer
  putStrLn ""
  putStrLn "Combined effectful server:"
  testCombinedEffects
  putStrLn ""
  putStrLn "ToHandler (ergonomic handlers):"
  testToHandler
  putStrLn ""
  putStrLn "mkApi (ergonomic server construction):"
  testMkApi
  putStrLn ""
  putStrLn "PathCapture FromRequestParts:"
  testPathCapture
  putStrLn ""
  putStrLn "QueryParam extraction:"
  testQueryParam
  putStrLn ""
  putStrLn "JsonBody via FromRequestParts:"
  testJsonBody
  putStrLn ""
  putStrLn "HeaderMap extraction:"
  testHeaderMap
  putStrLn ""
  putStrLn "FullRequest extraction:"
  testFullRequest
  putStrLn ""
  putStrLn "RawQuery extraction:"
  testRawQuery
  putStrLn ""
  putStrLn "RawPathParams extraction:"
  testRawPathParams
  putStrLn ""
  putStrLn "StringBody extraction:"
  testStringBody
  putStrLn ""
  putStrLn "MatchedPath extraction:"
  testMatchedPathExtract
  putStrLn ""
  putStrLn "OriginalUri extraction:"
  testOriginalUri
  putStrLn ""
  putStrLn "NestedPath extraction:"
  testNestedPath
  putStrLn ""
  putStrLn "RawForm extraction:"
  testRawForm
  putStrLn ""
  putStrLn "Form extraction:"
  testForm
  putStrLn ""
  putStrLn "Multipart extraction:"
  testMultipart
  putStrLn ""
  putStrLn "ConnectInfo extractor:"
  testConnectInfo
  putStrLn ""
  putStrLn "ReqMethod extractor:"
  testReqMethod
  putStrLn ""
  putStrLn "Extension extractor:"
  testExtension
  putStrLn ""
  putStrLn "Optional extractor:"
  testOptional
  putStrLn ""
  putStrLn "AppState extractor:"
  testAppState
  putStrLn ""
  putStrLn "ToHandler with two extractors:"
  testToHandlerTwoArgs
  putStrLn ""
  putStrLn "ToHandler with body + parts:"
  testToHandlerBodyParts
  putStrLn ""
  putStrLn "ToHandler error propagation:"
  testToHandlerErrorProp
  putStrLn ""
  putStrLn "Named routes (record-based handlers):"
  testNamedRoutes
  putStrLn ""
  putStrLn "Content negotiation — parseAccept:"
  testParseAccept
  putStrLn ""
  putStrLn "Content negotiation — matchFormat:"
  testMatchFormat
  putStrLn ""
  putStrLn "Content negotiation — negotiate:"
  testNegotiate
  putStrLn ""
  putStrLn "Path helpers (compile-time checks):"
  testPathHelpers
  putStrLn ""
  putStrLn "Named routes (extended):"
  testNamedRoutesExtended
  putStrLn ""
  putStrLn "Content negotiation — parseAccept (extended):"
  testParseAcceptExtended
  putStrLn ""
  putStrLn "Content negotiation — negotiate (extended):"
  testNegotiateExtended
  putStrLn ""
  putStrLn "mkRecordApi (automatic record-based binding):"
  testRecordApi
  putStrLn ""
  putStrLn "mkRecordApi + Describe wrapper:"
  testRecordApiWithDescribe
  putStrLn ""
  putStrLn "Named endpoints with tuples (transparency):"
  testNamedWithTuples
  putStrLn ""
  putStrLn "Versioned routing:"
  testVersionedRouting
  putStrLn ""
  putStrLn "ValidatedBody extraction:"
  testValidatedBody
  putStrLn ""
  putStrLn "SSE chunk encoding:"
  testSSEChunk
  putStrLn ""
  putStrLn "All acolyte-server tests passed."
