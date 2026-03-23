{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Runtime performance benchmarks for servant-reimagined.
--
-- Measures dispatch throughput, gRPC codec speed, middleware overhead,
-- extractor cost, and router scaling — all without network I/O.
--
-- Run: cabal run runtime-bench -- +RTS -T
module Main (main) where

import Test.Tasty.Bench

import Control.DeepSeq (NFData (..), force)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (statusCode)

import Tower (Service (..), Middleware, before, (|>))
import Http.Core
  ( Request (..)
  , Response (..)
  , emptyExtensions
  )
import Servant.Reimagined.Core
  ( Get, At, Param
  )
import Servant.Reimagined.Server
  ( mkApi
  , PathCapture (..)
  , QueryParam (..)
  , ReqHeader (..)
  , Json (..)
  , toBoundHandler
  , serve
  , emptyRouter
  , addRoute
  )
import Tower.Grpc.Codec
  ( encodeMessage
  , encodeMessages
  , decodeMessage
  , decodeMessages
  , GrpcMessage (..)
  )


-- ===================================================================
-- NFData instances
-- ===================================================================

instance NFData (Response ByteString) where
  rnf (Response s h b) = rnf (statusCode s) `seq` rnf h `seq` rnf b

instance NFData GrpcMessage where
  rnf (GrpcMessage c p) = rnf c `seq` rnf p


-- ===================================================================
-- Request construction helpers
-- ===================================================================

-- | Build a test request with the given method, path, query, and headers.
mkRequest
  :: ByteString                       -- ^ HTTP method
  -> ByteString                       -- ^ Raw path (e.g. "/users/42")
  -> [(ByteString, Maybe ByteString)] -- ^ Query parameters
  -> [(ByteString, ByteString)]       -- ^ Headers (name, value)
  -> ByteString                       -- ^ Body
  -> IO (Request ByteString)
mkRequest method path query headers body = do
  exts <- emptyExtensions
  let segments = filter (/= "") $ T.splitOn "/" (TE.decodeUtf8 path)
      ciHeaders = map (\(n, v) -> (CI.mk n, v)) headers
  pure Request
    { requestMethod     = method
    , requestPathRaw    = path
    , requestPath       = segments
    , requestQuery      = query
    , requestHeaders    = ciHeaders
    , requestBody       = body
    , requestExtensions = exts
    }

-- | Simple GET request to a path.
mkGet :: ByteString -> IO (Request ByteString)
mkGet path = mkRequest "GET" path [] [] ""

-- | GET request with query parameters.
mkGetWithQuery :: ByteString -> [(ByteString, Maybe ByteString)] -> IO (Request ByteString)
mkGetWithQuery path query = mkRequest "GET" path query [] ""

-- | GET request with query parameters and headers.
mkGetWithQueryAndHeaders
  :: ByteString
  -> [(ByteString, Maybe ByteString)]
  -> [(ByteString, ByteString)]
  -> IO (Request ByteString)
mkGetWithQueryAndHeaders path query headers =
  mkRequest "GET" path query headers ""


-- ===================================================================
-- 1. Dispatch throughput — API definitions and handlers
-- ===================================================================

-- 1-endpoint API
type API1 = '[ Get (At "health") Text ]

healthHandler :: IO Text
healthHandler = pure "ok"

-- 3-endpoint API
type API3 =
  '[ Get (At "health") Text
   , Get (At "users") (Json Text)
   , Get (Param "users" Int) (Json Text)
   ]

usersHandler :: IO (Json Text)
usersHandler = pure (Json "users")

getUserHandler :: PathCapture Int -> IO (Json Text)
getUserHandler (PathCapture n) = pure (Json (T.pack ("user-" <> show n)))

-- 5-endpoint API
type API5 =
  '[ Get (At "health") Text
   , Get (At "users") (Json Text)
   , Get (Param "users" Int) (Json Text)
   , Get (At "posts") (Json Text)
   , Get (Param "posts" Int) (Json Text)
   ]

postsHandler :: IO (Json Text)
postsHandler = pure (Json "posts")

getPostHandler :: PathCapture Int -> IO (Json Text)
getPostHandler (PathCapture n) = pure (Json (T.pack ("post-" <> show n)))


-- ===================================================================
-- 3. Middleware overhead — no-op layer
-- ===================================================================

noopLayer :: Middleware IO (Request ByteString) (Response ByteString)
noopLayer = before $ \_ -> pure ()


-- ===================================================================
-- 4. Extractor overhead — APIs with varying extractor counts
-- ===================================================================

-- 0 extractors
type ExtAPI0 = '[ Get (At "ext0") Text ]

ext0Handler :: IO Text
ext0Handler = pure "ok"

-- 1 extractor: PathCapture
type ExtAPI1 = '[ Get (Param "ext1" Int) (Json Text) ]

ext1Handler :: PathCapture Int -> IO (Json Text)
ext1Handler (PathCapture n) = pure (Json (T.pack (show n)))

-- 2 extractors: PathCapture + QueryParam
type ExtAPI2 = '[ Get (Param "ext2" Int) (Json Text) ]

ext2Handler :: PathCapture Int -> QueryParam "q" Text -> IO (Json Text)
ext2Handler (PathCapture n) (QueryParam q) =
  pure (Json (T.pack (show n) <> q))

-- 3 extractors: PathCapture + QueryParam + ReqHeader
type ExtAPI3 = '[ Get (Param "ext3" Int) (Json Text) ]

ext3Handler :: PathCapture Int -> QueryParam "q" Text -> ReqHeader "X-Id" -> IO (Json Text)
ext3Handler (PathCapture n) (QueryParam q) (ReqHeader _h) =
  pure (Json (T.pack (show n) <> q))


-- ===================================================================
-- 5. Router scaling — APIs with many endpoints
-- ===================================================================

-- 5-endpoint scaling API (distinct paths for worst-case matching)
type ScaleAPI5 =
  '[ Get (At "r1") Text
   , Get (At "r2") Text
   , Get (At "r3") Text
   , Get (At "r4") Text
   , Get (At "r5") Text
   ]

stubHandler :: IO Text
stubHandler = pure "ok"

-- 10-endpoint scaling API
type ScaleAPI10 =
  '[ Get (At "r1") Text
   , Get (At "r2") Text
   , Get (At "r3") Text
   , Get (At "r4") Text
   , Get (At "r5") Text
   , Get (At "r6") Text
   , Get (At "r7") Text
   , Get (At "r8") Text
   , Get (At "r9") Text
   , Get (At "r10") Text
   ]

-- 25-endpoint type aliases for individual routes (used with toBoundHandler)
type S1  = Get (At "s1")  Text
type S2  = Get (At "s2")  Text
type S3  = Get (At "s3")  Text
type S4  = Get (At "s4")  Text
type S5  = Get (At "s5")  Text
type S6  = Get (At "s6")  Text
type S7  = Get (At "s7")  Text
type S8  = Get (At "s8")  Text
type S9  = Get (At "s9")  Text
type S10 = Get (At "s10") Text
type S11 = Get (At "s11") Text
type S12 = Get (At "s12") Text
type S13 = Get (At "s13") Text
type S14 = Get (At "s14") Text
type S15 = Get (At "s15") Text
type S16 = Get (At "s16") Text
type S17 = Get (At "s17") Text
type S18 = Get (At "s18") Text
type S19 = Get (At "s19") Text
type S20 = Get (At "s20") Text
type S21 = Get (At "s21") Text
type S22 = Get (At "s22") Text
type S23 = Get (At "s23") Text
type S24 = Get (At "s24") Text
type S25 = Get (At "s25") Text

-- Build a 25-route router using the low-level Router API
mkScale25 :: Service IO (Request ByteString) (Response ByteString)
mkScale25 = serve
  $ addRoute (toBoundHandler @S25 stubHandler)
  $ addRoute (toBoundHandler @S24 stubHandler)
  $ addRoute (toBoundHandler @S23 stubHandler)
  $ addRoute (toBoundHandler @S22 stubHandler)
  $ addRoute (toBoundHandler @S21 stubHandler)
  $ addRoute (toBoundHandler @S20 stubHandler)
  $ addRoute (toBoundHandler @S19 stubHandler)
  $ addRoute (toBoundHandler @S18 stubHandler)
  $ addRoute (toBoundHandler @S17 stubHandler)
  $ addRoute (toBoundHandler @S16 stubHandler)
  $ addRoute (toBoundHandler @S15 stubHandler)
  $ addRoute (toBoundHandler @S14 stubHandler)
  $ addRoute (toBoundHandler @S13 stubHandler)
  $ addRoute (toBoundHandler @S12 stubHandler)
  $ addRoute (toBoundHandler @S11 stubHandler)
  $ addRoute (toBoundHandler @S10 stubHandler)
  $ addRoute (toBoundHandler @S9 stubHandler)
  $ addRoute (toBoundHandler @S8 stubHandler)
  $ addRoute (toBoundHandler @S7 stubHandler)
  $ addRoute (toBoundHandler @S6 stubHandler)
  $ addRoute (toBoundHandler @S5 stubHandler)
  $ addRoute (toBoundHandler @S4 stubHandler)
  $ addRoute (toBoundHandler @S3 stubHandler)
  $ addRoute (toBoundHandler @S2 stubHandler)
  $ addRoute (toBoundHandler @S1 stubHandler)
  $ emptyRouter


-- ===================================================================
-- Main benchmark
-- ===================================================================

main :: IO ()
main = do
  -- -------------------------------------------------------------------
  -- Build services once (outside benchmarks)
  -- -------------------------------------------------------------------

  -- Dispatch benchmarks
  let svc1 = mkApi @API1 healthHandler
  let svc3 = mkApi @API3 (healthHandler, usersHandler, getUserHandler)
  let svc5 = mkApi @API5
        (healthHandler, usersHandler, getUserHandler, postsHandler, getPostHandler)

  -- Middleware benchmarks
  let svcBase = mkApi @API1 healthHandler
  let svc1mw  = svcBase |> noopLayer
  let svc3mw  = svcBase |> noopLayer |> noopLayer |> noopLayer
  let svc5mw  = svcBase |> noopLayer |> noopLayer |> noopLayer |> noopLayer |> noopLayer

  -- Extractor benchmarks
  let svcExt0 = mkApi @ExtAPI0 ext0Handler
  let svcExt1 = mkApi @ExtAPI1 ext1Handler
  let svcExt2 = mkApi @ExtAPI2 ext2Handler
  let svcExt3 = mkApi @ExtAPI3 ext3Handler

  -- Router scaling benchmarks
  let svcScale1  = mkApi @API1 healthHandler
  let svcScale5  = mkApi @ScaleAPI5
        (stubHandler, stubHandler, stubHandler, stubHandler, stubHandler)
  let svcScale10 = mkApi @ScaleAPI10
        ( stubHandler, stubHandler, stubHandler, stubHandler, stubHandler
        , stubHandler, stubHandler, stubHandler, stubHandler, stubHandler )
  let svcScale25 = mkScale25

  -- -------------------------------------------------------------------
  -- gRPC codec payloads (pre-built)
  -- -------------------------------------------------------------------
  let payload10   = BS.replicate 10 0x41
  let payload100  = BS.replicate 100 0x42
  let payload1K   = BS.replicate 1024 0x43
  let payload10K  = BS.replicate (10 * 1024) 0x44
  let payload100K = BS.replicate (100 * 1024) 0x45

  let encoded10   = encodeMessage payload10
  let encoded100  = encodeMessage payload100
  let encoded1K   = encodeMessage payload1K
  let encoded10K  = encodeMessage payload10K
  let encoded100K = encodeMessage payload100K

  -- Multi-message payloads
  let msgs10  = replicate 10 payload100
  let msgs100 = replicate 100 payload100
  let encodedMulti10  = encodeMessages msgs10
  let encodedMulti100 = encodeMessages msgs100

  -- Force all pre-built data
  _ <- pure $! force encoded10
  _ <- pure $! force encoded100
  _ <- pure $! force encoded1K
  _ <- pure $! force encoded10K
  _ <- pure $! force encoded100K
  _ <- pure $! force encodedMulti10
  _ <- pure $! force encodedMulti100

  defaultMain
    [ -- ---------------------------------------------------------------
      -- 1. Request dispatch throughput
      -- ---------------------------------------------------------------
      bgroup "dispatch"
        [ bench "1-endpoint/hit" $ nfIO $ do
            req <- mkGet "/health"
            runService svc1 req
        , bench "3-endpoints/first" $ nfIO $ do
            req <- mkGet "/health"
            runService svc3 req
        , bench "3-endpoints/last" $ nfIO $ do
            req <- mkGet "/users/42"
            runService svc3 req
        , bench "3-endpoints/miss" $ nfIO $ do
            req <- mkGet "/nonexistent"
            runService svc3 req
        , bench "5-endpoints/first" $ nfIO $ do
            req <- mkGet "/health"
            runService svc5 req
        , bench "5-endpoints/last" $ nfIO $ do
            req <- mkGet "/posts/42"
            runService svc5 req
        , bench "5-endpoints/miss" $ nfIO $ do
            req <- mkGet "/nonexistent"
            runService svc5 req
        ]

      -- ---------------------------------------------------------------
      -- 2. gRPC codec throughput
      -- ---------------------------------------------------------------
    , bgroup "grpc-codec"
        [ bgroup "encode"
            [ bench "10B"   $ nf encodeMessage payload10
            , bench "100B"  $ nf encodeMessage payload100
            , bench "1KB"   $ nf encodeMessage payload1K
            , bench "10KB"  $ nf encodeMessage payload10K
            , bench "100KB" $ nf encodeMessage payload100K
            ]
        , bgroup "decode"
            [ bench "10B"   $ nf decodeMessage encoded10
            , bench "100B"  $ nf decodeMessage encoded100
            , bench "1KB"   $ nf decodeMessage encoded1K
            , bench "10KB"  $ nf decodeMessage encoded10K
            , bench "100KB" $ nf decodeMessage encoded100K
            ]
        , bgroup "encode-multi"
            [ bench "10-msgs"  $ nf encodeMessages msgs10
            , bench "100-msgs" $ nf encodeMessages msgs100
            ]
        , bgroup "decode-multi"
            [ bench "10-msgs"  $ nf decodeMessages encodedMulti10
            , bench "100-msgs" $ nf decodeMessages encodedMulti100
            ]
        ]

      -- ---------------------------------------------------------------
      -- 3. Middleware overhead
      -- ---------------------------------------------------------------
    , bgroup "middleware"
        [ bench "0-layers" $ nfIO $ do
            req <- mkGet "/health"
            runService svcBase req
        , bench "1-layer" $ nfIO $ do
            req <- mkGet "/health"
            runService svc1mw req
        , bench "3-layers" $ nfIO $ do
            req <- mkGet "/health"
            runService svc3mw req
        , bench "5-layers" $ nfIO $ do
            req <- mkGet "/health"
            runService svc5mw req
        ]

      -- ---------------------------------------------------------------
      -- 4. Extractor overhead
      -- ---------------------------------------------------------------
    , bgroup "extractors"
        [ bench "0-extractors" $ nfIO $ do
            req <- mkGet "/ext0"
            runService svcExt0 req
        , bench "1-extractor" $ nfIO $ do
            req <- mkGet "/ext1/42"
            runService svcExt1 req
        , bench "2-extractors" $ nfIO $ do
            req <- mkGetWithQuery "/ext2/42" [("q", Just "hello")]
            runService svcExt2 req
        , bench "3-extractors" $ nfIO $ do
            req <- mkGetWithQueryAndHeaders
              "/ext3/42"
              [("q", Just "hello")]
              [("X-Id", "abc123")]
            runService svcExt3 req
        ]

      -- ---------------------------------------------------------------
      -- 5. Router scaling (worst case: hit last route)
      -- ---------------------------------------------------------------
    , bgroup "router-scaling"
        [ bench "1-route/hit" $ nfIO $ do
            req <- mkGet "/health"
            runService svcScale1 req
        , bench "5-routes/last" $ nfIO $ do
            req <- mkGet "/r5"
            runService svcScale5 req
        , bench "10-routes/last" $ nfIO $ do
            req <- mkGet "/r10"
            runService svcScale10 req
        , bench "25-routes/last" $ nfIO $ do
            req <- mkGet "/s25"
            runService svcScale25 req
        , bench "25-routes/miss" $ nfIO $ do
            req <- mkGet "/nonexistent"
            runService svcScale25 req
        ]
    ]
