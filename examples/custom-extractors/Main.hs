-- | Custom extractors: FromRequestParts for headers, body, and query params.
--
-- Run:  cabal run custom-extractors
-- Test: curl -H 'Authorization: Bearer my-secret' http://localhost:3000/token
--       curl http://localhost:3000/headers
--       curl 'http://localhost:3000/search?q=haskell&format=json'
--       curl -X POST http://localhost:3000/echo -d 'Hello, world!'
--       curl -H 'Authorization: Bearer tok' 'http://localhost:3000/combined?q=test'
module Main (main) where

import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (statusCode)

import Acolyte.Prelude


-- ===================================================================
-- Custom extractor 1: BearerToken (from Authorization header)
-- ===================================================================

newtype BearerToken = BearerToken { unBearerToken :: Text }
  deriving (Show)

instance FromRequestParts BearerToken where
  fromRequestParts parts =
    case lookup "authorization" (rpHeaders parts) of
      Just val | "Bearer " `BS.isPrefixOf` val ->
        pure $ Right (BearerToken (TE.decodeUtf8 (BS.drop 7 val)))
      _ -> pure $ Left (mkError status401 "Missing or invalid Bearer token")


-- ===================================================================
-- Custom extractor 2: RawText (body as UTF-8 text via BodyBytes)
-- ===================================================================

newtype RawText = RawText { unRawText :: Text }
  deriving (Show)

instance FromRequestParts RawText where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    pure $ case mBody of
      Just (BodyBytes body) -> Right (RawText (TE.decodeUtf8 body))
      Nothing -> Left (mkError status400 "No request body")


-- ===================================================================
-- API type
-- ===================================================================

type ExtractorAPI =
  '[ Get  (At "token")    (Json Aeson.Value)       -- GET  /token    (bearer token)
   , Get  (At "headers")  (Json Aeson.Value)       -- GET  /headers  (all headers)
   , Get  (At "search")   (Json Aeson.Value)       -- GET  /search   (query params)
   , Post (At "echo")  (Json ()) (Json Aeson.Value) -- POST /echo     (raw text body)
   , Get  (At "combined") (Json Aeson.Value)       -- GET  /combined (multi-extractor)
   ]


-- ===================================================================
-- Handlers
-- ===================================================================

-- | Extract and display the Bearer token.
tokenHandler :: BearerToken -> IO (Json Aeson.Value)
tokenHandler (BearerToken tok) =
  pure $ Json $ object ["token" .= tok]

-- | Show all request headers.
headersHandler :: HeaderMap -> IO (Json Aeson.Value)
headersHandler (HeaderMap hdrs) =
  let pairs = [ (TE.decodeUtf8 (CI.original k), TE.decodeUtf8 v)
              | (k, v) <- hdrs ]
  in pure $ Json $ object
    [ "headers" .= object [ Key.fromText k .= v | (k, v) <- pairs ] ]

-- | Extract typed query parameters.
searchHandler :: QueryParam "q" Text -> OptionalParam "format" Text -> IO (Json Aeson.Value)
searchHandler (QueryParam q) (OptionalParam mFmt) =
  let fmt = case mFmt of { Just f -> f; Nothing -> "text" }
  in pure $ Json $ object
    [ "query" .= q, "format" .= fmt ]

-- | Echo body text back as JSON.
echoHandler :: RawText -> IO (Json Aeson.Value)
echoHandler (RawText txt) =
  pure $ Json $ object
    [ "echo" .= txt, "length" .= T.length txt ]

-- | Combine multiple extractors in one handler.
combinedHandler :: BearerToken -> QueryParam "q" Text -> HeaderMap -> IO (Json Aeson.Value)
combinedHandler (BearerToken tok) (QueryParam q) (HeaderMap hdrs) =
  pure $ Json $ object
    [ "token"        .= tok
    , "query"        .= q
    , "header_count" .= length hdrs
    ]


-- ===================================================================
-- Server + main
-- ===================================================================

server :: Service IO (Request ByteString) (Response ByteString)
server = mkApi @ExtractorAPI
  ( tokenHandler
  , headersHandler
  , searchHandler
  , echoHandler
  , combinedHandler
  )

main :: IO ()
main = do
  putStrLn "Custom extractors example on http://localhost:3000"
  putStrLn "  GET  /token     - BearerToken extractor"
  putStrLn "  GET  /headers   - HeaderMap extractor"
  putStrLn "  GET  /search    - QueryParam + OptionalParam"
  putStrLn "  POST /echo      - RawText body extractor"
  putStrLn "  GET  /combined  - multiple extractors in one handler"
  runServerBS 3000 server
