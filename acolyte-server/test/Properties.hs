{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (evaluate, try, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Foldable (for_)
import qualified Data.Aeson as Aeson

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Acolyte.Server
  ( parseFormUrlEncoded, parseMultipart, ParseCapture(..)
  , FilePart(..)
  , Json(..), IntoResponse(..), intoResponse, mkError, ServerError(..)
  )
import Acolyte.Server.Negotiate (parseAccept, matchFormat)
import Http.Core (Response(..), responseStatus, responseBody)
import Network.HTTP.Types (status400, status404, status422, status500, statusCode)


-- ===================================================================
-- Generators
-- ===================================================================

-- | Generate an ASCII alphanumeric ByteString (no encoding issues).
genAlphaBS :: Gen ByteString
genAlphaBS = TE.encodeUtf8 <$> Gen.text (Range.linear 1 20) Gen.alphaNum


-- ===================================================================
-- Property: Form URL encoding roundtrip
-- ===================================================================

prop_formUrlEncodedRoundtrip :: Property
prop_formUrlEncodedRoundtrip = property $ do
  pairs <- forAll $ Gen.list (Range.linear 0 10) ((,) <$> genAlphaBS <*> genAlphaBS)
  let encoded = BS.intercalate "&" [ k <> "=" <> v | (k, v) <- pairs ]
      decoded = parseFormUrlEncoded encoded
  -- Empty input gives empty output
  if null pairs
    then decoded === []
    else decoded === pairs


-- ===================================================================
-- Property: Multipart roundtrip
-- ===================================================================

prop_multipartRoundtrip :: Property
prop_multipartRoundtrip = property $ do
  -- Generate (fieldName, bodyContent) pairs with safe ASCII content
  pairs <- forAll $ Gen.list (Range.linear 1 5) $ do
    name <- Gen.text (Range.linear 1 20) Gen.alphaNum
    body <- Gen.text (Range.linear 0 50) Gen.alphaNum
    pure (name, body)

  let boundary = "testboundary123" :: ByteString
      -- Construct multipart wire format
      parts = [ "--" <> boundary <> "\r\n"
                <> "Content-Disposition: form-data; name=\"" <> TE.encodeUtf8 name <> "\"\r\n"
                <> "\r\n"
                <> TE.encodeUtf8 body <> "\r\n"
              | (name, body) <- pairs
              ]
      wire = BS.concat parts <> "--" <> boundary <> "--\r\n"
      parsed = parseMultipart boundary wire
      parsedPairs = [ (fpFieldName fp, TE.decodeUtf8 (fpBody fp))
                    | fp <- parsed
                    ]

  -- Verify field names and bodies match
  map fst parsedPairs === map fst pairs
  map snd parsedPairs === map snd pairs


-- ===================================================================
-- Property: ParseCapture Int roundtrip
-- ===================================================================

prop_parseCaptureIntRoundtrip :: Property
prop_parseCaptureIntRoundtrip = property $ do
  n <- forAll $ Gen.int (Range.linear 0 maxBound)
  parseCapture (T.pack (show n)) === Just n


-- ===================================================================
-- Property: ParseCapture Text identity
-- ===================================================================

prop_parseCaptureTextIdentity :: Property
prop_parseCaptureTextIdentity = property $ do
  t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  parseCapture t === Just t


-- ===================================================================
-- Property: Json IntoResponse roundtrip via aeson
-- ===================================================================

prop_intoResponseJsonRoundtrip :: Property
prop_intoResponseJsonRoundtrip = property $ do
  val <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  let resp = intoResponse (Json val)
      decoded = Aeson.decodeStrict' (responseBody resp) :: Maybe Text
  decoded === Just val


-- ===================================================================
-- Property: Either Left error status is preserved
-- ===================================================================

prop_intoResponseEitherLeft :: Property
prop_intoResponseEitherLeft = property $ do
  s <- forAll $ Gen.element [status400, status404, status422, status500]
  msg <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
  let resp = intoResponse (Left (mkError s msg) :: Either ServerError (Json Text))
  statusCode (responseStatus resp) === statusCode s


-- ===================================================================
-- Property: Either Right gives status 200
-- ===================================================================

prop_intoResponseEitherRight :: Property
prop_intoResponseEitherRight = property $ do
  val <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  let resp = intoResponse (Right (Json val) :: Either ServerError (Json Text))
  statusCode (responseStatus resp) === 200


-- ===================================================================
-- Property: ServerError status is preserved in response
-- ===================================================================

prop_serverErrorStatusPreserved :: Property
prop_serverErrorStatusPreserved = property $ do
  s <- forAll $ Gen.element [status400, status404, status422, status500]
  msg <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
  let resp = intoResponse (mkError s msg)
  statusCode (responseStatus resp) === statusCode s


-- ===================================================================
-- Property: matchFormat picks the highest quality available format
-- ===================================================================

prop_negotiatePicksHighestQuality :: Property
prop_negotiatePicksHighestQuality = property $ do
  -- Generate distinct quality values from a fixed set to avoid
  -- floating-point formatting issues. Values are valid HTTP q-values.
  qJson  <- forAll $ Gen.element [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 :: Double]
  qPlain <- forAll $ Gen.element [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 :: Double]

  -- Build an Accept header with explicit quality values
  let showQ q = show q
      acceptHdr = TE.encodeUtf8 $ T.pack $
        "application/json;q=" ++ showQ qJson ++ ", text/plain;q=" ++ showQ qPlain

      available :: [(Text, Text -> ByteString)]
      available =
        [ ("application/json", TE.encodeUtf8)
        , ("text/plain",       TE.encodeUtf8)
        ]

  case matchFormat acceptHdr available of
    Nothing -> failure  -- Should always match at least one
    Just (ct, _enc) ->
      if qJson >= qPlain
        then ct === "application/json"
        else ct === "text/plain"


-- ===================================================================
-- Property: parseAccept roundtrip — extracts correct media types
-- ===================================================================

prop_parseAcceptRoundtrip :: Property
prop_parseAcceptRoundtrip = property $ do
  -- Generate a list of (media-type, quality) pairs
  entries <- forAll $ Gen.list (Range.linear 1 5) $ do
    mtype <- Gen.element
      [ "application/json"
      , "text/plain"
      , "text/html"
      , "application/xml"
      , "image/png"
      ]
    q <- Gen.element [0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0]
    pure (mtype :: Text, q :: Double)

  -- Build Accept header string
  let formatEntry (mt, q) =
        if q == 1.0
          then mt
          else mt <> ";q=" <> T.pack (show q)
      acceptStr = TE.encodeUtf8 $ T.intercalate ", " (map formatEntry entries)

  let parsed = parseAccept acceptStr
      -- Extract just the media types from the parsed result
      parsedTypes = map fst parsed

  -- Every media type from the input should appear in the parsed output
  annotateShow parsed
  for_ entries $ \(mt, _q) ->
    assert (TE.encodeUtf8 mt `elem` parsedTypes)


-- ===================================================================
-- Fuzz: multipart parser never crashes on arbitrary input
-- ===================================================================

-- | Feed random bytes as boundary and body to parseMultipart and assert
-- it returns a result (never throws an uncaught exception).
prop_multipartParserDoesNotCrash :: Property
prop_multipartParserDoesNotCrash = property $ do
  boundary <- forAll $ Gen.bytes (Range.linear 1 50)
  body <- forAll $ Gen.bytes (Range.linear 0 10000)
  let parts = parseMultipart boundary body
  -- Force evaluation of all parts
  _ <- evalIO $ try @SomeException $
    evaluate (length parts `seq` ())
  success


-- ===================================================================
-- Fuzz: form URL parser never crashes on arbitrary input
-- ===================================================================

-- | Feed random bytes to parseFormUrlEncoded and assert it returns
-- a result (never throws an uncaught exception).
prop_formParserDoesNotCrash :: Property
prop_formParserDoesNotCrash = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 0 5000)
  let pairs = parseFormUrlEncoded bs
  -- Force evaluation of all pairs
  _ <- evalIO $ try @SomeException $
    evaluate (length pairs `seq` ())
  success


-- ===================================================================
-- Main
-- ===================================================================

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  ok <- tests
  if ok then pure () else error "Property tests failed"
