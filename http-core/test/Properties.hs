{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Http.Core
import Http.Core.Body
import Http.Core.Extensions (emptyExtensions, insertExtension, lookupExtension, deleteExtension)

import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Query)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- ===================================================================
-- Test value types
-- ===================================================================

newtype TestVal = TestVal Int
  deriving (Show, Eq, Typeable)


-- ===================================================================
-- Generators
-- ===================================================================

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-10000) 10000)

genByteString :: Gen ByteString
genByteString = Gen.bytes (Range.linear 0 256)

genText :: Gen Text
genText = Gen.text (Range.linear 0 256) Gen.unicode

genMethod :: Gen Method
genMethod = Gen.element
  [methodGet, methodPost, methodPut, methodDelete, methodPatch, methodHead]

genPathSegment :: Gen Text
genPathSegment = Gen.text (Range.linear 1 20) Gen.alphaNum

genPath :: Gen [Text]
genPath = Gen.list (Range.linear 0 5) genPathSegment

genQuery :: Gen Query
genQuery = Gen.list (Range.linear 0 5) $ do
  k <- Gen.bytes (Range.linear 1 20)
  mv <- Gen.maybe (Gen.bytes (Range.linear 0 50))
  pure (k, mv)

genHeader :: Gen Header
genHeader = do
  name <- Gen.element ["Content-Type", "Accept", "Authorization", "X-Custom"]
  val <- Gen.bytes (Range.linear 1 50)
  pure (name, val)

genHeaders :: Gen RequestHeaders
genHeaders = Gen.list (Range.linear 0 5) genHeader


-- ===================================================================
-- Properties
-- ===================================================================

-- 1. Insert a value into Extensions, lookup returns Just that value.
prop_extensionsInsertLookup :: Property
prop_extensionsInsertLookup = property $ do
  n <- forAll genInt
  val <- evalIO $ do
    exts <- emptyExtensions
    insertExtension (TestVal n) exts
    lookupExtension @TestVal exts
  val === Just (TestVal n)

-- 2. Insert twice with the same type, lookup returns the second value.
prop_extensionsOverwrite :: Property
prop_extensionsOverwrite = property $ do
  n1 <- forAll genInt
  n2 <- forAll genInt
  val <- evalIO $ do
    exts <- emptyExtensions
    insertExtension (TestVal n1) exts
    insertExtension (TestVal n2) exts
    lookupExtension @TestVal exts
  val === Just (TestVal n2)

-- 3. Insert then delete, lookup returns Nothing.
prop_extensionsDeleteLookup :: Property
prop_extensionsDeleteLookup = property $ do
  n <- forAll genInt
  val <- evalIO $ do
    exts <- emptyExtensions
    insertExtension (TestVal n) exts
    deleteExtension @TestVal exts
    lookupExtension @TestVal exts
  val === (Nothing :: Maybe TestVal)

-- 4. fromBytes roundtrip: bodyToStrict (fromBytes bs) == bs
prop_bodyFromBytesRoundtrip :: Property
prop_bodyFromBytesRoundtrip = property $ do
  bs <- forAll genByteString
  result <- evalIO $ bodyToStrict (fromBytes bs)
  result === bs

-- 5. fromText roundtrip: bodyToStrict (fromText t) == encodeUtf8 t
prop_bodyFromTextRoundtrip :: Property
prop_bodyFromTextRoundtrip = property $ do
  t <- forAll genText
  result <- evalIO $ bodyToStrict (fromText t)
  result === TE.encodeUtf8 t

-- 6. streamFromList roundtrip: concat of chunks
prop_streamFromListRoundtrip :: Property
prop_streamFromListRoundtrip = property $ do
  chunks <- forAll $ Gen.list (Range.linear 0 10) genByteString
  result <- evalIO $ do
    body <- streamFromList chunks
    bodyToStrict body
  result === BS.concat chunks

-- 7. isEmptyBody correctness
prop_isEmptyBodyCorrect :: Property
prop_isEmptyBodyCorrect = withTests 1 $ property $ do
  assert (isEmptyBody emptyBody)
  assert (isEmptyBody (fromBytes ""))
  assert (not (isEmptyBody (fromBytes "x")))

-- 8. splitRequest preserves method, path, query, headers
prop_splitRequestPreservesFields :: Property
prop_splitRequestPreservesFields = property $ do
  method  <- forAll genMethod
  path    <- forAll genPath
  query   <- forAll genQuery
  headers <- forAll genHeaders
  body    <- forAll genByteString

  let pathRaw = TE.encodeUtf8 ("/" <> T.intercalate "/" path)

  (parts, splitBody) <- evalIO $ do
    exts <- emptyExtensions
    let req = Request
          { requestMethod     = method
          , requestPathRaw    = pathRaw
          , requestPath       = path
          , requestQuery      = query
          , requestHeaders    = headers
          , requestBody       = body
          , requestExtensions = exts
          }
    pure (splitRequest req)

  rpMethod parts === method
  rpPathRaw parts === pathRaw
  rpPath parts === path
  rpQuery parts === query
  rpHeaders parts === headers
  splitBody === body


-- ===================================================================
-- Main — TemplateHaskell discovery
-- ===================================================================

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  passed <- tests
  if passed
    then pure ()
    else error "Property tests failed"
