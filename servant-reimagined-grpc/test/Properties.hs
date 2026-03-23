{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Servant.Reimagined.Core
import Servant.Reimagined.Server (Json)
import Servant.Reimagined.Grpc


-- ===================================================================
-- Test type with trivial GrpcCodec (identity on ByteString)
-- ===================================================================

newtype TestMsg = TestMsg ByteString
  deriving (Eq, Show)

instance GrpcCodec TestMsg where
  grpcEncode (TestMsg bs) = bs
  grpcDecode bs = Just (TestMsg bs)


-- ===================================================================
-- Property: GrpcCodec roundtrip
-- ===================================================================

prop_grpcCodecRoundtrip :: Property
prop_grpcCodecRoundtrip = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 0 200)
  let msg = TestMsg bs
  grpcDecode (grpcEncode msg) === Just msg


-- ===================================================================
-- Property: Proto generation produces valid syntax
-- ===================================================================

-- Test types for proto generation
data TestUser = TestUser !Text
  deriving (Eq, Show)

instance GrpcCodec TestUser where
  grpcEncode (TestUser t) = encodeText t
  grpcDecode bs = Just (TestUser (decodeText bs))

instance ProtoMessageName TestUser where
  protoMessageName = "TestUser"

instance GrpcCodec Text where
  grpcEncode = encodeText
  grpcDecode = Just . decodeText

instance ProtoMessageName Text where
  protoMessageName = "StringValue"

-- Simple UTF-8 helpers (matching the existing test pattern)
encodeText :: Text -> ByteString
encodeText t = BS.pack (map (fromIntegral . fromEnum) (T.unpack t))

decodeText :: ByteString -> Text
decodeText = T.pack . map (toEnum . fromIntegral) . BS.unpack

-- API for proto generation test
type TestPath = '[ 'Lit "test" ]
type TestAPI = '[ Get TestPath Text
                , Post TestPath (Json TestUser) (Json TestUser)
                ]

prop_protoGenerationValid :: Property
prop_protoGenerationValid = property $ do
  -- Generate with varying package and service names
  pkg <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
  svc <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha

  let proto = generateProto @TestAPI pkg svc

  -- Must start with proto3 syntax
  assert $ T.isPrefixOf "syntax = \"proto3\";" proto

  -- Must contain package declaration
  assert $ T.isInfixOf ("package " <> pkg <> ";") proto

  -- Must contain service declaration
  assert $ T.isInfixOf ("service " <> svc <> " {") proto

  -- Must have balanced braces
  let opens  = T.count "{" proto
      closes = T.count "}" proto
  opens === closes


-- ===================================================================
-- Main
-- ===================================================================

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  ok <- tests
  if ok then pure () else error "Property tests failed"
