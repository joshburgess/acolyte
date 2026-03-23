{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Tower.Protobuf
import Tower.Protobuf.Wire (encodeFixed32, encodeFixed64, decodeVarint, decodeFixed32,
                            decodeFixed64, encodeLengthDelimited, decodeLengthDelimited,
                            zigzagEncode, zigzagDecode)
import Tower.Protobuf.Encode (runProtoBuilder, ProtoBuilder(..))


-- ===================================================================
-- Test message types
-- ===================================================================

data Simple = Simple
  { sName :: Field 1 Text
  , sAge  :: Field 2 Int32
  } deriving (Show, Eq, Generic)

instance ProtoMessage Simple

data WithOpt = WithOpt
  { wName :: Field 1 Text
  , wNick :: Field 2 (Maybe Text)
  } deriving (Show, Eq, Generic)

instance ProtoMessage WithOpt

data AllDefaults = AllDefaults
  { dName :: Field 1 Text
  , dAge  :: Field 2 Int32
  , dFlag :: Field 3 Bool
  } deriving (Show, Eq, Generic)

instance ProtoMessage AllDefaults


-- ===================================================================
-- Generators
-- ===================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 200) Gen.unicode

genSimple :: Gen Simple
genSimple = Simple
  <$> (Field <$> genText)
  <*> (Field <$> Gen.int32 Range.linearBounded)

genWithOpt :: Gen WithOpt
genWithOpt = WithOpt
  <$> (Field <$> genText)
  <*> (Field <$> Gen.maybe genText)


-- ===================================================================
-- Properties
-- ===================================================================

-- | For any Word64, decoding the varint encoding gives back the original.
prop_varintRoundtrip :: Property
prop_varintRoundtrip = property $ do
  n <- forAll $ Gen.word64 Range.linearBounded
  let bs = runProtoBuilder (protoEncodeValue @Word64 n)
  case decodeVarint bs of
    Just (v, rest) -> do
      v === n
      assert (BS.null rest)
    Nothing -> failure

-- | For any Int64, zigzagDecode (zigzagEncode n) == n.
prop_zigzagRoundtrip :: Property
prop_zigzagRoundtrip = property $ do
  n <- forAll $ Gen.int64 Range.linearBounded
  zigzagDecode (zigzagEncode n) === n

-- | For any Word32, fixed32 roundtrip works.
prop_fixed32Roundtrip :: Property
prop_fixed32Roundtrip = property $ do
  n <- forAll $ Gen.word32 Range.linearBounded
  let bs = runProtoBuilder (ProtoBuilder 4 (encodeFixed32 n))
  case decodeFixed32 bs of
    Just (v, rest) -> do
      v === n
      assert (BS.null rest)
    Nothing -> failure

-- | For any Word64, fixed64 roundtrip works.
prop_fixed64Roundtrip :: Property
prop_fixed64Roundtrip = property $ do
  n <- forAll $ Gen.word64 Range.linearBounded
  let bs = runProtoBuilder (ProtoBuilder 8 (encodeFixed64 n))
  case decodeFixed64 bs of
    Just (v, rest) -> do
      v === n
      assert (BS.null rest)
    Nothing -> failure

-- | For any ByteString, length-delimited roundtrip works.
prop_lengthDelimitedRoundtrip :: Property
prop_lengthDelimitedRoundtrip = property $ do
  input <- forAll $ Gen.bytes (Range.linear 0 5000)
  let len = BS.length input
      varintLen n
        | n < 0x80       = 1
        | n < 0x4000     = 2
        | n < 0x200000   = 3
        | otherwise      = 4
      totalLen = varintLen len + len
      bs = runProtoBuilder (ProtoBuilder totalLen (encodeLengthDelimited input))
  case decodeLengthDelimited bs of
    Just (v, rest) -> do
      v === input
      assert (BS.null rest)
    Nothing -> failure

-- | For a Simple message with random name/age, decode (encode msg) == Right msg.
prop_messageRoundtrip :: Property
prop_messageRoundtrip = property $ do
  msg <- forAll genSimple
  decode (encode msg) === Right msg

-- | For WithOpt with random Maybe Text, roundtrip works.
prop_optionalFieldRoundtrip :: Property
prop_optionalFieldRoundtrip = property $ do
  msg <- forAll genWithOpt
  decode (encode msg) === Right msg

-- | A message with all-default values encodes to empty bytes.
prop_defaultsProduceEmptyEncoding :: Property
prop_defaultsProduceEmptyEncoding = property $ do
  let msg = AllDefaults (Field "") (Field 0) (Field False)
  encode msg === BS.empty


-- ===================================================================
-- Main (TemplateHaskell discovery)
-- ===================================================================

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  ok <- tests
  if ok
    then putStrLn "All property tests passed!"
    else do
      putStrLn "Some property tests FAILED!"
      error "Property test failure"
