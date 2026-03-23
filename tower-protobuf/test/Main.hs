{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

import Tower.Protobuf
import Tower.Protobuf.Wire
import Tower.Protobuf.Encode
import Tower.Protobuf.Decode


-- ===================================================================
-- Test helpers
-- ===================================================================

assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label

-- | Helper: build a ByteString from a Builder-producing function
buildBS :: ProtoBuilder -> ByteString
buildBS = runProtoBuilder


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

data WithList = WithList
  { wTags :: Field 1 [Text]
  } deriving (Show, Eq, Generic)

instance ProtoMessage WithList

data AllDefaults = AllDefaults
  { dName :: Field 1 Text
  , dAge  :: Field 2 Int32
  , dFlag :: Field 3 Bool
  } deriving (Show, Eq, Generic)

instance ProtoMessage AllDefaults


-- ===================================================================
-- 1. Wire primitives: Varint
-- ===================================================================

testVarintRoundtrip :: IO ()
testVarintRoundtrip = do
  putStrLn "=== Varint roundtrip ==="
  let check val = do
        let bs = buildBS (ProtoBuilder (varintSz val) (encodeVarint val))
        case decodeVarint bs of
          Just (v, rest) -> do
            assert ("varint roundtrip " ++ show val) (v == val)
            assert ("varint no leftover " ++ show val) (BS.null rest)
          Nothing -> error $ "FAIL: varint decode failed for " ++ show val
  check 0
  check 1
  check 127
  check 128
  check 300
  check (fromIntegral (maxBound @Word32) :: Word64)
  check (maxBound @Word64)
  where
    varintSz :: Word64 -> Int
    varintSz n
      | n < 0x80       = 1
      | n < 0x4000     = 2
      | n < 0x200000   = 3
      | n < 0x10000000 = 4
      | n < 0x0800000000 = 5
      | n < 0x040000000000 = 6
      | n < 0x02000000000000 = 7
      | n < 0x0100000000000000 = 8
      | n < 0x8000000000000000 = 9
      | otherwise = 10

-- ===================================================================
-- 2. Wire primitives: Zigzag
-- ===================================================================

testZigzagRoundtrip :: IO ()
testZigzagRoundtrip = do
  putStrLn "=== Zigzag roundtrip ==="
  let check val = assert ("zigzag roundtrip " ++ show val)
                         (zigzagDecode (zigzagEncode val) == val)
  check 0
  check (-1)
  check 1
  check (-2)
  check 2
  check (minBound @Int64)
  check (maxBound @Int64)


-- ===================================================================
-- 3. Wire primitives: Fixed32/Fixed64
-- ===================================================================

testFixed32Roundtrip :: IO ()
testFixed32Roundtrip = do
  putStrLn "=== Fixed32 roundtrip ==="
  let check val = do
        let bs = buildBS (ProtoBuilder 4 (encodeFixed32 val))
        case decodeFixed32 bs of
          Just (v, rest) -> do
            assert ("fixed32 roundtrip " ++ show val) (v == val)
            assert ("fixed32 no leftover " ++ show val) (BS.null rest)
          Nothing -> error $ "FAIL: fixed32 decode failed for " ++ show val
  check 0
  check 1
  check 255
  check 65535
  check (maxBound @Word32)

testFixed64Roundtrip :: IO ()
testFixed64Roundtrip = do
  putStrLn "=== Fixed64 roundtrip ==="
  let check val = do
        let bs = buildBS (ProtoBuilder 8 (encodeFixed64 val))
        case decodeFixed64 bs of
          Just (v, rest) -> do
            assert ("fixed64 roundtrip " ++ show val) (v == val)
            assert ("fixed64 no leftover " ++ show val) (BS.null rest)
          Nothing -> error $ "FAIL: fixed64 decode failed for " ++ show val
  check 0
  check 1
  check (maxBound @Word64)
  check 0xDEADBEEFCAFEBABE


-- ===================================================================
-- 4. Wire primitives: Float/Double
-- ===================================================================

testFloatRoundtrip :: IO ()
testFloatRoundtrip = do
  putStrLn "=== Float roundtrip ==="
  let check val = do
        let bs = buildBS (ProtoBuilder 4 (encodeProtoFloat val))
        case decodeProtoFloat bs of
          Just (v, rest) -> do
            assert ("float roundtrip " ++ show val) (v == val)
            assert ("float no leftover " ++ show val) (BS.null rest)
          Nothing -> error $ "FAIL: float decode failed for " ++ show val
  check 0.0
  check 1.5
  check (-3.14)
  check (1.0 / 0.0)  -- infinity

  -- NaN: NaN /= NaN, so test with isNaN
  let nanBs = buildBS (ProtoBuilder 4 (encodeProtoFloat (0.0 / 0.0)))
  case decodeProtoFloat nanBs of
    Just (v, rest) -> do
      assert "float NaN roundtrip" (isNaN v)
      assert "float NaN no leftover" (BS.null rest)
    Nothing -> error "FAIL: float NaN decode failed"

testDoubleRoundtrip :: IO ()
testDoubleRoundtrip = do
  putStrLn "=== Double roundtrip ==="
  let check val = do
        let bs = buildBS (ProtoBuilder 8 (encodeProtoDouble val))
        case decodeProtoDouble bs of
          Just (v, rest) -> do
            assert ("double roundtrip " ++ show val) (v == val)
            assert ("double no leftover " ++ show val) (BS.null rest)
          Nothing -> error $ "FAIL: double decode failed for " ++ show val
  check 0.0
  check 1.5
  check (-3.14)
  check (1.0 / 0.0)

  let nanBs = buildBS (ProtoBuilder 8 (encodeProtoDouble (0.0 / 0.0)))
  case decodeProtoDouble nanBs of
    Just (v, rest) -> do
      assert "double NaN roundtrip" (isNaN v)
      assert "double NaN no leftover" (BS.null rest)
    Nothing -> error "FAIL: double NaN decode failed"


-- ===================================================================
-- 5. Wire primitives: Tags
-- ===================================================================

testTagEncodeDecode :: IO ()
testTagEncodeDecode = do
  putStrLn "=== Tag encode/decode ==="

  -- field 1, varint
  let bs1 = buildBS (ProtoBuilder 1 (encodeTag 1 WireVarint))
  case decodeTag bs1 of
    Just (fn, wt, rest) -> do
      assert "tag field 1 varint: field number" (fn == 1)
      assert "tag field 1 varint: wire type" (wt == WireVarint)
      assert "tag field 1 varint: no leftover" (BS.null rest)
    Nothing -> error "FAIL: tag decode failed"

  -- field 15, length-delimited
  let bs2 = buildBS (ProtoBuilder 1 (encodeTag 15 WireLengthDelimited))
  case decodeTag bs2 of
    Just (fn, wt, rest) -> do
      assert "tag field 15 ld: field number" (fn == 15)
      assert "tag field 15 ld: wire type" (wt == WireLengthDelimited)
      assert "tag field 15 ld: no leftover" (BS.null rest)
    Nothing -> error "FAIL: tag decode failed"

  -- field 536870911 (max field number: 2^29 - 1)
  let bs3 = buildBS (ProtoBuilder 5 (encodeTag 536870911 WireVarint))
  case decodeTag bs3 of
    Just (fn, wt, _) -> do
      assert "tag max field: field number" (fn == 536870911)
      assert "tag max field: wire type" (wt == WireVarint)
    Nothing -> error "FAIL: tag decode failed for max field"


-- ===================================================================
-- 6. Wire primitives: Length-delimited
-- ===================================================================

testLengthDelimitedRoundtrip :: IO ()
testLengthDelimitedRoundtrip = do
  putStrLn "=== Length-delimited roundtrip ==="

  -- empty
  let emptyBs = buildBS (ProtoBuilder 1 (encodeLengthDelimited BS.empty))
  case decodeLengthDelimited emptyBs of
    Just (val, rest) -> do
      assert "length-delimited empty: value" (val == BS.empty)
      assert "length-delimited empty: no leftover" (BS.null rest)
    Nothing -> error "FAIL: length-delimited empty decode failed"

  -- "hello"
  let hello = "hello" :: ByteString
      helloBs = buildBS (ProtoBuilder (1 + BS.length hello) (encodeLengthDelimited hello))
  case decodeLengthDelimited helloBs of
    Just (val, rest) -> do
      assert "length-delimited hello: value" (val == hello)
      assert "length-delimited hello: no leftover" (BS.null rest)
    Nothing -> error "FAIL: length-delimited hello decode failed"

  -- 1000 bytes
  let bigBs = BS.replicate 1000 0x42
      bigEncoded = buildBS (ProtoBuilder (2 + 1000) (encodeLengthDelimited bigBs))
  case decodeLengthDelimited bigEncoded of
    Just (val, rest) -> do
      assert "length-delimited 1000 bytes: value" (val == bigBs)
      assert "length-delimited 1000 bytes: no leftover" (BS.null rest)
    Nothing -> error "FAIL: length-delimited 1000 bytes decode failed"


-- ===================================================================
-- 7. Wire primitives: Skip field
-- ===================================================================

testSkipField :: IO ()
testSkipField = do
  putStrLn "=== Skip field ==="

  -- skip varint
  let varintBs = buildBS (ProtoBuilder 1 (encodeVarint 42)) <> "tail"
  case skipField WireVarint varintBs of
    Just rest -> assert "skip varint" (rest == "tail")
    Nothing -> error "FAIL: skip varint failed"

  -- skip fixed32
  let f32Bs = buildBS (ProtoBuilder 4 (encodeFixed32 0)) <> "tail"
  case skipField WireFixed32 f32Bs of
    Just rest -> assert "skip fixed32" (rest == "tail")
    Nothing -> error "FAIL: skip fixed32 failed"

  -- skip fixed64
  let f64Bs = buildBS (ProtoBuilder 8 (encodeFixed64 0)) <> "tail"
  case skipField WireFixed64 f64Bs of
    Just rest -> assert "skip fixed64" (rest == "tail")
    Nothing -> error "FAIL: skip fixed64 failed"

  -- skip length-delimited
  let ldBs = buildBS (ProtoBuilder 6 (encodeLengthDelimited "hello")) <> "tail"
  case skipField WireLengthDelimited ldBs of
    Just rest -> assert "skip length-delimited" (rest == "tail")
    Nothing -> error "FAIL: skip length-delimited failed"


-- ===================================================================
-- 8. Encoding: ProtoEncode Int32
-- ===================================================================

testEncodeInt32 :: IO ()
testEncodeInt32 = do
  putStrLn "=== ProtoEncode Int32 ==="

  -- Non-default encodes as varint
  let pb = protoEncodeValue @Int32 42
  assert "Int32 42: encodes non-empty" (pbLength pb > 0)

  -- Field encoding: tag + value
  let fpb = encodeField (Field 42 :: Field 1 Int32)
  assert "Int32 field 1 42: encodes non-empty" (pbLength fpb > 0)

  -- Default (0) is skipped
  let defPb = encodeField (Field 0 :: Field 1 Int32)
  assert "Int32 default (0) is skipped" (pbLength defPb == 0)


-- ===================================================================
-- 9. Encoding: ProtoEncode Bool
-- ===================================================================

testEncodeBool :: IO ()
testEncodeBool = do
  putStrLn "=== ProtoEncode Bool ==="

  -- True -> varint 1
  let truePb = protoEncodeValue True
  let trueBytes = runProtoBuilder truePb
  case decodeVarint trueBytes of
    Just (v, _) -> assert "Bool True encodes to varint 1" (v == 1)
    Nothing -> error "FAIL: Bool True encode failed"

  -- False -> field is skipped (default)
  let defPb = encodeField (Field False :: Field 1 Bool)
  assert "Bool False (default) is skipped" (pbLength defPb == 0)


-- ===================================================================
-- 10. Encoding: ProtoEncode Text
-- ===================================================================

testEncodeText :: IO ()
testEncodeText = do
  putStrLn "=== ProtoEncode Text ==="

  let pb = protoEncodeValue ("hello" :: Text)
  -- length-delimited: varint(5) + "hello" = 6 bytes
  assert "Text 'hello': length 6" (pbLength pb == 6)

  -- Field encoding
  let fpb = encodeField (Field "hello" :: Field 1 Text)
  assert "Text field non-empty" (pbLength fpb > 0)

  -- Default ("") is skipped
  let defPb = encodeField (Field "" :: Field 1 Text)
  assert "Text default ('') is skipped" (pbLength defPb == 0)


-- ===================================================================
-- 11. Encoding: ProtoEncode ByteString
-- ===================================================================

testEncodeByteString :: IO ()
testEncodeByteString = do
  putStrLn "=== ProtoEncode ByteString ==="

  let bs = "world" :: ByteString
      pb = protoEncodeValue bs
  -- varint(5) + "world" = 6 bytes
  assert "ByteString 'world': length 6" (pbLength pb == 6)

  -- Default (empty) is skipped
  let defPb = encodeField (Field BS.empty :: Field 1 ByteString)
  assert "ByteString default (empty) is skipped" (pbLength defPb == 0)


-- ===================================================================
-- 12. Encoding: Optional and Repeated fields
-- ===================================================================

testEncodeOptionalField :: IO ()
testEncodeOptionalField = do
  putStrLn "=== Optional field encoding ==="

  -- Nothing -> empty
  let nothingPb = encodeOptionalField (Field Nothing :: Field 1 (Maybe Text))
  assert "Nothing encodes to empty" (pbLength nothingPb == 0)

  -- Just val -> field
  let justPb = encodeOptionalField (Field (Just "hi") :: Field 1 (Maybe Text))
  assert "Just 'hi' encodes non-empty" (pbLength justPb > 0)

testEncodeRepeatedField :: IO ()
testEncodeRepeatedField = do
  putStrLn "=== Repeated field encoding ==="

  -- Empty list -> empty
  let emptyPb = encodeRepeatedField (Field [] :: Field 1 [Text])
  assert "empty list encodes to empty" (pbLength emptyPb == 0)

  -- Non-empty list
  let listPb = encodeRepeatedField (Field ["a", "b", "c"] :: Field 1 [Text])
  assert "list [a,b,c] encodes non-empty" (pbLength listPb > 0)


-- ===================================================================
-- 13. Encoding: ProtoBuilder length tracking
-- ===================================================================

testProtoBuilderLength :: IO ()
testProtoBuilderLength = do
  putStrLn "=== ProtoBuilder length tracking ==="

  -- Build several values and verify pbLength matches actual encoded bytes
  let pb1 = protoEncodeValue @Int32 42
      bs1 = runProtoBuilder pb1
  assert "Int32 42: pbLength matches actual" (pbLength pb1 == BS.length bs1)

  let pb2 = protoEncodeValue ("hello world" :: Text)
      bs2 = runProtoBuilder pb2
  assert "Text: pbLength matches actual" (pbLength pb2 == BS.length bs2)

  let pb3 = protoEncodeValue @Double 3.14
      bs3 = runProtoBuilder pb3
  assert "Double: pbLength matches actual" (pbLength pb3 == BS.length bs3)

  -- Combined
  let combined = pb1 <> pb2 <> pb3
      bsAll = runProtoBuilder combined
  assert "combined: pbLength matches actual" (pbLength combined == BS.length bsAll)


-- ===================================================================
-- 14. Decoding: ProtoDecode Int32
-- ===================================================================

testDecodeInt32 :: IO ()
testDecodeInt32 = do
  putStrLn "=== ProtoDecode Int32 ==="

  assert "Int32 from varint 42" (protoDecodeValue @Int32 (RawVarint 42) == Right 42)
  assert "Int32 from varint 0"  (protoDecodeValue @Int32 (RawVarint 0) == Right 0)

  -- Wrong wire type
  case protoDecodeValue @Int32 (RawBytes "hello") of
    Left _ -> assert "Int32 rejects RawBytes" True
    Right _ -> assert "Int32 rejects RawBytes" False


-- ===================================================================
-- 15. Decoding: ProtoDecode Text
-- ===================================================================

testDecodeText :: IO ()
testDecodeText = do
  putStrLn "=== ProtoDecode Text ==="

  assert "Text from RawBytes 'hello'" (protoDecodeValue @Text (RawBytes "hello") == Right "hello")
  assert "Text from RawBytes empty"   (protoDecodeValue @Text (RawBytes "") == Right "")

  -- Invalid UTF-8
  let badUtf8 = BS.pack [0xFF, 0xFE]
  case protoDecodeValue @Text (RawBytes badUtf8) of
    Left InvalidUtf8 -> assert "Text rejects invalid UTF-8" True
    _                -> assert "Text rejects invalid UTF-8" False


-- ===================================================================
-- 16. Decoding: ProtoDecode ByteString
-- ===================================================================

testDecodeByteString :: IO ()
testDecodeByteString = do
  putStrLn "=== ProtoDecode ByteString ==="

  assert "ByteString from RawBytes" (protoDecodeValue @ByteString (RawBytes "hello") == Right "hello")
  assert "ByteString from RawBytes empty" (protoDecodeValue @ByteString (RawBytes "") == Right "")


-- ===================================================================
-- 17. Decoding: parseFields
-- ===================================================================

testParseFields :: IO ()
testParseFields = do
  putStrLn "=== parseFields ==="

  -- Encode a Simple message and parse the raw fields
  let msg = Simple (Field "Alice") (Field 30)
      bs = encode msg
  case parseFields bs of
    Right fields -> do
      assert "parseFields: 2 fields" (length fields == 2)
      -- field 1 should be RawBytes (text)
      case lookup 1 fields of
        Just (RawBytes v) -> assert "parseFields: field 1 is 'Alice'" (v == "Alice")
        _ -> assert "parseFields: field 1 is RawBytes" False
      -- field 2 should be RawVarint
      case lookup 2 fields of
        Just (RawVarint v) -> assert "parseFields: field 2 is 30" (v == 30)
        _ -> assert "parseFields: field 2 is RawVarint" False
    Left err -> error $ "FAIL: parseFields failed: " ++ show err

  -- Empty input
  case parseFields BS.empty of
    Right fields -> assert "parseFields: empty input -> empty list" (null fields)
    Left err -> error $ "FAIL: parseFields empty failed: " ++ show err


-- ===================================================================
-- 18. Decoding: Unknown fields skipped
-- ===================================================================

testUnknownFields :: IO ()
testUnknownFields = do
  putStrLn "=== Unknown fields skipped ==="

  -- Encode a Simple message, then try to decode it as AllDefaults
  -- (which expects fields 1, 2, 3). Since Simple only has fields 1, 2,
  -- field 3 will use its default.
  let msg = Simple (Field "Bob") (Field 25)
      bs = encode msg
  case decode @AllDefaults bs of
    Right ad -> do
      assert "unknown fields: name preserved" (unField (dName ad) == "Bob")
      assert "unknown fields: age preserved" (unField (dAge ad) == 25)
      assert "unknown fields: flag defaults to False" (unField (dFlag ad) == False)
    Left err -> error $ "FAIL: unknown fields decode failed: " ++ show err


-- ===================================================================
-- 19. ProtoMessage: Simple roundtrip
-- ===================================================================

testSimpleRoundtrip :: IO ()
testSimpleRoundtrip = do
  putStrLn "=== Simple message roundtrip ==="

  let msg = Simple (Field "Alice") (Field 30)
      bs = encode msg
  case decode bs of
    Right msg' -> assert "Simple roundtrip" (msg == msg')
    Left err -> error $ "FAIL: Simple roundtrip: " ++ show err


-- ===================================================================
-- 20. ProtoMessage: Optional fields
-- ===================================================================

testOptionalRoundtrip :: IO ()
testOptionalRoundtrip = do
  putStrLn "=== Optional fields roundtrip ==="

  -- With value
  let msg1 = WithOpt (Field "Alice") (Field (Just "Ali"))
  case decode (encode msg1) of
    Right msg1' -> assert "WithOpt Just roundtrip" (msg1 == msg1')
    Left err -> error $ "FAIL: WithOpt Just roundtrip: " ++ show err

  -- Without value
  let msg2 = WithOpt (Field "Bob") (Field Nothing)
  case decode (encode msg2) of
    Right msg2' -> assert "WithOpt Nothing roundtrip" (msg2 == msg2')
    Left err -> error $ "FAIL: WithOpt Nothing roundtrip: " ++ show err


-- ===================================================================
-- 21. ProtoMessage: Repeated fields
-- ===================================================================

testRepeatedRoundtrip :: IO ()
testRepeatedRoundtrip = do
  putStrLn "=== Repeated fields roundtrip ==="

  let msg = WithList (Field ["alpha", "beta", "gamma"])
  case decode (encode msg) of
    Right msg' -> assert "WithList roundtrip" (msg == msg')
    Left err -> error $ "FAIL: WithList roundtrip: " ++ show err

  -- Empty list
  let msgEmpty = WithList (Field [])
  case decode (encode msgEmpty) of
    Right msg' -> assert "WithList empty roundtrip" (msg' == msgEmpty)
    Left err -> error $ "FAIL: WithList empty roundtrip: " ++ show err


-- ===================================================================
-- 22. ProtoMessage: All-default values
-- ===================================================================

testDefaultsEncoding :: IO ()
testDefaultsEncoding = do
  putStrLn "=== Default values encoding ==="

  let msg = AllDefaults (Field "") (Field 0) (Field False)
      bs = encode msg
  assert "all-default message encodes to empty bytes" (BS.null bs)


-- ===================================================================
-- 23. ProtoMessage: Missing fields use defaults
-- ===================================================================

testMissingFieldsDefault :: IO ()
testMissingFieldsDefault = do
  putStrLn "=== Missing fields use defaults ==="

  -- Decode empty bytes as AllDefaults
  case decode @AllDefaults BS.empty of
    Right ad -> do
      assert "missing field: name defaults to ''" (unField (dName ad) == "")
      assert "missing field: age defaults to 0" (unField (dAge ad) == 0)
      assert "missing field: flag defaults to False" (unField (dFlag ad) == False)
    Left err -> error $ "FAIL: missing fields decode: " ++ show err


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "tower-protobuf unit tests"
  putStrLn (replicate 40 '-')

  testVarintRoundtrip
  testZigzagRoundtrip
  testFixed32Roundtrip
  testFixed64Roundtrip
  testFloatRoundtrip
  testDoubleRoundtrip
  testTagEncodeDecode
  testLengthDelimitedRoundtrip
  testSkipField
  testEncodeInt32
  testEncodeBool
  testEncodeText
  testEncodeByteString
  testEncodeOptionalField
  testEncodeRepeatedField
  testProtoBuilderLength
  testDecodeInt32
  testDecodeText
  testDecodeByteString
  testParseFields
  testUnknownFields
  testSimpleRoundtrip
  testOptionalRoundtrip
  testRepeatedRoundtrip
  testDefaultsEncoding
  testMissingFieldsDefault

  putStrLn (replicate 40 '-')
  putStrLn "All tests passed!"
