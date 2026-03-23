{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
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

data Address = Address
  { city :: Field 1 Text
  , addrZip :: Field 2 Int32
  } deriving (Show, Eq, Generic)

instance ProtoMessage Address

data Person = Person
  { pName :: Field 1 Text
  , pAddr :: Field 2 (Maybe Address)
  } deriving (Show, Eq, Generic)

instance ProtoMessage Person

data WithRepeatedInt = WithRepeatedInt
  { riName :: Field 1 Text
  , riNums :: Field 2 [Int32]
  } deriving (Show, Eq, Generic)

instance ProtoMessage WithRepeatedInt


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
-- 24. Nested message roundtrip
-- ===================================================================

testNestedMessageRoundtrip :: IO ()
testNestedMessageRoundtrip = do
  putStrLn "=== Nested message roundtrip ==="

  let addr = Address (Field "Springfield") (Field 62704)
      person = Person (Field "Homer") (Field (Just addr))
      bs = encode person
  case decode bs of
    Right person' -> assert "nested message roundtrip" (person == person')
    Left err -> error $ "FAIL: nested message roundtrip: " ++ show err

  -- Nested with Nothing
  let person2 = Person (Field "Bart") (Field Nothing)
  case decode (encode person2) of
    Right person2' -> assert "nested message Nothing roundtrip" (person2 == person2')
    Left err -> error $ "FAIL: nested Nothing roundtrip: " ++ show err


-- ===================================================================
-- 25. Out-of-order field decoding
-- ===================================================================

testOutOfOrderDecoding :: IO ()
testOutOfOrderDecoding = do
  putStrLn "=== Out-of-order field decoding ==="

  -- Manually build bytes with field 2 (Int32, varint) before field 1 (Text, length-delimited)
  -- Field 2: tag = (2 << 3 | 0) = 16 = 0x10, value = varint 25
  -- Field 1: tag = (1 << 3 | 2) = 10 = 0x0A, value = length-delimited "Alice"
  let field2Bytes = buildBS (ProtoBuilder 1 (encodeTag 2 WireVarint))
                 <> buildBS (ProtoBuilder 1 (encodeVarint 25))
      field1Bytes = buildBS (ProtoBuilder 1 (encodeTag 1 WireLengthDelimited))
                 <> buildBS (ProtoBuilder 6 (encodeLengthDelimited "Alice"))
      outOfOrder = field2Bytes <> field1Bytes
  case decode @Simple outOfOrder of
    Right msg -> do
      assert "out-of-order: name is Alice" (unField (sName msg) == "Alice")
      assert "out-of-order: age is 25" (unField (sAge msg) == 25)
    Left err -> error $ "FAIL: out-of-order decode: " ++ show err


-- ===================================================================
-- 26. Duplicate field (last-wins)
-- ===================================================================

testDuplicateFieldLastWins :: IO ()
testDuplicateFieldLastWins = do
  putStrLn "=== Duplicate field (last-wins) ==="

  -- Encode field 1 as "first", then field 2 as 10, then field 1 again as "second"
  let f1first = buildBS (ProtoBuilder 1 (encodeTag 1 WireLengthDelimited))
             <> buildBS (ProtoBuilder 6 (encodeLengthDelimited "first"))
      f2val   = buildBS (ProtoBuilder 1 (encodeTag 2 WireVarint))
             <> buildBS (ProtoBuilder 1 (encodeVarint 10))
      f1second = buildBS (ProtoBuilder 1 (encodeTag 1 WireLengthDelimited))
              <> buildBS (ProtoBuilder 7 (encodeLengthDelimited "second"))
      combined = f1first <> f2val <> f1second
  case decode @Simple combined of
    Right msg -> do
      assert "duplicate field last-wins: name is 'second'" (unField (sName msg) == "second")
      assert "duplicate field last-wins: age is 10" (unField (sAge msg) == 10)
    Left err -> error $ "FAIL: duplicate field decode: " ++ show err


-- ===================================================================
-- 27. Malformed input tests
-- ===================================================================

testMalformedInput :: IO ()
testMalformedInput = do
  putStrLn "=== Malformed input tests ==="

  -- Truncated varint: 0x80 has continuation bit set but no following byte
  let truncVarint = BS.pack [0x80]
  case decode @Simple truncVarint of
    Left _ -> assert "truncated varint returns Left" True
    Right _ -> assert "truncated varint returns Left" False

  -- Truncated length-delimited: tag for field 1 string + varint length 100 + only 5 bytes
  let tagBytes = buildBS (ProtoBuilder 1 (encodeTag 1 WireLengthDelimited))
      lenBytes = buildBS (ProtoBuilder 1 (encodeVarint 100))
      dataBytes = BS.replicate 5 0x41
      truncLD = tagBytes <> lenBytes <> dataBytes
  case decode @Simple truncLD of
    Left _ -> assert "truncated length-delimited returns Left" True
    Right _ -> assert "truncated length-delimited returns Left" False

  -- Empty input: decode as Simple -> returns message with all defaults
  case decode @Simple BS.empty of
    Right msg -> do
      assert "empty input: name defaults to ''" (unField (sName msg) == "")
      assert "empty input: age defaults to 0" (unField (sAge msg) == 0)
    Left err -> error $ "FAIL: empty input decode: " ++ show err

  -- Single byte 0xFF -> decode returns Left (invalid tag: wire type 7 is unknown)
  let badByte = BS.pack [0xFF]
  case decode @Simple badByte of
    Left _ -> assert "single byte 0xFF returns Left" True
    Right _ -> assert "single byte 0xFF returns Left" False


-- ===================================================================
-- 28. Overlong varint rejection
-- ===================================================================

testOverlongVarintRejection :: IO ()
testOverlongVarintRejection = do
  putStrLn "=== Overlong varint rejection ==="

  -- Construct a 10-byte varint where byte 10 has bits 1-6 set.
  -- 9 continuation bytes (0x80) + final byte with high bits: 0x7E (bits 1-6 set)
  -- This represents a varint that overflows the 64-bit space.
  let overlongVarint = BS.pack [0x80, 0x80, 0x80, 0x80, 0x80,
                                0x80, 0x80, 0x80, 0x80, 0x7E]
  case decodeVarint overlongVarint of
    Nothing -> assert "overlong varint rejected" True
    Just _  -> assert "overlong varint rejected" False

  -- A valid 10-byte varint (max Word64) should still decode
  -- maxBound :: Word64 = 0xFFFFFFFFFFFFFFFF
  -- As varint: 9 bytes of 0xFF + final byte 0x01
  let maxVarint = BS.pack [0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                           0xFF, 0xFF, 0xFF, 0xFF, 0x01]
  case decodeVarint maxVarint of
    Just (v, rest) -> do
      assert "max varint decodes correctly" (v == maxBound @Word64)
      assert "max varint no leftover" (BS.null rest)
    Nothing -> assert "max varint decodes correctly" False


-- ===================================================================
-- 29. Zigzag encoding of negative values
-- ===================================================================

testZigzagNegativeEfficiency :: IO ()
testZigzagNegativeEfficiency = do
  putStrLn "=== Zigzag negative value efficiency ==="

  -- Small negatives should produce small zigzag-encoded values
  assert "zigzag(-1) = 1" (zigzagEncode (-1) == 1)
  assert "zigzag(-2) = 3" (zigzagEncode (-2) == 3)
  assert "zigzag(-100) = 199" (zigzagEncode (-100) == 199)

  -- Positive values should also be small
  assert "zigzag(0) = 0" (zigzagEncode 0 == 0)
  assert "zigzag(1) = 2" (zigzagEncode 1 == 2)
  assert "zigzag(100) = 200" (zigzagEncode 100 == 200)

  -- Verify zigzag of small negatives uses fewer bytes than plain int encoding
  -- -1 as Int32 on wire (varint) is 10 bytes (sign-extended to Word64)
  -- -1 as zigzag -> 1 -> 1 byte
  let plainNeg1Bytes = BS.length $ runProtoBuilder (protoEncodeValue @Int32 (-1))
      zigzagNeg1 = zigzagEncode (-1)
      zigzagVarintSz n
        | n < 0x80       = 1
        | n < 0x4000     = 2
        | otherwise      = 3
  assert "zigzag(-1) more compact than plain Int32(-1)"
    (zigzagVarintSz zigzagNeg1 < plainNeg1Bytes)


-- ===================================================================
-- 30. Large message test
-- ===================================================================

testLargeMessage :: IO ()
testLargeMessage = do
  putStrLn "=== Large message test ==="

  -- Create a message with a 10KB Text field and a 100-element repeated Int32 list
  let bigText = T.replicate 10000 "a"  -- 10KB text
      nums = map fromIntegral [1..100 :: Int] :: [Int32]
      msg = WithRepeatedInt (Field bigText) (Field nums)
      bs = encode msg
  case decode @WithRepeatedInt bs of
    Right msg' -> do
      assert "large message roundtrip" (msg == msg')
      assert "large text preserved" (T.length (unField (riName msg')) == 10000)
      assert "repeated list length preserved" (length (unField (riNums msg')) == 100)
    Left err -> error $ "FAIL: large message roundtrip: " ++ show err


-- ===================================================================
-- 31. Wire compatibility with proto3 spec
-- ===================================================================

-- | Equivalent of:
-- message TestCompat {
--   string name = 1;
--   int32 id = 2;
--   bool active = 3;
-- }
data TestCompat = TestCompat
  { tcName   :: Field 1 Text
  , tcId     :: Field 2 Int32
  , tcActive :: Field 3 Bool
  } deriving (Show, Eq, Generic)

instance ProtoMessage TestCompat

testWireCompatibility :: IO ()
testWireCompatibility = do
  putStrLn "=== Wire compatibility with proto3 spec ==="

  let msg = TestCompat (Field "alice") (Field 42) (Field True)
      encoded = encode msg

  -- Expected wire bytes computed from proto3 spec:
  -- Field 1 (string): tag=0x0A (field 1, wire type 2), length=5, "alice"
  -- Field 2 (int32):  tag=0x10 (field 2, wire type 0), varint 42 = 0x2A
  -- Field 3 (bool):   tag=0x18 (field 3, wire type 0), varint 1 = 0x01
  let expected = BS.pack [0x0A, 0x05, 0x61, 0x6C, 0x69, 0x63, 0x65, 0x10, 0x2A, 0x18, 0x01]

  assert "wire compat: encode matches proto3 spec bytes" (encoded == expected)

  -- Reverse: decode known bytes and verify values
  case decode @TestCompat expected of
    Right msg' -> do
      assert "wire compat: decode name"   (unField (tcName msg') == "alice")
      assert "wire compat: decode id"     (unField (tcId msg') == 42)
      assert "wire compat: decode active" (unField (tcActive msg') == True)
    Left err -> error $ "FAIL: wire compat decode: " ++ show err


-- ===================================================================
-- 31b. Negative Int32 wire compatibility
-- ===================================================================

testNegativeInt32Wire :: IO ()
testNegativeInt32Wire = do
  putStrLn "=== Negative Int32 wire compatibility ==="

  -- Proto3 encodes negative int32 as 10-byte varint (sign-extended to 64 bits).
  -- Simple { sName = Field "test", sAge = Field (-1) }
  -- Field 1 (Text "test"): tag = 0x0A (field 1, wire type 2), length = 4, "test" = 74 65 73 74
  -- Field 2 (Int32 -1):    tag = 0x10 (field 2, wire type 0),
  --   value = varint(0xFFFFFFFFFFFFFFFF) = FF FF FF FF FF FF FF FF FF 01 (10 bytes)
  let msg = Simple (Field "test") (Field (-1))
      encoded = encode msg
      expected = BS.pack
        [ 0x0A, 0x04, 0x74, 0x65, 0x73, 0x74  -- field 1: "test"
        , 0x10                                  -- field 2: tag
        , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF          -- 10-byte varint for -1
        , 0xFF, 0xFF, 0xFF, 0xFF, 0x01          -- (sign-extended to 64 bits)
        ]
  assert "negative int32: encode matches proto3 spec" (encoded == expected)

  -- Reverse: decode known bytes
  case decode @Simple expected of
    Right msg' -> do
      assert "negative int32: decode name" (unField (sName msg') == "test")
      assert "negative int32: decode age" (unField (sAge msg') == -1)
    Left err -> error $ "FAIL: negative int32 decode: " ++ show err


-- ===================================================================
-- 31c. Bool field wire format
-- ===================================================================

testBoolWireFormat :: IO ()
testBoolWireFormat = do
  putStrLn "=== Bool field wire format ==="

  -- true encodes as varint 1; false is omitted (proto3 default)
  -- AllDefaults { dName = "", dAge = 0, dFlag = True }
  -- name="" -> skipped, age=0 -> skipped
  -- field 3 (Bool True): tag = 0x18 (field 3, wire type 0), value = varint 1 = 0x01
  let msgTrue = AllDefaults (Field "") (Field 0) (Field True)
      encodedTrue = encode msgTrue
      expectedTrue = BS.pack [0x18, 0x01]
  assert "bool true: encode matches proto3 spec" (encodedTrue == expectedTrue)

  case decode @AllDefaults expectedTrue of
    Right msg' -> do
      assert "bool true: decode flag" (unField (dFlag msg') == True)
      assert "bool true: decode name default" (unField (dName msg') == "")
      assert "bool true: decode age default" (unField (dAge msg') == 0)
    Left err -> error $ "FAIL: bool true decode: " ++ show err

  -- false is proto3 default -> all-default message encodes to empty
  let msgFalse = AllDefaults (Field "") (Field 0) (Field False)
      encodedFalse = encode msgFalse
  assert "bool false: omitted on wire (empty bytes)" (BS.null encodedFalse)


-- ===================================================================
-- 31d. Empty string field wire format
-- ===================================================================

testEmptyStringWire :: IO ()
testEmptyStringWire = do
  putStrLn "=== Empty string field wire format ==="

  -- Proto3: "" is default for string, so present-but-empty is omitted.
  -- Simple { sName = "", sAge = 42 }
  -- name="" -> skipped, age=42 -> tag=0x10, varint 42=0x2A
  let msg = Simple (Field "") (Field 42)
      encoded = encode msg
      expected = BS.pack [0x10, 0x2A]
  assert "empty string: omitted on wire" (encoded == expected)

  case decode @Simple expected of
    Right msg' -> do
      assert "empty string: decode name defaults to ''" (unField (sName msg') == "")
      assert "empty string: decode age" (unField (sAge msg') == 42)
    Left err -> error $ "FAIL: empty string decode: " ++ show err


-- ===================================================================
-- 31e. Varint edge cases (multi-byte boundaries)
-- ===================================================================

testVarintEdgeCases :: IO ()
testVarintEdgeCases = do
  putStrLn "=== Varint edge cases ==="

  -- Value 128: 2-byte varint 0x80 0x01
  -- Simple { sName = "", sAge = 128 }
  -- name="" -> skipped, age=128 -> tag=0x10, varint [0x80, 0x01]
  let msg128 = Simple (Field "") (Field 128)
      encoded128 = encode msg128
      expected128 = BS.pack [0x10, 0x80, 0x01]
  assert "varint 128: 2-byte encoding" (encoded128 == expected128)

  case decode @Simple expected128 of
    Right msg' -> assert "varint 128: roundtrip" (unField (sAge msg') == 128)
    Left err -> error $ "FAIL: varint 128 decode: " ++ show err

  -- Value 16384: 3-byte varint 0x80 0x80 0x01
  -- Simple { sName = "", sAge = 16384 }
  let msg16384 = Simple (Field "") (Field 16384)
      encoded16384 = encode msg16384
      expected16384 = BS.pack [0x10, 0x80, 0x80, 0x01]
  assert "varint 16384: 3-byte encoding" (encoded16384 == expected16384)

  case decode @Simple expected16384 of
    Right msg' -> assert "varint 16384: roundtrip" (unField (sAge msg') == 16384)
    Left err -> error $ "FAIL: varint 16384 decode: " ++ show err


-- ===================================================================
-- 31f. Nested message wire bytes
-- ===================================================================

testNestedMessageWire :: IO ()
testNestedMessageWire = do
  putStrLn "=== Nested message wire bytes ==="

  -- Person { pName = "Homer", pAddr = Just (Address { city = "NYC", addrZip = 100 }) }
  --
  -- Address submessage:
  --   field 1 (Text "NYC"): tag=0x0A, len=3, 4E 59 43
  --   field 2 (Int32 100):  tag=0x10, varint 100=0x64
  --   submessage bytes: 0A 03 4E 59 43 10 64 (7 bytes)
  --
  -- Person:
  --   field 1 (Text "Homer"): tag=0x0A, len=5, 48 6F 6D 65 72
  --   field 2 (submessage):   tag=0x12 (field 2, wire type 2), len=7, <submessage>
  let addr = Address (Field "NYC") (Field 100)
      person = Person (Field "Homer") (Field (Just addr))
      encoded = encode person
      expected = BS.pack
        [ 0x0A, 0x05, 0x48, 0x6F, 0x6D, 0x65, 0x72  -- field 1: "Homer"
        , 0x12, 0x07                                   -- field 2: tag + length 7
        , 0x0A, 0x03, 0x4E, 0x59, 0x43                -- submessage field 1: "NYC"
        , 0x10, 0x64                                   -- submessage field 2: 100
        ]
  assert "nested message: encode matches hand-computed bytes" (encoded == expected)

  case decode @Person expected of
    Right person' -> do
      assert "nested message: decode name" (unField (pName person') == "Homer")
      case unField (pAddr person') of
        Just addr' -> do
          assert "nested message: decode city" (unField (city addr') == "NYC")
          assert "nested message: decode zip" (unField (addrZip addr') == 100)
        Nothing -> error "FAIL: nested message decode: addr is Nothing"
    Left err -> error $ "FAIL: nested message wire decode: " ++ show err


-- ===================================================================
-- 31g. Repeated field (unpacked) wire bytes
-- ===================================================================

testRepeatedFieldWire :: IO ()
testRepeatedFieldWire = do
  putStrLn "=== Repeated field (unpacked) wire bytes ==="

  -- WithList { wTags = ["ab", "cd"] }
  -- Each string gets its own tag (field 1, wire type 2 = 0x0A):
  --   "ab": tag=0x0A, len=2, 61 62
  --   "cd": tag=0x0A, len=2, 63 64
  let msg = WithList (Field ["ab", "cd"])
      encoded = encode msg
      expected = BS.pack
        [ 0x0A, 0x02, 0x61, 0x62  -- "ab"
        , 0x0A, 0x02, 0x63, 0x64  -- "cd"
        ]
  assert "repeated unpacked: encode matches hand-computed bytes" (encoded == expected)

  case decode @WithList expected of
    Right msg' -> assert "repeated unpacked: decode roundtrip"
                         (unField (wTags msg') == ["ab", "cd"])
    Left err -> error $ "FAIL: repeated unpacked decode: " ++ show err

  -- Verify that multiple entries for same field are correctly merged on decode
  -- Manually construct: "ab" entry, then "cd" entry, then "ef" entry
  let extraEntry = BS.pack [0x0A, 0x02, 0x65, 0x66]  -- "ef"
      extended = expected <> extraEntry
  case decode @WithList extended of
    Right msg' -> assert "repeated unpacked: merge 3 entries"
                         (unField (wTags msg') == ["ab", "cd", "ef"])
    Left err -> error $ "FAIL: repeated unpacked merge: " ++ show err


-- ===================================================================
-- 32. Packed repeated field decoding
-- ===================================================================

data Packed = Packed
  { pScores :: Field 1 [Int32]
  } deriving (Show, Eq, Generic)

instance ProtoMessage Packed

testPackedRepeatedRoundtrip :: IO ()
testPackedRepeatedRoundtrip = do
  putStrLn "=== Packed repeated field decoding ==="

  -- Encode using packed format (single length-delimited blob)
  let scores = [10, 20, 30, 40, 50] :: [Int32]
      packedBytes = runProtoBuilder (encodePackedField (Field scores :: Field 1 [Int32]))
  -- Decode using Generics decoder (which now handles packed)
  case decode @Packed packedBytes of
    Right msg -> assert "packed repeated roundtrip" (unField (pScores msg) == scores)
    Left err -> error $ "FAIL: packed repeated roundtrip: " ++ show err

  -- Encode using unpacked format (each element individually tagged)
  let unpackedBytes = runProtoBuilder (encodeRepeatedField (Field scores :: Field 1 [Int32]))
  case decode @Packed unpackedBytes of
    Right msg -> assert "unpacked repeated roundtrip" (unField (pScores msg) == scores)
    Left err -> error $ "FAIL: unpacked repeated roundtrip: " ++ show err

  -- Empty packed field
  let emptyPacked = runProtoBuilder (encodePackedField (Field [] :: Field 1 [Int32]))
  case decode @Packed emptyPacked of
    Right msg -> assert "empty packed roundtrip" (unField (pScores msg) == [])
    Left err -> error $ "FAIL: empty packed roundtrip: " ++ show err


-- ===================================================================
-- 33. Mixed packed/unpacked entries
-- ===================================================================

testMixedPackedUnpacked :: IO ()
testMixedPackedUnpacked = do
  putStrLn "=== Mixed packed/unpacked entries ==="

  -- Manually construct wire bytes with a packed blob AND individual entries
  -- for the same field number 1.
  -- First: unpacked entry for value 1
  let unpacked1 = runProtoBuilder (encodeRepeatedField (Field [1 :: Int32] :: Field 1 [Int32]))
  -- Second: packed blob for values [2, 3, 4]
  let packed234 = runProtoBuilder (encodePackedField (Field [2, 3, 4 :: Int32] :: Field 1 [Int32]))
  -- Third: unpacked entry for value 5
  let unpacked5 = runProtoBuilder (encodeRepeatedField (Field [5 :: Int32] :: Field 1 [Int32]))
  -- Concatenate them (this is valid proto3: decoder must merge all entries)
  let mixed = unpacked1 <> packed234 <> unpacked5
  case decode @Packed mixed of
    Right msg -> assert "mixed packed/unpacked decodes all values"
                        (unField (pScores msg) == [1, 2, 3, 4, 5])
    Left err -> error $ "FAIL: mixed packed/unpacked: " ++ show err


-- ===================================================================
-- 34. ProtoMap field roundtrip
-- ===================================================================

data WithMap = WithMap
  { wmLabels :: Field 1 (ProtoMap Text Int32)
  } deriving (Show, Eq, Generic)

instance ProtoMessage WithMap

testMapRoundtrip :: IO ()
testMapRoundtrip = do
  putStrLn "=== ProtoMap field roundtrip ==="

  -- Non-empty map
  let m = Map.fromList [("alpha", 1), ("beta", 2), ("gamma", 3)]
      msg = WithMap (Field (ProtoMap m))
  case decode (encode msg) of
    Right msg' -> assert "map roundtrip" (msg == msg')
    Left err -> error $ "FAIL: map roundtrip: " ++ show err


-- ===================================================================
-- 35. Empty map
-- ===================================================================

testEmptyMap :: IO ()
testEmptyMap = do
  putStrLn "=== Empty map ==="

  let msg = WithMap (Field (ProtoMap Map.empty))
      bs = encode msg
  -- Empty map should encode to empty bytes (no entries)
  assert "empty map encodes to empty bytes" (BS.null bs)

  case decode @WithMap bs of
    Right msg' -> assert "empty map roundtrip" (msg == msg')
    Left err -> error $ "FAIL: empty map roundtrip: " ++ show err


-- ===================================================================
-- 36. Map with multiple entry types
-- ===================================================================

data WithMaps = WithMaps
  { wmStrStr :: Field 1 (ProtoMap Text Text)
  , wmStrInt :: Field 2 (ProtoMap Text Int32)
  } deriving (Show, Eq, Generic)

instance ProtoMessage WithMaps

testMultipleMaps :: IO ()
testMultipleMaps = do
  putStrLn "=== Multiple map fields ==="

  let msg = WithMaps
              (Field (ProtoMap (Map.fromList [("key1", "val1"), ("key2", "val2")])))
              (Field (ProtoMap (Map.fromList [("count", 42)])))
  case decode (encode msg) of
    Right msg' -> assert "multiple maps roundtrip" (msg == msg')
    Left err -> error $ "FAIL: multiple maps roundtrip: " ++ show err


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
  testNestedMessageRoundtrip
  testOutOfOrderDecoding
  testDuplicateFieldLastWins
  testMalformedInput
  testOverlongVarintRejection
  testZigzagNegativeEfficiency
  testLargeMessage
  testWireCompatibility
  testNegativeInt32Wire
  testBoolWireFormat
  testEmptyStringWire
  testVarintEdgeCases
  testNestedMessageWire
  testRepeatedFieldWire
  testPackedRepeatedRoundtrip
  testMixedPackedUnpacked
  testMapRoundtrip
  testEmptyMap
  testMultipleMaps

  putStrLn (replicate 40 '-')
  putStrLn "All tests passed!"
