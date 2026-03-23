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
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

import Tower.Protobuf
import Tower.Protobuf.Wire
import Tower.Protobuf.Encode
import Tower.Protobuf.Decode (RawField(..), parseFields, DecodeError(..))
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

  putStrLn (replicate 40 '-')
  putStrLn "All tests passed!"
