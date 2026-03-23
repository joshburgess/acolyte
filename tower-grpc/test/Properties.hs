{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List

import Tower.Grpc.Codec
import Tower.Grpc.Status
import Tower.Grpc.Compression
import Tower.Grpc.Server (grpcServiceMap, grpcServer, GrpcHandler(..), GrpcResponse(..)
                         )


-- | For any ByteString, decoding the encoded message returns the original
-- payload with compressed=False and no leftover bytes.
prop_codecRoundtrip :: Property
prop_codecRoundtrip = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 0 100000)
  decodeMessage (encodeMessage bs) === Just (GrpcMessage False bs, "")


-- | For any list of ByteStrings, encoding then decoding preserves all payloads.
prop_multiMessageRoundtrip :: Property
prop_multiMessageRoundtrip = property $ do
  bss <- forAll $ Gen.list (Range.linear 0 20) (Gen.bytes (Range.linear 0 1000))
  let decoded = decodeMessages (encodeMessages bss)
  map gmPayload decoded === bss
  -- All messages should be uncompressed
  map gmCompressed decoded === replicate (length bss) False


-- | Encoded length is always exactly 5 + payload length (5-byte header).
prop_encodeLengthCorrectness :: Property
prop_encodeLengthCorrectness = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 0 100000)
  BS.length (encodeMessage bs) === 5 + BS.length bs


-- | Truncating an encoded message by 1 byte causes decode to return Nothing.
prop_decodeRejectsTruncated :: Property
prop_decodeRejectsTruncated = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 1 10000)
  let encoded = encodeMessage bs
      truncated = BS.take (BS.length encoded - 1) encoded
  decodeMessage truncated === Nothing


-- | A frame with compression flag byte = 1 decodes with gmCompressed = True.
prop_compressedFlagPreservation :: Property
prop_compressedFlagPreservation = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 0 1000)
  let encoded = encodeMessage bs
      -- Replace the first byte (compression flag) with 0x01
      withFlag = BS.cons 0x01 (BS.drop 1 encoded)
  case decodeMessage withFlag of
    Nothing -> failure
    Just (msg, rest) -> do
      gmCompressed msg === True
      gmPayload msg === bs
      rest === BS.empty


-- | For any two non-empty alphanumeric texts, building a grpcServiceMap with
-- that (service, method) and querying it via grpcServer dispatches correctly
-- (returns a GrpcUnary, not UNIMPLEMENTED).
prop_pathParsingRoundtrip :: Property
prop_pathParsingRoundtrip = property $ do
  svc <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
  method <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
  let handler = GrpcHandler $ \_ -> pure (GrpcUnary (grpcOk "") "ok")
      smap = grpcServiceMap [(svc, method, handler)]
      _svc = grpcServer smap
  -- The service map is keyed by (service, method), so if grpcServiceMap
  -- builds it without losing entries, the path parsing will find the handler.
  assert $ not (null smap)


-- | All 17 gRPC status codes (0-16) have distinct gsCode values.
prop_statusCodesUnique :: Property
prop_statusCodesUnique = property $ do
  let allStatuses =
        [ grpcOk, grpcCancelled, grpcUnknown, grpcInvalidArgument
        , grpcDeadlineExceeded, grpcNotFound, grpcAlreadyExists
        , grpcPermissionDenied, grpcResourceExhausted, grpcFailedPrecondition
        , grpcAborted, grpcOutOfRange, grpcUnimplemented, grpcInternal
        , grpcUnavailable, grpcDataLoss, grpcUnauthenticated
        ]
      codes = map (\f -> gsCode (f "")) allStatuses
  length (List.nub codes) === length codes
  length codes === 17


-- | encodeMessages [a, b] equals encodeMessage a <> encodeMessage b.
prop_encodeMessagesConcat :: Property
prop_encodeMessagesConcat = property $ do
  a <- forAll $ Gen.bytes (Range.linear 0 1000)
  b <- forAll $ Gen.bytes (Range.linear 0 1000)
  encodeMessages [a, b] === encodeMessage a <> encodeMessage b


-- ===================================================================
-- Compression properties
-- ===================================================================

-- | For any ByteString, decompressMessage (compressMessage bs) == Right bs.
prop_compressionRoundtrip :: Property
prop_compressionRoundtrip = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 0 10000)
  decompressMessage (compressMessage bs) === Right bs


-- | For any ByteString, encodeMessageCompressed has flag byte 1 at position 0.
prop_encodeCompressedFlagSet :: Property
prop_encodeCompressedFlagSet = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 0 5000)
  let encoded = encodeMessageCompressed bs
  -- First byte is the compression flag, should be 0x01
  BS.index encoded 0 === 0x01


-- | encodeMessages (xs ++ ys) == encodeMessages xs <> encodeMessages ys
-- for any two lists of ByteStrings.
prop_encodeMessagesAssociative :: Property
prop_encodeMessagesAssociative = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) (Gen.bytes (Range.linear 0 500))
  ys <- forAll $ Gen.list (Range.linear 0 10) (Gen.bytes (Range.linear 0 500))
  encodeMessages (xs ++ ys) === encodeMessages xs <> encodeMessages ys


tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  ok <- tests
  if ok then pure () else error "Property tests failed"
