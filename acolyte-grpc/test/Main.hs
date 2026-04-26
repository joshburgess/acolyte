{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Acolyte.Core
import Acolyte.Server (Json)
import Acolyte.Grpc


-- ===================================================================
-- Test types
-- ===================================================================

data HealthStatus = HealthStatus !Text
data CreateUserReq = CreateUserReq !Text
  deriving (Eq, Show)
data User = User !Text
  deriving (Eq, Show)
data Article = Article !Text

instance GrpcCodec HealthStatus where
  grpcEncode (HealthStatus t) = encodeUtf8 t
  grpcDecode bs = Just (HealthStatus (decodeUtf8 bs))

instance GrpcCodec CreateUserReq where
  grpcEncode (CreateUserReq t) = encodeUtf8 t
  grpcDecode bs = Just (CreateUserReq (decodeUtf8 bs))

instance GrpcCodec User where
  grpcEncode (User t) = encodeUtf8 t
  grpcDecode bs = Just (User (decodeUtf8 bs))

instance GrpcCodec Article where
  grpcEncode (Article t) = encodeUtf8 t
  grpcDecode bs = Just (Article (decodeUtf8 bs))

instance GrpcCodec Text where
  grpcEncode = encodeUtf8
  grpcDecode = Just . decodeUtf8

instance ProtoMessageName HealthStatus where
  protoMessageName = "HealthStatus"

instance ProtoMessageName CreateUserReq where
  protoMessageName = "CreateUserReq"

instance ProtoMessageName User where
  protoMessageName = "User"

instance ProtoMessageName Article where
  protoMessageName = "Article"

instance ProtoMessageName Text where
  protoMessageName = "StringValue"

-- HasProtoFields instances for full message generation
instance HasProtoFields User where
  protoFields =
    [ ProtoField "name" 1 ProtoString
    , ProtoField "id"   2 ProtoInt64
    ]

instance HasProtoFields CreateUserReq where
  protoFields =
    [ ProtoField "name" 1 ProtoString
    ]

instance HasProtoFields Article where
  protoFields =
    [ ProtoField "title"  1 ProtoString
    , ProtoField "body"   2 ProtoString
    , ProtoField "author" 3 (ProtoMessage "User")
    ]

-- CollectTypeDef instances to enable automatic collection
instance {-# OVERLAPPING #-} CollectTypeDef User where
  collectTypeDef = [MessageDef (protoMessageName @User) (protoFields @User)]

instance {-# OVERLAPPING #-} CollectTypeDef CreateUserReq where
  collectTypeDef = [MessageDef (protoMessageName @CreateUserReq) (protoFields @CreateUserReq)]

instance {-# OVERLAPPING #-} CollectTypeDef Article where
  collectTypeDef = [MessageDef (protoMessageName @Article) (protoFields @Article)]

-- helpers
encodeUtf8 :: Text -> ByteString
encodeUtf8 = BS.pack . map (fromIntegral . fromEnum) . T.unpack

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.pack . map (toEnum . fromIntegral) . BS.unpack


-- ===================================================================
-- Test API
-- ===================================================================

type HealthPath    = '[ 'Lit "health" ]
type UsersPath     = '[ 'Lit "users" ]
type ArticlesPath  = '[ 'Lit "articles" ]

type TestAPI =
  '[ Get  HealthPath Text
   , Post UsersPath  (Json CreateUserReq) (Json User)
   ]


-- ===================================================================
-- Three-endpoint API
-- ===================================================================

type ThreeAPI =
  '[ Get  HealthPath   Text
   , Post UsersPath    (Json CreateUserReq) (Json User)
   , Get  ArticlesPath (Json Article)
   ]


-- ===================================================================
-- Capture-containing path API
-- ===================================================================

type UserByIdPath = '[ 'Lit "users", 'Capture Int ]

type CaptureAPI =
  '[ Get UserByIdPath (Json User)
   ]


-- ===================================================================
-- Helpers
-- ===================================================================

assert :: String -> Bool -> IO ()
assert msg True  = putStrLn $ "  OK: " ++ msg
assert msg False = error   $ "FAIL: " ++ msg


-- ===================================================================
-- Tests
-- ===================================================================

testBuildGrpcHandlers :: IO ()
testBuildGrpcHandlers = do
  putStrLn "BuildGrpcHandlers:"

  let svcMap = mkGrpcServiceMap @TestAPI "test" "TestSvc"
        ( healthHandler
        , createUserHandler
        )
  -- Check the service map has 2 entries
  assert "2 methods" (Map.size svcMap == 2)
  assert "has Health" (Map.member ("test.TestSvc", "Health") svcMap)
  assert "has Users" (Map.member ("test.TestSvc", "Users") svcMap)

  where
    healthHandler :: GrpcHandlerFn
    healthHandler _req = pure $ Right "ok"

    createUserHandler :: GrpcHandlerFn
    createUserHandler req = pure $ Right ("created:" <> req)


testProtoGeneration :: IO ()
testProtoGeneration = do
  putStrLn "\nProto generation:"

  let proto = generateProto @TestAPI "test" "TestSvc"
  assert "has syntax" (T.isInfixOf "syntax = \"proto3\"" proto)
  assert "has package" (T.isInfixOf "package test;" proto)
  assert "has service" (T.isInfixOf "service TestSvc {" proto)
  assert "has Health rpc" (T.isInfixOf "rpc Health" proto)
  assert "has Users rpc" (T.isInfixOf "rpc Users" proto)
  assert "has Empty input" (T.isInfixOf "(Empty)" proto)
  assert "has User output" (T.isInfixOf "User)" proto)


type EffectAPI =
  '[ Requires Auth (Get HealthPath Text)
   , Post UsersPath (Json CreateUserReq) (Json User)
   ]

testEffectDelegation :: IO ()
testEffectDelegation = do
  putStrLn "\nEffect delegation:"

  let svcMap = mkGrpcServiceMap @EffectAPI "test" "EffSvc"
        ( healthH, createH )
  assert "effect: 2 methods" (Map.size svcMap == 2)
  assert "effect: has Health" (Map.member ("test.EffSvc", "Health") svcMap)

  where
    healthH :: GrpcHandlerFn
    healthH _ = pure $ Right "ok"
    createH :: GrpcHandlerFn
    createH _ = pure $ Right "created"


-- ===================================================================
-- 1. Multi-endpoint API (3 endpoints)
-- ===================================================================

testThreeEndpointAPI :: IO ()
testThreeEndpointAPI = do
  putStrLn "\nThree-endpoint API:"

  let svcMap = mkGrpcServiceMap @ThreeAPI "test" "ThreeSvc"
        ( healthH, createH, articlesH )
  assert "3 methods" (Map.size svcMap == 3)
  assert "has Health"   (Map.member ("test.ThreeSvc", "Health") svcMap)
  assert "has Users"    (Map.member ("test.ThreeSvc", "Users") svcMap)
  assert "has Articles" (Map.member ("test.ThreeSvc", "Articles") svcMap)

  where
    healthH :: GrpcHandlerFn
    healthH _ = pure $ Right "ok"
    createH :: GrpcHandlerFn
    createH _ = pure $ Right "created"
    articlesH :: GrpcHandlerFn
    articlesH _ = pure $ Right "article"


-- ===================================================================
-- 2. GrpcCodec roundtrip
-- ===================================================================

testGrpcCodecRoundtrip :: IO ()
testGrpcCodecRoundtrip = do
  putStrLn "\nGrpcCodec roundtrip:"

  let user = User "alice"
      encoded = grpcEncode user
      decoded = grpcDecode encoded :: Maybe User
  assert "User roundtrip" (decoded == Just user)

  let req = CreateUserReq "bob"
      encodedReq = grpcEncode req
      decodedReq = grpcDecode encodedReq :: Maybe CreateUserReq
  assert "CreateUserReq roundtrip" (decodedReq == Just req)

  let txt = "hello world" :: Text
      encodedTxt = grpcEncode txt
      decodedTxt = grpcDecode encodedTxt :: Maybe Text
  assert "Text roundtrip" (decodedTxt == Just txt)

  -- Empty bytestring roundtrip
  let empty = "" :: Text
      encodedEmpty = grpcEncode empty
      decodedEmpty = grpcDecode encodedEmpty :: Maybe Text
  assert "empty Text roundtrip" (decodedEmpty == Just empty)


-- ===================================================================
-- 3. Capture-containing paths
-- ===================================================================

testCapturePaths :: IO ()
testCapturePaths = do
  putStrLn "\nCapture-containing paths:"

  -- Get '[ 'Lit "users", 'Capture Int ] (Json User)
  -- Should produce method name "Users" (capture is skipped)
  let svcMap = mkGrpcServiceMap @CaptureAPI "test" "CapSvc"
        captureH
  assert "capture: 1 method"  (Map.size svcMap == 1)
  assert "capture: has Users" (Map.member ("test.CapSvc", "Users") svcMap)

  where
    captureH :: GrpcHandlerFn
    captureH _ = pure $ Right "user-by-id"


-- ===================================================================
-- 4. Proto generation completeness (3-endpoint API)
-- ===================================================================

testProtoCompleteness :: IO ()
testProtoCompleteness = do
  putStrLn "\nProto generation completeness:"

  let proto = generateProto @ThreeAPI "test" "ThreeSvc"
      protoLines = T.lines proto
      rpcLines = filter (T.isInfixOf "rpc ") protoLines

  assert "exactly 3 rpc lines" (length rpcLines == 3)

  -- Verify each rpc line has the expected input/output types
  assert "Health: (Empty) returns (StringValue)"
    (any (\l -> T.isInfixOf "rpc Health" l
             && T.isInfixOf "(Empty)" l
             && T.isInfixOf "(StringValue)" l) rpcLines)

  assert "Users: (CreateUserReq) returns (User)"
    (any (\l -> T.isInfixOf "rpc Users" l
             && T.isInfixOf "(CreateUserReq)" l
             && T.isInfixOf "(User)" l) rpcLines)

  assert "Articles: (Empty) returns (Article)"
    (any (\l -> T.isInfixOf "rpc Articles" l
             && T.isInfixOf "(Empty)" l
             && T.isInfixOf "(Article)" l) rpcLines)


-- ===================================================================
-- 5. GrpcReady negative test (commented out)
-- ===================================================================

-- The following block demonstrates that an API containing a type
-- without a GrpcCodec instance will fail at compile time.
--
-- data NoCodecType = NoCodecType
--
-- instance ProtoMessageName NoCodecType where
--   protoMessageName = "NoCodecType"
--
-- type BadAPI = '[ Get HealthPath NoCodecType ]
--
-- testBadAPI :: GrpcReady BadAPI => IO ()
-- testBadAPI = pure ()
--
-- Expected compile error:
--   No instance for 'GrpcCodec NoCodecType'
--     arising from a use of 'GrpcReady'
--
-- This confirms that GrpcReady catches missing GrpcCodec instances
-- at compile time, preventing runtime serialization failures.


-- ===================================================================
-- 6. Proto generation for endpoint with request body
-- ===================================================================

type PostUserAPI = '[ Post UsersPath (Json CreateUserReq) (Json User) ]

testProtoWithRequestBody :: IO ()
testProtoWithRequestBody = do
  putStrLn "\nProto generation for endpoint with request body:"

  -- Post UsersPath (Json CreateUserReq) (Json User)
  -- Should generate: rpc Users (CreateUserReq) returns (User);
  let proto = generateProto @PostUserAPI "test" "PostSvc"
      protoLines = T.lines proto
      rpcLines = filter (T.isInfixOf "rpc ") protoLines

  assert "exactly 1 rpc line" (length rpcLines == 1)
  assert "rpc Users (CreateUserReq) returns (User);"
    (any (T.isInfixOf "rpc Users (CreateUserReq) returns (User);") rpcLines)


-- ===================================================================
-- 7. Proto generation for bodyless endpoint
-- ===================================================================

type GetHealthAPI = '[ Get HealthPath Text ]

testProtoBodyless :: IO ()
testProtoBodyless = do
  putStrLn "\nProto generation for bodyless endpoint:"

  -- Get HealthPath Text
  -- Should generate: rpc Health (Empty) returns (StringValue);
  let proto = generateProto @GetHealthAPI "test" "HealthSvc"
      protoLines = T.lines proto
      rpcLines = filter (T.isInfixOf "rpc ") protoLines

  assert "exactly 1 rpc line" (length rpcLines == 1)
  assert "rpc Health (Empty) returns (StringValue);"
    (any (T.isInfixOf "rpc Health (Empty) returns (StringValue);") rpcLines)


-- ===================================================================
-- 8. renderFieldType
-- ===================================================================

testRenderFieldType :: IO ()
testRenderFieldType = do
  putStrLn "\nrenderFieldType:"

  assert "int32"    (renderFieldType ProtoInt32    == "int32")
  assert "int64"    (renderFieldType ProtoInt64    == "int64")
  assert "uint32"   (renderFieldType ProtoUInt32   == "uint32")
  assert "uint64"   (renderFieldType ProtoUInt64   == "uint64")
  assert "double"   (renderFieldType ProtoDouble   == "double")
  assert "float"    (renderFieldType ProtoFloat    == "float")
  assert "bool"     (renderFieldType ProtoBool     == "bool")
  assert "string"   (renderFieldType ProtoString   == "string")
  assert "bytes"    (renderFieldType ProtoBytes    == "bytes")
  assert "message"  (renderFieldType (ProtoMessage "Foo") == "Foo")
  assert "repeated" (renderFieldType (ProtoRepeated ProtoString) == "repeated string")
  assert "optional" (renderFieldType (ProtoOptional ProtoInt32)  == "optional int32")
  assert "repeated message" (renderFieldType (ProtoRepeated (ProtoMessage "Bar")) == "repeated Bar")


-- ===================================================================
-- 9. renderMessage
-- ===================================================================

testRenderMessage :: IO ()
testRenderMessage = do
  putStrLn "\nrenderMessage:"

  let msg = MessageDef "User"
        [ ProtoField "name"  1 ProtoString
        , ProtoField "email" 2 ProtoString
        , ProtoField "id"    3 ProtoInt64
        ]
      rendered = renderMessage msg

  assert "message User {"     (T.isInfixOf "message User {"     rendered)
  assert "string name = 1;"   (T.isInfixOf "string name = 1;"   rendered)
  assert "string email = 2;"  (T.isInfixOf "string email = 2;"  rendered)
  assert "int64 id = 3;"      (T.isInfixOf "int64 id = 3;"      rendered)
  assert "closing brace"      (T.isInfixOf "}"                   rendered)

  -- Nested message field
  let msg2 = MessageDef "Article"
        [ ProtoField "title"  1 ProtoString
        , ProtoField "author" 2 (ProtoMessage "User")
        ]
      rendered2 = renderMessage msg2

  assert "User author = 2;" (T.isInfixOf "User author = 2;" rendered2)


-- ===================================================================
-- 10. generateProtoFull
-- ===================================================================

testGenerateProtoFull :: IO ()
testGenerateProtoFull = do
  putStrLn "\ngenerateProtoFull:"

  let msgs =
        [ MessageDef "CreateUserReq" [ ProtoField "name" 1 ProtoString ]
        , MessageDef "User"
            [ ProtoField "name" 1 ProtoString
            , ProtoField "id"   2 ProtoInt64
            ]
        ]
      proto = generateProtoFull @TestAPI "test" "TestSvc" msgs

  assert "has syntax"    (T.isInfixOf "syntax = \"proto3\";" proto)
  assert "has package"   (T.isInfixOf "package test;" proto)
  assert "has service"   (T.isInfixOf "service TestSvc {" proto)
  assert "has Health rpc" (T.isInfixOf "rpc Health" proto)
  assert "has Users rpc" (T.isInfixOf "rpc Users" proto)
  assert "has Empty import" (T.isInfixOf "import \"google/protobuf/empty.proto\";" proto)

  -- Full message definitions should be present
  assert "message CreateUserReq" (T.isInfixOf "message CreateUserReq {" proto)
  assert "string name = 1;"      (T.isInfixOf "string name = 1;" proto)
  assert "message User"          (T.isInfixOf "message User {" proto)
  assert "int64 id = 2;"         (T.isInfixOf "int64 id = 2;" proto)

  -- No commented stubs
  assert "no stubs" (not (T.isInfixOf "// message" proto))


-- ===================================================================
-- 11. generateProto with CollectMessageDefs (auto-collected)
-- ===================================================================

testGenerateProtoAutoCollect :: IO ()
testGenerateProtoAutoCollect = do
  putStrLn "\ngenerateProto with auto-collected message defs:"

  -- TestAPI has User and CreateUserReq which have CollectTypeDef instances
  let proto = generateProto @TestAPI "test" "TestSvc"

  assert "has syntax"    (T.isInfixOf "syntax = \"proto3\";" proto)
  assert "has package"   (T.isInfixOf "package test;" proto)
  assert "has service"   (T.isInfixOf "service TestSvc {" proto)
  assert "has Empty import" (T.isInfixOf "import \"google/protobuf/empty.proto\";" proto)

  -- Auto-collected message defs
  assert "message CreateUserReq" (T.isInfixOf "message CreateUserReq {" proto)
  assert "message User"          (T.isInfixOf "message User {" proto)
  assert "string name = 1;"      (T.isInfixOf "string name = 1;" proto)


-- ===================================================================
-- 12. generateProtoFull without Empty import
-- ===================================================================

testGenerateProtoFullNoEmpty :: IO ()
testGenerateProtoFullNoEmpty = do
  putStrLn "\ngenerateProtoFull without Empty import:"

  let msgs = [ MessageDef "CreateUserReq" [ ProtoField "name" 1 ProtoString ]
             , MessageDef "User" [ ProtoField "name" 1 ProtoString ]
             ]
      proto = generateProtoFull @PostUserAPI "test" "PostSvc" msgs

  assert "no Empty import" (not (T.isInfixOf "google/protobuf/empty.proto" proto))
  assert "has message defs" (T.isInfixOf "message CreateUserReq {" proto)


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "acolyte-grpc tests:\n"
  testBuildGrpcHandlers
  testProtoGeneration
  testEffectDelegation
  testThreeEndpointAPI
  testGrpcCodecRoundtrip
  testCapturePaths
  testProtoCompleteness
  testProtoWithRequestBody
  testProtoBodyless
  testRenderFieldType
  testRenderMessage
  testGenerateProtoFull
  testGenerateProtoAutoCollect
  testGenerateProtoFullNoEmpty
  putStrLn "\nAll acolyte-grpc tests passed."
