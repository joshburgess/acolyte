-- | Generate @.proto@ files from acolyte API types.
--
-- @
-- import Acolyte.Grpc.Generate
--
-- protoFile :: Text
-- protoFile = generateProto @MyAPI "mypackage" "MyService"
-- @
module Acolyte.Grpc.Generate
  ( -- * Proto generation
    generateProto
  , generateProtoFull
    -- * Message definitions
  , MessageDef (..)
  , renderMessage
  , renderFieldType
    -- * Collecting message defs from API types
  , CollectMessageDefs (..)
  , CollectTypeDef (..)
    -- * Type class
  , ApiToProtoMethods (..)
  , ProtoMethod (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy (..))

import Acolyte.Core.Endpoint (Endpoint, NoBody)
import Acolyte.Core.Path (PathSegment(..))
import Acolyte.Core.Effect (Requires)
import Acolyte.Core.Wrapper
  ( Describe, WithParams, WithHeaders
  , ServerStream, ClientStream, BidiStream, RespondsWith
  )
import Acolyte.Grpc.Proto (ProtoMessageName(..), ProtoField(..), ProtoFieldType(..))
import Acolyte.Server.Response (Json)


-- | A proto service method descriptor.
data ProtoMethod = ProtoMethod
  { pmName       :: !Text  -- ^ PascalCase method name
  , pmInputType  :: !Text  -- ^ input message type name
  , pmOutputType :: !Text  -- ^ output message type name
  } deriving (Show, Eq)


-- | A complete proto message definition (name + fields).
data MessageDef = MessageDef
  { mdName   :: !Text
  , mdFields :: ![ProtoField]
  } deriving (Show, Eq)


-- | Render a 'ProtoFieldType' to its proto3 text representation.
renderFieldType :: ProtoFieldType -> Text
renderFieldType ProtoInt32          = "int32"
renderFieldType ProtoInt64          = "int64"
renderFieldType ProtoUInt32         = "uint32"
renderFieldType ProtoUInt64         = "uint64"
renderFieldType ProtoDouble         = "double"
renderFieldType ProtoFloat          = "float"
renderFieldType ProtoBool           = "bool"
renderFieldType ProtoString         = "string"
renderFieldType ProtoBytes          = "bytes"
renderFieldType (ProtoMessage name) = name
renderFieldType (ProtoRepeated inner) = "repeated " <> renderFieldType inner
renderFieldType (ProtoOptional inner) = "optional " <> renderFieldType inner


-- | Render a 'MessageDef' to proto3 text.
--
-- @
-- message User {
--   string name = 1;
--   string email = 2;
--   int64 id = 3;
-- }
-- @
renderMessage :: MessageDef -> Text
renderMessage (MessageDef name fields) =
  T.unlines $
    [ "message " <> name <> " {" ] ++
    [ "  " <> renderFieldType (pfType f) <> " " <> pfName f <> " = " <> T.pack (show (pfNumber f)) <> ";"
    | f <- fields
    ] ++
    [ "}" ]


-- | Generate a complete @.proto@ file with explicit message definitions.
generateProtoFull
  :: forall api
   . ApiToProtoMethods api
  => Text           -- ^ package name
  -> Text           -- ^ service name
  -> [MessageDef]   -- ^ message definitions
  -> Text           -- ^ .proto file content
generateProtoFull pkg svc msgs =
  let methods = apiToProtoMethods @api
      needsEmpty = any (\m -> pmInputType m == "Empty") methods
      emptyImport = if needsEmpty
        then [ "import \"google/protobuf/empty.proto\";", "" ]
        else []
  in T.unlines $
    [ "syntax = \"proto3\";"
    , ""
    ] ++
    emptyImport ++
    [ "package " <> pkg <> ";"
    , ""
    , "service " <> svc <> " {"
    ] ++
    [ "  rpc " <> pmName m <> " (" <> pmInputType m <> ") returns (" <> pmOutputType m <> ");"
    | m <- methods
    ] ++
    [ "}"
    ] ++
    (if null msgs then []
     else "" : concatMap (\msg -> [renderMessage msg]) msgs)


-- | Generate a @.proto@ file from an API type.
--
-- @
-- generateProto @MyAPI "mypackage" "UserService"
-- @
--
-- When the API's request\/response types have 'HasProtoFields' and
-- 'ProtoMessageName' instances, full message definitions are emitted.
-- Otherwise, commented stubs are produced.
generateProto
  :: forall api
   . (ApiToProtoMethods api, CollectMessageDefs api)
  => Text     -- ^ package name
  -> Text     -- ^ service name
  -> Text     -- ^ .proto file content
generateProto pkg svc =
  let methods = apiToProtoMethods @api
      collected = collectMessageDefs @api
      needsEmpty = any (\m -> pmInputType m == "Empty") methods
      emptyImport = if needsEmpty
        then [ "import \"google/protobuf/empty.proto\";", "" ]
        else []
  in T.unlines $
    [ "syntax = \"proto3\";"
    , ""
    ] ++
    emptyImport ++
    [ "package " <> pkg <> ";"
    , ""
    , "service " <> svc <> " {"
    ] ++
    [ "  rpc " <> pmName m <> " (" <> pmInputType m <> ") returns (" <> pmOutputType m <> ");"
    | m <- methods
    ] ++
    [ "}" ] ++
    (if null collected
      then
        [ ""
        , "// Message definitions (generate from HasProtoFields or write manually):"
        ] ++
        [ "// message " <> t <> " { ... }"
        | t <- uniqueTypes methods
        ]
      else "" : concatMap (\msg -> [renderMessage msg]) (dedupMsgs collected))


-- | Deduplicate message definitions by name.
dedupMsgs :: [MessageDef] -> [MessageDef]
dedupMsgs = go []
  where
    go _    []     = []
    go seen (m:ms)
      | mdName m `elem` seen = go seen ms
      | otherwise             = m : go (mdName m : seen) ms


-- | Collect unique type names from methods.
uniqueTypes :: [ProtoMethod] -> [Text]
uniqueTypes methods =
  let allTypes = concatMap (\m -> [pmInputType m, pmOutputType m]) methods
      noDups [] = []
      noDups (x:xs)
        | x `elem` xs = noDups xs
        | x == "Empty" = noDups xs
        | otherwise    = x : noDups xs
  in noDups allTypes


-- | Walk the API type and extract proto method descriptors.
class ApiToProtoMethods (api :: [Type]) where
  apiToProtoMethods :: [ProtoMethod]

instance ApiToProtoMethods '[] where
  apiToProtoMethods = []

instance (EndpointProtoMethod e, ApiToProtoMethods rest)
  => ApiToProtoMethods (e ': rest) where
  apiToProtoMethods = endpointProtoMethod @e : apiToProtoMethods @rest


-- | Extract a proto method from a single endpoint type.
class EndpointProtoMethod (e :: Type) where
  endpointProtoMethod :: ProtoMethod

instance EndpointProtoMethod inner => EndpointProtoMethod (Requires eff inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance EndpointProtoMethod inner => EndpointProtoMethod (Describe desc inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance EndpointProtoMethod inner => EndpointProtoMethod (WithParams ps inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance EndpointProtoMethod inner => EndpointProtoMethod (WithHeaders hs inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance EndpointProtoMethod inner => EndpointProtoMethod (ServerStream inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance EndpointProtoMethod inner => EndpointProtoMethod (ClientStream inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance EndpointProtoMethod inner => EndpointProtoMethod (BidiStream inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance EndpointProtoMethod inner => EndpointProtoMethod (RespondsWith s inner) where
  endpointProtoMethod = endpointProtoMethod @inner

instance {-# OVERLAPPING #-} (PathToMethodName path, ResponseTypeName resp)
  => EndpointProtoMethod (Endpoint m path NoBody resp) where
  endpointProtoMethod = ProtoMethod
    { pmName       = pathToMethodName @path
    , pmInputType  = "Empty"
    , pmOutputType = responseTypeName @resp
    }

instance (PathToMethodName path, RequestTypeName req, ResponseTypeName resp)
  => EndpointProtoMethod (Endpoint m path req resp) where
  endpointProtoMethod = ProtoMethod
    { pmName       = pathToMethodName @path
    , pmInputType  = requestTypeName @req
    , pmOutputType = responseTypeName @resp
    }


-- | Get the proto type name for a response type.
class ResponseTypeName (a :: Type) where
  responseTypeName :: Text

instance ProtoMessageName a => ResponseTypeName (Json a) where
  responseTypeName = protoMessageName @a

instance {-# OVERLAPPABLE #-} ProtoMessageName a => ResponseTypeName a where
  responseTypeName = protoMessageName @a


-- | Get the proto type name for a request type.
class RequestTypeName (a :: Type) where
  requestTypeName :: Text

instance ProtoMessageName a => RequestTypeName (Json a) where
  requestTypeName = protoMessageName @a

instance {-# OVERLAPPABLE #-} ProtoMessageName a => RequestTypeName a where
  requestTypeName = protoMessageName @a


-- | Convert a type-level path to a PascalCase gRPC method name.
class PathToMethodName (path :: [PathSegment]) where
  pathToMethodName :: Text

instance PathToMethodName '[] where
  pathToMethodName = "Unknown"

instance (KnownSymbol s, PathToMethodName rest)
  => PathToMethodName ('Lit s ': rest) where
  pathToMethodName = capitalize (T.pack (symbolVal (Proxy @s)))

instance PathToMethodName rest
  => PathToMethodName ('Capture t ': rest) where
  pathToMethodName = pathToMethodName @rest


capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise             = c


-- ===================================================================
-- CollectMessageDefs: walk API and gather MessageDefs
-- ===================================================================

-- | Walk an API type list and collect 'MessageDef' for every
-- request\/response type that has both 'ProtoMessageName' and
-- 'HasProtoFields' instances.
class CollectMessageDefs (api :: [Type]) where
  collectMessageDefs :: [MessageDef]

instance CollectMessageDefs '[] where
  collectMessageDefs = []

instance (CollectEndpointDefs e, CollectMessageDefs rest)
  => CollectMessageDefs (e ': rest) where
  collectMessageDefs = collectEndpointDefs @e ++ collectMessageDefs @rest


-- | Collect message defs from a single endpoint.
class CollectEndpointDefs (e :: Type) where
  collectEndpointDefs :: [MessageDef]

instance CollectEndpointDefs inner => CollectEndpointDefs (Requires eff inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance CollectEndpointDefs inner => CollectEndpointDefs (Describe desc inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance CollectEndpointDefs inner => CollectEndpointDefs (WithParams ps inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance CollectEndpointDefs inner => CollectEndpointDefs (WithHeaders hs inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance CollectEndpointDefs inner => CollectEndpointDefs (ServerStream inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance CollectEndpointDefs inner => CollectEndpointDefs (ClientStream inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance CollectEndpointDefs inner => CollectEndpointDefs (BidiStream inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance CollectEndpointDefs inner => CollectEndpointDefs (RespondsWith s inner) where
  collectEndpointDefs = collectEndpointDefs @inner

instance {-# OVERLAPPING #-} (CollectTypeDef resp)
  => CollectEndpointDefs (Endpoint m path NoBody resp) where
  collectEndpointDefs = collectTypeDef @resp

instance (CollectTypeDef req, CollectTypeDef resp)
  => CollectEndpointDefs (Endpoint m path req resp) where
  collectEndpointDefs = collectTypeDef @req ++ collectTypeDef @resp


-- | Collect a 'MessageDef' from a single type, if it has the
-- required instances. The base instance returns @[]@.
class CollectTypeDef (a :: Type) where
  collectTypeDef :: [MessageDef]

-- | Json wrapper: delegate to inner type.
instance CollectTypeDef inner => CollectTypeDef (Json inner) where
  collectTypeDef = collectTypeDef @inner

-- | Default: no message def available.
instance {-# OVERLAPPABLE #-} CollectTypeDef a where
  collectTypeDef = []

-- | Types with both 'ProtoMessageName' and 'HasProtoFields' produce
-- a 'MessageDef'. Users opt in by providing an overlapping instance.
--
-- To collect message defs for your type, add:
--
-- @
-- instance {-# OVERLAPPING #-} CollectTypeDef MyType where
--   collectTypeDef = [MessageDef (protoMessageName @MyType) (protoFields @MyType)]
-- @
