-- | Parse proto3 service definitions into the shared API IR.
--
-- Supports enough proto3 syntax to extract services, RPCs, messages,
-- and enums. Does not follow imports or act as a full protobuf compiler.
module Acolyte.Codegen.Proto
  ( -- * Parsing
    parseProtoFile
  , parseProtoText
    -- * Types
  , ProtoFile (..)
  , ProtoService (..)
  , ProtoRpc (..)
  , ProtoMessage (..)
  , ProtoField (..)
  , ProtoEnum (..)
  , ProtoParseError (..)
    -- * Conversion to IR
  , protoToIR
  ) where

import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Acolyte.Codegen.IR


-- ===================================================================
-- Types
-- ===================================================================

-- | Errors that can occur when parsing a proto file.
data ProtoParseError
  = ProtoSyntaxError !Text
  | ProtoUnsupportedSyntax !Text
  deriving (Show)


-- | A parsed .proto file.
data ProtoFile = ProtoFile
  { protoSyntax   :: !Text        -- ^ "proto3"
  , protoPackage  :: !Text        -- ^ Package name
  , protoServices :: ![ProtoService]
  , protoMessages :: ![ProtoMessage]
  , protoEnums    :: ![ProtoEnum]
  , protoImports  :: ![Text]
  } deriving (Show)


-- | A service definition.
data ProtoService = ProtoService
  { psName :: !Text
  , psRpcs :: ![ProtoRpc]
  } deriving (Show)


-- | An RPC method.
data ProtoRpc = ProtoRpc
  { prName         :: !Text
  , prInputType    :: !Text
  , prOutputType   :: !Text
  , prClientStream :: !Bool
  , prServerStream :: !Bool
  } deriving (Show)


-- | A message definition.
data ProtoMessage = ProtoMessage
  { pmName   :: !Text
  , pmFields :: ![ProtoField]
  } deriving (Show)


-- | A field in a message.
data ProtoField = ProtoField
  { pfFieldType   :: !Text
  , pfFieldName   :: !Text
  , pfFieldNumber :: !Int
  , pfRepeated    :: !Bool
  , pfOptional    :: !Bool
  } deriving (Show)


-- | A basic enum definition.
data ProtoEnum = ProtoEnum
  { peName   :: !Text
  , peValues :: ![(Text, Int)]
  } deriving (Show)


-- ===================================================================
-- Parsing
-- ===================================================================

-- | Parse a .proto file from disk.
parseProtoFile :: FilePath -> IO (Either ProtoParseError ProtoFile)
parseProtoFile path = do
  content <- TIO.readFile path
  pure (parseProtoText content)


-- | Parse proto3 text content.
parseProtoText :: Text -> Either ProtoParseError ProtoFile
parseProtoText input =
  let cleaned = stripComments input
      stmts   = splitStatements cleaned
  in buildProtoFile stmts


-- | Strip // and /* */ comments.
stripComments :: Text -> Text
stripComments = go
  where
    go t
      | T.null t = T.empty
      | Just rest <- T.stripPrefix "//" t =
          let (_, after) = T.breakOn "\n" rest
          in go (T.drop 1 after)  -- skip the \n itself
      | Just rest <- T.stripPrefix "/*" t =
          let (_, after) = T.breakOn "*/" rest
          in go (T.drop 2 after)  -- skip the */
      | otherwise =
          case T.uncons t of
            Just (c, rest) -> T.cons c (go rest)
            Nothing        -> T.empty


-- | Split on top-level statements, respecting braces.
-- Returns a list of statement strings, where brace-delimited blocks
-- are kept together.
splitStatements :: Text -> [Text]
splitStatements input = go 0 T.empty (T.unpack input)
  where
    go :: Int -> Text -> String -> [Text]
    go _ acc [] =
      let s = T.strip acc
      in [s | not (T.null s)]
    go depth acc ('{':cs) =
      go (depth + 1) (T.snoc acc '{') cs
    go depth acc ('}':cs) =
      let newDepth = max 0 (depth - 1)
          acc' = T.snoc acc '}'
      in if newDepth == 0
         then let s = T.strip acc'
              in [s | not (T.null s)] ++ go 0 T.empty cs
         else go newDepth acc' cs
    go 0 acc (';':cs) =
      let s = T.strip acc
      in [s | not (T.null s)] ++ go 0 T.empty cs
    go depth acc (c:cs) =
      go depth (T.snoc acc c) cs


-- | Build a ProtoFile from parsed top-level statements.
buildProtoFile :: [Text] -> Either ProtoParseError ProtoFile
buildProtoFile stmts =
  let syntax   = findSyntax stmts
      package  = findPackage stmts
      imports  = findImports stmts
      services = findServices stmts
      messages = findMessages stmts
      enums    = findEnums stmts
  in case syntax of
    Just s | s /= "proto3" -> Left (ProtoUnsupportedSyntax s)
    _ -> Right ProtoFile
      { protoSyntax   = maybe "proto3" id syntax
      , protoPackage  = maybe "" id package
      , protoServices = services
      , protoMessages = messages
      , protoEnums    = enums
      , protoImports  = imports
      }


-- | Extract syntax value.
findSyntax :: [Text] -> Maybe Text
findSyntax [] = Nothing
findSyntax (s:ss)
  | "syntax" `T.isPrefixOf` T.stripStart s =
      Just (extractQuoted s)
  | otherwise = findSyntax ss


-- | Extract package name.
findPackage :: [Text] -> Maybe Text
findPackage [] = Nothing
findPackage (s:ss)
  | "package" `T.isPrefixOf` T.stripStart s =
      let ws = T.words (T.stripStart s)
      in case ws of
        (_:name:_) -> Just (T.strip name)
        _ -> findPackage ss
  | otherwise = findPackage ss


-- | Extract import paths.
findImports :: [Text] -> [Text]
findImports = map extractQuoted . filter (\s -> "import" `T.isPrefixOf` T.stripStart s)


-- | Extract quoted string from a statement like: syntax = "proto3"
extractQuoted :: Text -> Text
extractQuoted t =
  let afterQuote = T.drop 1 (snd (T.breakOn "\"" t))
      value = fst (T.breakOn "\"" afterQuote)
  in value


-- | Parse all service blocks.
findServices :: [Text] -> [ProtoService]
findServices [] = []
findServices (s:ss)
  | isServiceBlock s = parseService s : findServices ss
  | otherwise = findServices ss


isServiceBlock :: Text -> Bool
isServiceBlock t =
  let stripped = T.stripStart t
  in "service " `T.isPrefixOf` stripped && T.isInfixOf "{" t


parseService :: Text -> ProtoService
parseService t =
  let stripped = T.stripStart t
      afterKeyword = T.drop 8 stripped  -- drop "service "
      (name, rest) = T.break (\c -> c == '{' || isSpace c) (T.stripStart afterKeyword)
      body = extractBraceBody rest
      rpcs = parseRpcs body
  in ProtoService (T.strip name) rpcs


-- | Extract content between first { and last }.
extractBraceBody :: Text -> Text
extractBraceBody t =
  let afterOpen = T.drop 1 (snd (T.breakOn "{" t))
      -- Find last }
      reversed = T.reverse afterOpen
      afterClose = T.drop 1 (snd (T.breakOn "}" reversed))
  in T.reverse afterClose


-- | Parse rpc statements from a service body.
parseRpcs :: Text -> [ProtoRpc]
parseRpcs body =
  let -- Split on "rpc " to find each rpc declaration
      parts = T.splitOn "rpc " body
  in [ rpc | p <- drop 1 parts, let rpc = parseRpc p ]


parseRpc :: Text -> ProtoRpc
parseRpc t =
  let -- Format: MethodName (Input) returns (Output) [;{}]
      stripped = T.stripStart t
      (name, rest1) = T.break (\c -> c == '(' || isSpace c) stripped
      -- Find input type in parens
      (clientStream, inputType) = extractParenType rest1
      -- Find "returns"
      afterReturns = T.drop 1 (snd (T.breakOn "returns" rest1))
      rest2 = T.drop 6 afterReturns  -- drop "eturns" (we dropped "r" with breakOn)
      (serverStream, outputType) = extractParenType (T.stripStart rest2)
  in ProtoRpc
    { prName         = T.strip name
    , prInputType    = inputType
    , prOutputType   = outputType
    , prClientStream = clientStream
    , prServerStream = serverStream
    }


-- | Extract type from parens, detecting "stream" keyword.
-- Input: "(stream Foo)" or "(Foo)"
-- Returns: (isStream, typeName)
extractParenType :: Text -> (Bool, Text)
extractParenType t =
  let afterOpen = T.drop 1 (snd (T.breakOn "(" t))
      inner = T.strip (fst (T.breakOn ")" afterOpen))
  in if "stream " `T.isPrefixOf` inner
     then (True, T.strip (T.drop 7 inner))
     else (False, inner)


-- | Parse all message blocks.
findMessages :: [Text] -> [ProtoMessage]
findMessages [] = []
findMessages (s:ss)
  | isMessageBlock s = parseMessage s : findMessages ss
  | otherwise = findMessages ss


isMessageBlock :: Text -> Bool
isMessageBlock t =
  let stripped = T.stripStart t
  in "message " `T.isPrefixOf` stripped && T.isInfixOf "{" t


parseMessage :: Text -> ProtoMessage
parseMessage t =
  let stripped = T.stripStart t
      afterKeyword = T.drop 8 stripped  -- drop "message "
      (name, rest) = T.break (\c -> c == '{' || isSpace c) (T.stripStart afterKeyword)
      body = extractBraceBody rest
      fields = parseFields body
  in ProtoMessage (T.strip name) fields


-- | Parse fields from a message body.
parseFields :: Text -> [ProtoField]
parseFields body =
  let lines_ = filter (not . T.null) $ map T.strip $ T.splitOn ";" body
      -- Filter out nested messages, enums, reserved, option statements, oneof blocks
      fieldLines = filter isFieldLine lines_
  in map parseField fieldLines


isFieldLine :: Text -> Bool
isFieldLine t =
  let stripped = T.stripStart t
  in not (T.null stripped)
     && not ("message " `T.isPrefixOf` stripped)
     && not ("enum " `T.isPrefixOf` stripped)
     && not ("reserved " `T.isPrefixOf` stripped)
     && not ("option " `T.isPrefixOf` stripped)
     && not ("oneof " `T.isPrefixOf` stripped)
     && not ("{" `T.isPrefixOf` stripped)
     && not ("}" `T.isPrefixOf` stripped)
     && T.any isDigit stripped  -- must have a field number
     && T.any (== '=') stripped


parseField :: Text -> ProtoField
parseField t =
  let ws = T.words (T.stripStart t)
  in case ws of
    ("repeated":typ:name:_:"=":numStr:_) ->
      ProtoField typ (cleanName name) (readInt numStr) True False
    ("repeated":typ:name:"=":numStr:_) ->
      ProtoField typ (cleanName name) (readInt numStr) True False
    ("optional":typ:name:_:"=":numStr:_) ->
      ProtoField typ (cleanName name) (readInt numStr) False True
    ("optional":typ:name:"=":numStr:_) ->
      ProtoField typ (cleanName name) (readInt numStr) False True
    (typ:name:"=":numStr:_) ->
      ProtoField typ (cleanName name) (readInt numStr) False False
    -- Handle "type name = num" without spaces around =
    (typ:nameEq:_) | T.isInfixOf "=" nameEq ->
      let (name, rest') = T.break (== '=') nameEq
          numStr = T.strip (T.drop 1 rest')
      in ProtoField typ (cleanName name) (readInt numStr) False False
    _ -> ProtoField "string" "unknown" 0 False False


cleanName :: Text -> Text
cleanName = T.filter (\c -> isAlphaNum c || c == '_')


readInt :: Text -> Int
readInt t =
  let digits = T.takeWhile isDigit (T.filter (\c -> isDigit c || c == '-') t)
  in if T.null digits then 0 else read (T.unpack digits)


-- | Parse all enum blocks.
findEnums :: [Text] -> [ProtoEnum]
findEnums [] = []
findEnums (s:ss)
  | isEnumBlock s = parseEnum s : findEnums ss
  | otherwise = findEnums ss


isEnumBlock :: Text -> Bool
isEnumBlock t =
  let stripped = T.stripStart t
  in "enum " `T.isPrefixOf` stripped && T.isInfixOf "{" t


parseEnum :: Text -> ProtoEnum
parseEnum t =
  let stripped = T.stripStart t
      afterKeyword = T.drop 5 stripped  -- drop "enum "
      (name, rest) = T.break (\c -> c == '{' || isSpace c) (T.stripStart afterKeyword)
      body = extractBraceBody rest
      values = parseEnumValues body
  in ProtoEnum (T.strip name) values


parseEnumValues :: Text -> [(Text, Int)]
parseEnumValues body =
  let lines_ = filter (not . T.null) $ map T.strip $ T.splitOn ";" body
      valLines = filter (\l -> T.any (== '=') l && not ("option " `T.isPrefixOf` l)) lines_
  in map parseEnumValue valLines


parseEnumValue :: Text -> (Text, Int)
parseEnumValue t =
  let ws = T.words (T.stripStart t)
  in case ws of
    (name:"=":numStr:_) -> (name, readInt numStr)
    _ -> ("UNKNOWN", 0)


-- ===================================================================
-- Conversion to IR
-- ===================================================================

-- | Convert a parsed proto file to the shared API IR.
protoToIR :: ProtoFile -> ApiIR
protoToIR pf = ApiIR
  { apiTitle     = if T.null (protoPackage pf)
                   then "Proto API"
                   else protoPackage pf
  , apiVersion   = "0.0.0"
  , apiEndpoints = concatMap serviceEndpoints (protoServices pf)
  , apiSchemas   = map messageToSchema (protoMessages pf)
                ++ map enumToSchema (protoEnums pf)
  }


-- | Convert a service's RPCs to endpoints.
-- gRPC uses POST for all RPCs, with path /ServiceName/MethodName.
serviceEndpoints :: ProtoService -> [EndpointIR]
serviceEndpoints svc = map (rpcToEndpoint (psName svc)) (psRpcs svc)


rpcToEndpoint :: Text -> ProtoRpc -> EndpointIR
rpcToEndpoint serviceName rpc = EndpointIR
  { epMethod       = POST
  , epPath         = [LitSegment serviceName, LitSegment (prName rpc)]
  , epOperationId  = Just (prName rpc)
  , epSummary      = Nothing
  , epRequestBody  = if isEmptyType (prInputType rpc)
                     then Nothing
                     else Just (SchemaRef (prInputType rpc))
  , epResponseType = if isEmptyType (prOutputType rpc)
                     then Nothing
                     else Just (SchemaRef (prOutputType rpc))
  , epStatusCode   = 200
  , epRequiresAuth = False
  }


-- | Check if a type represents "no data" (google.protobuf.Empty).
isEmptyType :: Text -> Bool
isEmptyType t = t == "google.protobuf.Empty" || t == "Empty"


-- | Convert a proto message to a named schema.
messageToSchema :: ProtoMessage -> (Text, SchemaIR)
messageToSchema msg =
  ( pmName msg
  , SchemaObject (pmName msg) (map fieldToFieldIR (pmFields msg))
  )


fieldToFieldIR :: ProtoField -> FieldIR
fieldToFieldIR pf =
  let baseSchema = protoTypeToSchema (pfFieldType pf)
      schema = if pfRepeated pf
               then SchemaArray baseSchema
               else baseSchema
  in FieldIR
    { fieldName     = pfFieldName pf
    , fieldSchema   = schema
    , fieldRequired = not (pfOptional pf)
    }


-- | Map proto types to schema IR types.
protoTypeToSchema :: Text -> SchemaIR
protoTypeToSchema "string"   = SchemaString
protoTypeToSchema "bytes"    = SchemaString
protoTypeToSchema "bool"     = SchemaBoolean
protoTypeToSchema "int32"    = SchemaInteger
protoTypeToSchema "int64"    = SchemaInteger
protoTypeToSchema "uint32"   = SchemaInteger
protoTypeToSchema "uint64"   = SchemaInteger
protoTypeToSchema "sint32"   = SchemaInteger
protoTypeToSchema "sint64"   = SchemaInteger
protoTypeToSchema "fixed32"  = SchemaInteger
protoTypeToSchema "fixed64"  = SchemaInteger
protoTypeToSchema "sfixed32" = SchemaInteger
protoTypeToSchema "sfixed64" = SchemaInteger
protoTypeToSchema "float"    = SchemaNumber
protoTypeToSchema "double"   = SchemaNumber
protoTypeToSchema name       = SchemaRef name  -- message reference


-- | Convert an enum to a schema (represented as a string with known values).
enumToSchema :: ProtoEnum -> (Text, SchemaIR)
enumToSchema e = (peName e, SchemaString)
