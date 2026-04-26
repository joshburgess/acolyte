-- | @acolyte-codegen@ — generate API types from OpenAPI specs.
--
-- Reads Swagger 2.0 or OpenAPI 3.x JSON and produces Haskell modules
-- with type-level API definitions.
--
-- @
-- import Acolyte.Codegen
--
-- main = do
--   spec <- readSpecFile "openapi.json"
--   case spec of
--     Left err -> putStrLn (show err)
--     Right api -> putStrLn (T.unpack (emitModule defaultEmitConfig api))
-- @
module Acolyte.Codegen
  ( -- * Parsing
    parseSpec
  , ParseError (..)
  , readSpecFile
    -- * Proto parsing
  , parseProtoFile
  , parseProtoText
  , protoToIR
  , generateFromProto
  , ProtoFile (..)
  , ProtoService (..)
  , ProtoRpc (..)
  , ProtoMessage (..)
  , ProtoField (..)
  , ProtoEnum (..)
  , ProtoParseError (..)
    -- * Proto diffing
  , diffProtos
  , ProtoDiff (..)
  , DiffSeverity (..)
    -- * IR
  , ApiIR (..)
  , EndpointIR (..)
  , PathSegmentIR (..)
  , SchemaIR (..)
  , FieldIR (..)
  , HttpMethod (..)
    -- * Code generation
  , emitModule
  , EmitConfig (..)
  , defaultEmitConfig
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import System.FilePath (takeExtension)

import Acolyte.Codegen.IR
import Acolyte.Codegen.Parse
import Acolyte.Codegen.Emit
import Acolyte.Codegen.Proto
import Acolyte.Codegen.ProtoDiff


-- | Read and parse a spec file. Supports both JSON and YAML.
--
-- File format is detected by extension:
-- * @.json@ → JSON parser
-- * @.yaml@, @.yml@ → YAML parser
-- * anything else → try YAML first (more permissive), fall back to JSON
readSpecFile :: FilePath -> IO (Either ParseError ApiIR)
readSpecFile path = do
  bytes <- BS.readFile path
  let ext = takeExtension path
      mVal = case ext of
        ".json" -> Aeson.decodeStrict' bytes
        ".yaml" -> either (const Nothing) Just (Yaml.decodeEither' bytes)
        ".yml"  -> either (const Nothing) Just (Yaml.decodeEither' bytes)
        _       -> case Yaml.decodeEither' bytes of
                     Right v  -> Just v
                     Left _   -> Aeson.decodeStrict' bytes
  case mVal of
    Nothing  -> pure (Left (InvalidJSON "Failed to decode file (tried JSON and YAML)"))
    Just val -> pure (parseSpec val)


-- | Generate Haskell code from a .proto file.
generateFromProto :: FilePath -> IO Text
generateFromProto path = do
  result <- parseProtoFile path
  case result of
    Left err -> error (show err)
    Right proto -> do
      let ir = protoToIR proto
      pure (emitModule defaultEmitConfig ir)
