-- | @servant-reimagined-codegen@ — generate API types from OpenAPI specs.
--
-- Reads Swagger 2.0 or OpenAPI 3.x JSON and produces Haskell modules
-- with type-level API definitions.
--
-- @
-- import Servant.Reimagined.Codegen
--
-- main = do
--   spec <- readSpecFile "openapi.json"
--   case spec of
--     Left err -> putStrLn (show err)
--     Right api -> putStrLn (T.unpack (emitModule defaultEmitConfig api))
-- @
module Servant.Reimagined.Codegen
  ( -- * Parsing
    parseSpec
  , ParseError (..)
  , readSpecFile
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
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)

import Servant.Reimagined.Codegen.IR
import Servant.Reimagined.Codegen.Parse
import Servant.Reimagined.Codegen.Emit


-- | Read and parse a spec file (JSON).
readSpecFile :: FilePath -> IO (Either ParseError ApiIR)
readSpecFile path = do
  bytes <- LBS.readFile path
  case Aeson.decode bytes of
    Nothing  -> pure (Left (InvalidJSON "Failed to decode JSON"))
    Just val -> pure (parseSpec val)
