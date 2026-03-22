-- | Intermediate representation for API specs.
--
-- Both Swagger 2.0 and OpenAPI 3.x parse into this shared IR.
-- The code emitter generates Haskell from IR, not from raw JSON.
module Servant.Reimagined.Codegen.IR
  ( -- * API IR
    ApiIR (..)
  , EndpointIR (..)
  , PathSegmentIR (..)
  , SchemaIR (..)
  , FieldIR (..)
  , HttpMethod (..)
  ) where

import Data.Text (Text)


-- | An API parsed from a spec file.
data ApiIR = ApiIR
  { apiTitle     :: !Text
  , apiVersion   :: !Text
  , apiEndpoints :: ![EndpointIR]
  , apiSchemas   :: ![(Text, SchemaIR)]  -- named schemas
  } deriving (Show)


-- | A single endpoint.
data EndpointIR = EndpointIR
  { epMethod       :: !HttpMethod
  , epPath         :: ![PathSegmentIR]
  , epOperationId  :: !(Maybe Text)
  , epSummary      :: !(Maybe Text)
  , epRequestBody  :: !(Maybe SchemaIR)
  , epResponseType :: !(Maybe SchemaIR)
  , epStatusCode   :: !Int
  , epRequiresAuth :: !Bool
  } deriving (Show)


-- | A path segment.
data PathSegmentIR
  = LitSegment !Text
  | CaptureSegment !Text !Text  -- name, type (e.g., "id", "integer")
  deriving (Show, Eq)


-- | HTTP method.
data HttpMethod = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS
  deriving (Show, Eq)


-- | A schema (type description).
data SchemaIR
  = SchemaString
  | SchemaInteger
  | SchemaNumber
  | SchemaBoolean
  | SchemaArray !SchemaIR
  | SchemaObject !Text ![FieldIR]  -- name, fields
  | SchemaRef !Text                -- reference to a named schema
  deriving (Show)


-- | A field in an object schema.
data FieldIR = FieldIR
  { fieldName     :: !Text
  , fieldSchema   :: !SchemaIR
  , fieldRequired :: !Bool
  } deriving (Show)
