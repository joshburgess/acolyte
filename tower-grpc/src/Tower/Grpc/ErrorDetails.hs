-- | Rich gRPC error details.
--
-- gRPC supports structured error details beyond a simple status code and
-- message. This module provides types for common error detail kinds
-- (bad request, debug info, retry info, help, resource info) and a way
-- to encode them into a 'GrpcResponse'.
--
-- Details are encoded as simple JSON in the @grpc-status-details-bin@
-- trailer, keeping the implementation dependency-free (no protobuf).
--
-- @
-- import Tower.Grpc.ErrorDetails
--
-- handler :: ByteString -> IO (Either GrpcStatus ByteString)
-- handler req = pure $ Left $ rsStatus $ richError (grpcInvalidArgument "bad input")
--   [ DetailBadRequest $ BadRequest
--       [ FieldViolation "name" "must not be empty"
--       , FieldViolation "age" "must be positive"
--       ]
--   ]
-- @
module Tower.Grpc.ErrorDetails
  ( -- * Rich error status
    RichGrpcStatus (..)
  , richError
    -- * Error detail types
  , ErrorDetail (..)
  , BadRequest (..)
  , FieldViolation (..)
  , DebugInfo (..)
  , RetryInfo (..)
  , Help (..)
  , HelpLink (..)
  , ResourceInfo (..)
    -- * Converting to GrpcResponse
  , richErrorResponse
    -- * Encoding
  , encodeDetails
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Tower.Grpc.Status (GrpcStatus (..))
import Tower.Grpc.Server (GrpcResponse (..))


-- | A gRPC status enriched with structured error details.
data RichGrpcStatus = RichGrpcStatus
  { rsStatus  :: !GrpcStatus
  , rsDetails :: ![ErrorDetail]
  } deriving (Show, Eq)


-- | Create a 'RichGrpcStatus'.
richError :: GrpcStatus -> [ErrorDetail] -> RichGrpcStatus
richError = RichGrpcStatus


-- | A structured error detail.
data ErrorDetail
  = DetailBadRequest !BadRequest
  | DetailDebugInfo !DebugInfo
  | DetailRetryInfo !RetryInfo
  | DetailHelp !Help
  | DetailResource !ResourceInfo
  deriving (Show, Eq)


-- | Describes violations in a client request.
data BadRequest = BadRequest
  { brFieldViolations :: ![FieldViolation]
  } deriving (Show, Eq)


-- | A single field violation within a 'BadRequest'.
data FieldViolation = FieldViolation
  { fvField       :: !Text
  , fvDescription :: !Text
  } deriving (Show, Eq)


-- | Debug information (stack traces, diagnostic messages).
data DebugInfo = DebugInfo
  { diStackEntries :: ![Text]
  , diDetail       :: !Text
  } deriving (Show, Eq)


-- | Retry information with a delay hint.
data RetryInfo = RetryInfo
  { riRetryDelay :: !Int  -- ^ Suggested retry delay in milliseconds
  } deriving (Show, Eq)


-- | Help links for the client.
data Help = Help
  { hLinks :: ![HelpLink]
  } deriving (Show, Eq)


-- | A single help link.
data HelpLink = HelpLink
  { hlDescription :: !Text
  , hlUrl         :: !Text
  } deriving (Show, Eq)


-- | Information about the resource involved in the error.
data ResourceInfo = ResourceInfo
  { riResourceType :: !Text
  , riResourceName :: !Text
  , riOwner        :: !Text
  , riDescription  :: !Text
  } deriving (Show, Eq)


-- | Convert a 'RichGrpcStatus' to a 'GrpcResponse'.
--
-- The error details are JSON-encoded and placed in the @grpc-message@
-- field, providing structured error information to clients that parse it.
-- The status code and human-readable summary come from 'rsStatus'.
richErrorResponse :: RichGrpcStatus -> GrpcResponse
richErrorResponse rich =
  let status = rsStatus rich
      details = rsDetails rich
      detailsJson = encodeDetails details
      -- Append details JSON to the message if there are details
      enrichedStatus = if null details
        then status
        else status { gsMessage = gsMessage status <> " [details:" <> detailsJson <> "]" }
  in GrpcError enrichedStatus


-- | Encode error details as a simple JSON string.
--
-- Uses a dependency-free JSON encoding (no aeson). The format is:
--
-- @
-- [{"@type":"BadRequest","field_violations":[{"field":"name","description":"required"}]}, ...]
-- @
encodeDetails :: [ErrorDetail] -> Text
encodeDetails details =
  "[" <> T.intercalate "," (map encodeDetail details) <> "]"


-- | Encode a single error detail as JSON.
encodeDetail :: ErrorDetail -> Text
encodeDetail (DetailBadRequest br) =
  "{\"@type\":\"BadRequest\",\"field_violations\":["
  <> T.intercalate "," (map encodeFieldViolation (brFieldViolations br))
  <> "]}"

encodeDetail (DetailDebugInfo di) =
  "{\"@type\":\"DebugInfo\",\"stack_entries\":["
  <> T.intercalate "," (map jsonString (diStackEntries di))
  <> "],\"detail\":" <> jsonString (diDetail di) <> "}"

encodeDetail (DetailRetryInfo ri) =
  "{\"@type\":\"RetryInfo\",\"retry_delay_ms\":" <> T.pack (show (riRetryDelay ri)) <> "}"

encodeDetail (DetailHelp h) =
  "{\"@type\":\"Help\",\"links\":["
  <> T.intercalate "," (map encodeHelpLink (hLinks h))
  <> "]}"

encodeDetail (DetailResource ri) =
  "{\"@type\":\"ResourceInfo\""
  <> ",\"resource_type\":" <> jsonString (riResourceType ri)
  <> ",\"resource_name\":" <> jsonString (riResourceName ri)
  <> ",\"owner\":" <> jsonString (riOwner ri)
  <> ",\"description\":" <> jsonString (riDescription ri)
  <> "}"


-- | Encode a field violation as JSON.
encodeFieldViolation :: FieldViolation -> Text
encodeFieldViolation fv =
  "{\"field\":" <> jsonString (fvField fv)
  <> ",\"description\":" <> jsonString (fvDescription fv) <> "}"


-- | Encode a help link as JSON.
encodeHelpLink :: HelpLink -> Text
encodeHelpLink hl =
  "{\"description\":" <> jsonString (hlDescription hl)
  <> ",\"url\":" <> jsonString (hlUrl hl) <> "}"


-- | Encode a Text value as a JSON string with basic escaping.
jsonString :: Text -> Text
jsonString t = "\"" <> escapeJson t <> "\""


-- | Escape special characters for JSON.
escapeJson :: Text -> Text
escapeJson = T.concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = T.singleton c
