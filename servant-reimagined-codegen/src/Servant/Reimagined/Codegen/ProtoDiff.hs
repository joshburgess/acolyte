-- | Compare two proto files and report breaking/non-breaking changes.
--
-- Useful for CI pipelines that need to detect backwards-incompatible
-- changes in .proto definitions before deployment.
module Servant.Reimagined.Codegen.ProtoDiff
  ( diffProtos
  , ProtoDiff (..)
  , DiffSeverity (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Servant.Reimagined.Codegen.Proto


-- | Severity of a proto change.
data DiffSeverity
  = Breaking     -- ^ Backwards-incompatible change
  | NonBreaking  -- ^ Safe addition
  deriving (Show, Eq)


-- | A single difference between two proto files.
data ProtoDiff = ProtoDiff
  { pdSeverity    :: !DiffSeverity
  , pdDescription :: !Text
  } deriving (Show, Eq)


-- | Compare two proto files and report all differences.
diffProtos :: ProtoFile -> ProtoFile -> [ProtoDiff]
diffProtos old new = concat
  [ diffServices (protoServices old) (protoServices new)
  , diffMessages (protoMessages old) (protoMessages new)
  ]


-- ===================================================================
-- Service diffing
-- ===================================================================

diffServices :: [ProtoService] -> [ProtoService] -> [ProtoDiff]
diffServices oldSvcs newSvcs = concat
  [ -- Services removed (breaking)
    [ ProtoDiff Breaking ("Service removed: " <> psName s)
    | s <- oldSvcs
    , not (any (\n -> psName n == psName s) newSvcs)
    ]
  , -- Services added (non-breaking)
    [ ProtoDiff NonBreaking ("Service added: " <> psName s)
    | s <- newSvcs
    , not (any (\o -> psName o == psName s) oldSvcs)
    ]
  , -- Services in both: diff their RPCs
    [ d
    | o <- oldSvcs
    , n <- newSvcs
    , psName o == psName n
    , d <- diffRpcs (psName o) (psRpcs o) (psRpcs n)
    ]
  ]


diffRpcs :: Text -> [ProtoRpc] -> [ProtoRpc] -> [ProtoDiff]
diffRpcs svcName oldRpcs newRpcs = concat
  [ -- RPCs removed (breaking)
    [ ProtoDiff Breaking ("RPC removed: " <> svcName <> "." <> prName r)
    | r <- oldRpcs
    , not (any (\n -> prName n == prName r) newRpcs)
    ]
  , -- RPCs added (non-breaking)
    [ ProtoDiff NonBreaking ("RPC added: " <> svcName <> "." <> prName r)
    | r <- newRpcs
    , not (any (\o -> prName o == prName r) oldRpcs)
    ]
  , -- RPCs in both: check input/output type changes
    [ d
    | o <- oldRpcs
    , n <- newRpcs
    , prName o == prName n
    , d <- diffRpcTypes svcName o n
    ]
  ]


diffRpcTypes :: Text -> ProtoRpc -> ProtoRpc -> [ProtoDiff]
diffRpcTypes svcName old new = concat
  [ [ ProtoDiff Breaking
      ("RPC input type changed: " <> svcName <> "." <> prName old
        <> " (" <> prInputType old <> " -> " <> prInputType new <> ")")
    | prInputType old /= prInputType new
    ]
  , [ ProtoDiff Breaking
      ("RPC output type changed: " <> svcName <> "." <> prName old
        <> " (" <> prOutputType old <> " -> " <> prOutputType new <> ")")
    | prOutputType old /= prOutputType new
    ]
  ]


-- ===================================================================
-- Message diffing
-- ===================================================================

diffMessages :: [ProtoMessage] -> [ProtoMessage] -> [ProtoDiff]
diffMessages oldMsgs newMsgs = concat
  [ -- Messages added (non-breaking)
    [ ProtoDiff NonBreaking ("Message added: " <> pmName m)
    | m <- newMsgs
    , not (any (\o -> pmName o == pmName m) oldMsgs)
    ]
  , -- Messages in both: diff their fields
    [ d
    | o <- oldMsgs
    , n <- newMsgs
    , pmName o == pmName n
    , d <- diffFields (pmName o) (pmFields o) (pmFields n)
    ]
  ]


diffFields :: Text -> [ProtoField] -> [ProtoField] -> [ProtoDiff]
diffFields msgName oldFields newFields = concat
  [ -- Fields removed (breaking)
    [ ProtoDiff Breaking ("Field removed: " <> msgName <> "." <> pfFieldName f)
    | f <- oldFields
    , not (any (\n -> pfFieldName n == pfFieldName f) newFields)
    ]
  , -- Fields added (non-breaking)
    [ ProtoDiff NonBreaking ("Field added: " <> msgName <> "." <> pfFieldName f)
    | f <- newFields
    , not (any (\o -> pfFieldName o == pfFieldName f) oldFields)
    ]
  , -- Fields in both: check type and number changes
    [ d
    | o <- oldFields
    , n <- newFields
    , pfFieldName o == pfFieldName n
    , d <- diffFieldDetails msgName o n
    ]
  ]


diffFieldDetails :: Text -> ProtoField -> ProtoField -> [ProtoDiff]
diffFieldDetails msgName old new = concat
  [ [ ProtoDiff Breaking
      ("Field type changed: " <> msgName <> "." <> pfFieldName old
        <> " (" <> pfFieldType old <> " -> " <> pfFieldType new <> ")")
    | pfFieldType old /= pfFieldType new
    ]
  , [ ProtoDiff Breaking
      ("Field number changed: " <> msgName <> "." <> pfFieldName old
        <> " (" <> T.pack (show (pfFieldNumber old)) <> " -> " <> T.pack (show (pfFieldNumber new)) <> ")")
    | pfFieldNumber old /= pfFieldNumber new
    ]
  ]
