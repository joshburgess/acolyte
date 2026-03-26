{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Named routes: register handlers via a record instead of a positional tuple.
--
-- Two approaches are provided:
--
-- == Manual: 'NamedApi' (no 'Named' wrapper required)
--
-- Define a record, write a 'NamedApi' instance that maps it to a tuple,
-- and use 'mkNamedApi'. The mapping is explicit and positional.
--
-- == Automatic: 'mkRecordApi' (requires 'Named' wrappers)
--
-- Wrap each endpoint with @Named "fieldName"@, define a record whose
-- field names match the endpoint names, and use 'mkRecordApi'. GHC's
-- 'HasField' instances handle the matching — field order doesn't matter.
--
-- @
-- type API =
--   '[ Named "health"  (Get (At "health") Text)
--    , Named "getUser" (Get (Param "users" Int) (Json User))
--    ]
--
-- data Handlers = Handlers
--   { getUser :: PathCapture Int -> IO (Json User)  -- order doesn't matter
--   , health  :: IO Text
--   }
--
-- server = mkRecordApi \@API Handlers { getUser = ..., health = ... }
-- @
module Servant.Reimagined.Server.Named
  ( -- * Manual named handler conversion
    NamedApi (..)
  , mkNamedApi
  , effectfulNamedApi
    -- * Automatic record-based handler binding
  , BuildRecordApi (..)
  , mkRecordApi
  , effectfulRecordApi
  ) where

import Data.Kind (Type)
import Data.ByteString (ByteString)
import GHC.Records (HasField (..))

import Tower.Service (Service)
import Http.Core (Request, Response)

import Servant.Reimagined.Core.API (Serves)
import Servant.Reimagined.Core.Wrapper (Named, AllNamed, EndpointNames, NoDuplicateNames)
import Servant.Reimagined.Server.MkApi (BuildApi, mkApi, toBoundHandler)
import Servant.Reimagined.Server.Handler (HasEndpointInfo)
import Servant.Reimagined.Server.ToHandler (ToHandler)
import Servant.Reimagined.Server.Effects (EffectfulServer (..), effectfulApi, fromRouter)
import Servant.Reimagined.Server.Router (Router, emptyRouter, addRoute)
import qualified Servant.Reimagined.Server.Router as Router


-- ===================================================================
-- Manual approach: NamedApi (backward-compatible)
-- ===================================================================

-- | Type class for converting a named handler record to a positional tuple
-- for use with 'mkApi'.
--
-- The @api@ parameter is the API type (a type-level list of endpoints).
-- @record@ is the user's handler record type. @tuple@ is the handler
-- tuple that 'BuildApi' expects.
--
-- The functional dependency @record -> tuple@ means each record type
-- determines a unique tuple type — GHC can infer the tuple from the record.
class NamedApi (api :: [Type]) record tuple | record -> tuple where
  toHandlerTuple :: record -> tuple


-- | Build a tower 'Service' from a named handler record.
--
-- Equivalent to @mkApi \@api . toHandlerTuple@.
--
-- @
-- server = mkNamedApi \@API MyHandlers { ... }
-- @
mkNamedApi
  :: forall api record tuple
   . (NamedApi api record tuple, Serves api tuple, BuildApi api tuple)
  => record
  -> Service IO (Request ByteString) (Response ByteString)
mkNamedApi record = mkApi @api (toHandlerTuple @api record)


-- | Build an 'EffectfulServer' from a named handler record.
--
-- Equivalent to @effectfulApi \@api . toHandlerTuple@.
--
-- @
-- server = run $ effectfulNamedApi \@API MyHandlers { ... }
-- @
effectfulNamedApi
  :: forall api record tuple
   . (NamedApi api record tuple, Serves api tuple, BuildApi api tuple)
  => record
  -> EffectfulServer api '[]
effectfulNamedApi record = effectfulApi @api (toHandlerTuple @api record)


-- ===================================================================
-- Automatic approach: BuildRecordApi (requires Named wrappers)
-- ===================================================================

-- | Recursively walk a Named API list, extracting handlers from a record
-- via 'HasField' and adding them to the router.
--
-- Each endpoint must be wrapped with @Named "fieldName"@. The record
-- must have a field with that name whose type satisfies 'ToHandler'.
class BuildRecordApi (api :: [Type]) record where
  buildRecordApi :: record -> Router -> Router

instance BuildRecordApi '[] record where
  buildRecordApi _ router = router

instance
  ( HasField name record handler
  , HasEndpointInfo (Named name endpoint)
  , ToHandler handler
  , BuildRecordApi rest record
  ) => BuildRecordApi (Named name endpoint ': rest) record where
  buildRecordApi rec router =
    buildRecordApi @rest rec
      (addRoute (toBoundHandler @(Named name endpoint) (getField @name rec)) router)


-- | Build a tower 'Service' from a Named API and a handler record.
--
-- Record fields are matched to endpoints by name — field order does
-- not matter. Every endpoint in the API must be wrapped with 'Named',
-- all names must be unique, and the record must have a matching field
-- for each name.
--
-- @
-- type API =
--   '[ Named "health"     (Get (At "health") Text)
--    , Named "getUser"    (Get (Param "users" Int) (Json User))
--    , Named "createUser" (Post (At "users") (Json CreateUser) (Json User))
--    ]
--
-- data Handlers = Handlers
--   { createUser :: JsonBody CreateUser -> IO (Json User)
--   , health     :: IO Text
--   , getUser    :: PathCapture Int -> IO (Json User)
--   }
--
-- server = mkRecordApi \@API Handlers { ... }
-- @
mkRecordApi
  :: forall api record
   . (AllNamed api, NoDuplicateNames (EndpointNames api), BuildRecordApi api record)
  => record
  -> Service IO (Request ByteString) (Response ByteString)
mkRecordApi record = Router.serve (buildRecordApi @api record emptyRouter)


-- | Build an 'EffectfulServer' from a Named API and a handler record.
--
-- Like 'mkRecordApi' but returns an 'EffectfulServer' for use with
-- 'provide' and 'run'.
--
-- @
-- server = run $ effectfulRecordApi \@API Handlers { ... }
-- @
effectfulRecordApi
  :: forall api record
   . (AllNamed api, NoDuplicateNames (EndpointNames api), BuildRecordApi api record)
  => record
  -> EffectfulServer api '[]
effectfulRecordApi record = fromRouter (buildRecordApi @api record emptyRouter)
