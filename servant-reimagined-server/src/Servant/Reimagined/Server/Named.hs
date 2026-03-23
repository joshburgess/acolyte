-- | Named routes: register handlers via a record instead of a positional tuple.
--
-- Define a record type whose fields correspond to your API endpoints,
-- write a 'NamedApi' instance to convert it to a handler tuple, and
-- use 'mkNamedApi' or 'effectfulNamedApi' instead of 'mkApi'/'effectfulApi'.
--
-- @
-- data Handlers = Handlers
--   { health     :: IO Text
--   , getUser    :: PathCapture Int -> IO (Json User)
--   , createUser :: JsonBody CreateUser -> IO (Json User)
--   }
--
-- instance NamedApi API Handlers (IO Text, PathCapture Int -> IO (Json User), JsonBody CreateUser -> IO (Json User)) where
--   toHandlerTuple h = (health h, getUser h, createUser h)
--
-- server = mkNamedApi \@API Handlers { health = ..., getUser = ..., createUser = ... }
-- @
module Servant.Reimagined.Server.Named
  ( -- * Named handler conversion
    NamedApi (..)
  , mkNamedApi
  , effectfulNamedApi
  ) where

import Data.Kind (Type)
import Data.ByteString (ByteString)

import Tower.Service (Service)
import Http.Core (Request, Response)

import Servant.Reimagined.Core.API (Serves)
import Servant.Reimagined.Server.MkApi (BuildApi, mkApi)
import Servant.Reimagined.Server.Effects (EffectfulServer (..), effectfulApi)


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
