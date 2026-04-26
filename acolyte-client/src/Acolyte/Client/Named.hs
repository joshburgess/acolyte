{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Named endpoint client calls and record-based client generation.
--
-- 'callNamed' looks up an endpoint by its type-level name:
--
-- @
-- callNamed \@"getUser" \@API client 42
-- @
--
-- 'mkClientRecord' builds a record of client functions from a Named API:
--
-- @
-- data MyClient = MyClient
--   { health  :: () -> IO (Either ClientError Text)
--   , getUser :: Int -> IO (Either ClientError User)
--   } deriving (Generic)
--
-- myClient = mkClientRecord \@API client :: MyClient
-- @
module Acolyte.Client.Named
  ( -- * Named endpoint calls
    callNamed
    -- * Record-based client generation
  , mkClientRecord
  , GBuildClientRecord (..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Data.Aeson as Aeson

import Acolyte.Core.Wrapper (LookupNamed)
import Acolyte.Client.Core (Client, ClientError)
import Acolyte.Client.Call (callEndpoint, EndpointRequest, ReqArgs, ReqResult)


-- | Call a named endpoint by its label, looking it up in the API type.
--
-- @
-- type API = '[ Named "health"  (Get (At "health") Text)
--             , Named "getUser" (Get (Param "users" Int) (Json User))
--             ]
--
-- health <- callNamed \@"health"  \@API client ()
-- user   <- callNamed \@"getUser" \@API client 42
-- @
callNamed
  :: forall (name :: Symbol) (api :: [Type]) endpoint
   . ( endpoint ~ LookupNamed name api
     , EndpointRequest endpoint
     , Aeson.FromJSON (ReqResult endpoint)
     )
  => Client
  -> ReqArgs endpoint
  -> IO (Either ClientError (ReqResult endpoint))
callNamed = callEndpoint @endpoint


-- ===================================================================
-- Record-based client generation
-- ===================================================================

-- | Build a record of client functions from a Named API.
--
-- Each record field must match a @Named@ endpoint label. The field
-- type must be @ReqArgs endpoint -> IO (Either ClientError (ReqResult endpoint))@.
--
-- @
-- type API = '[ Named "health"  (Get (At "health") Text)
--             , Named "getUser" (Get (Param "users" Int) (Json User))
--             ]
--
-- data MyClient = MyClient
--   { health  :: () -> IO (Either ClientError Text)
--   , getUser :: Int -> IO (Either ClientError User)
--   } deriving (Generic)
--
-- myClient = mkClientRecord \@API client :: MyClient
-- @
mkClientRecord
  :: forall api record
   . (Generic record, GBuildClientRecord api (Rep record))
  => Client -> record
mkClientRecord client = to (gBuildClientRecord @api client)


-- | Walk a Generic representation, matching record field names to
-- Named API endpoint labels and producing client call functions.
class GBuildClientRecord (api :: [Type]) (f :: Type -> Type) where
  gBuildClientRecord :: Client -> f p

-- Datatype metadata: delegate to inner
instance GBuildClientRecord api f => GBuildClientRecord api (D1 meta f) where
  gBuildClientRecord client = M1 (gBuildClientRecord @api client)

-- Constructor metadata: delegate to inner
instance GBuildClientRecord api f => GBuildClientRecord api (C1 meta f) where
  gBuildClientRecord client = M1 (gBuildClientRecord @api client)

-- Product: build left and right independently
instance (GBuildClientRecord api f, GBuildClientRecord api g)
  => GBuildClientRecord api (f :*: g) where
  gBuildClientRecord client =
    gBuildClientRecord @api client :*: gBuildClientRecord @api client

-- Record selector: match field name to Named endpoint, produce client function
instance
  ( KnownSymbol name
  , endpoint ~ LookupNamed name api
  , EndpointRequest endpoint
  , Aeson.FromJSON (ReqResult endpoint)
  , fieldType ~ (ReqArgs endpoint -> IO (Either ClientError (ReqResult endpoint)))
  ) => GBuildClientRecord api (S1 ('MetaSel ('Just name) su ss ds) (Rec0 fieldType)) where
  gBuildClientRecord client = M1 (K1 (callEndpoint @endpoint client))

-- Unit: no fields
instance GBuildClientRecord api U1 where
  gBuildClientRecord _ = U1
