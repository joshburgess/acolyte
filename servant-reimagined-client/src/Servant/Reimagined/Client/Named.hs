{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Named endpoint client calls.
--
-- Instead of specifying the full endpoint type:
--
-- @
-- callEndpoint \@(Named "getUser" (Get UserByIdPath (Json User))) client 42
-- @
--
-- Use 'callNamed' with just the name and API type:
--
-- @
-- callNamed \@"getUser" \@API client 42
-- @
module Servant.Reimagined.Client.Named
  ( callNamed
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import qualified Data.Aeson as Aeson

import Servant.Reimagined.Core.Wrapper (LookupNamed)
import Servant.Reimagined.Client.Core (Client, ClientError)
import Servant.Reimagined.Client.Call (callEndpoint, EndpointRequest, ReqArgs, ReqResult)


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
