-- | Sub-API composition: combine multiple smaller APIs into one server.
--
-- Each sub-API uses flat tuple instances (O(1) compile time). The
-- combined server merges all routes and preserves compile-time
-- completeness checking across all sub-APIs.
--
-- @
-- type UsersAPI    = '[Get UsersPath ..., Post UsersPath ...]
-- type ArticlesAPI = '[Get ArticlesPath ..., Delete ArticlePath ...]
--
-- -- Combined type for OpenAPI / client generation
-- type FullAPI = UsersAPI ++ ArticlesAPI
--
-- -- Build server with completeness check for BOTH sub-APIs
-- server = combineServer2 @UsersAPI @ArticlesAPI
--   usersHandlers
--   articleHandlers
-- @
module Servant.Reimagined.Server.Combine
  ( -- * Combining 2 sub-APIs
    combineServer2
    -- * Combining 3 sub-APIs
  , combineServer3
    -- * Combining 4 sub-APIs
  , combineServer4
    -- * Combining 5 sub-APIs
  , combineServer5
    -- * Combining 6 sub-APIs
  , combineServer6
    -- * Combining 7 sub-APIs
  , combineServer7
    -- * Combining 8 sub-APIs
  , combineServer8
    -- * Low-level: build sub-API router and merge
  , subRouter
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

import Tower.Service (Service)
import Http.Core (Request, Response)

import Servant.Reimagined.Core.API (Serves, type (++))
import Servant.Reimagined.Server.Handler (HasEndpointInfo)
import Servant.Reimagined.Server.Router (Router, emptyRouter, serve)
import Servant.Reimagined.Server.Wiring (BuildServer, buildRouter)


-- | Build a router for a sub-API and merge into an existing router.
--
-- This is the building block. Each call is O(1) (flat tuple instance).
subRouter
  :: forall api handlers
   . (Serves api handlers, BuildServer api handlers)
  => handlers
  -> Router
  -> Router
subRouter handlers = buildRouter @api handlers


-- | Combine 2 sub-APIs with compile-time completeness for both.
combineServer2
  :: forall api1 api2 h1 h2
   . ( Serves api1 h1, BuildServer api1 h1
     , Serves api2 h2, BuildServer api2 h2
     )
  => h1 -> h2
  -> Service IO (Request ByteString) (Response ByteString)
combineServer2 h1 h2 = serve
  $ subRouter @api2 h2
  $ subRouter @api1 h1
  $ emptyRouter


-- | Combine 3 sub-APIs.
combineServer3
  :: forall api1 api2 api3 h1 h2 h3
   . ( Serves api1 h1, BuildServer api1 h1
     , Serves api2 h2, BuildServer api2 h2
     , Serves api3 h3, BuildServer api3 h3
     )
  => h1 -> h2 -> h3
  -> Service IO (Request ByteString) (Response ByteString)
combineServer3 h1 h2 h3 = serve
  $ subRouter @api3 h3
  $ subRouter @api2 h2
  $ subRouter @api1 h1
  $ emptyRouter


-- | Combine 4 sub-APIs.
combineServer4
  :: forall api1 api2 api3 api4 h1 h2 h3 h4
   . ( Serves api1 h1, BuildServer api1 h1
     , Serves api2 h2, BuildServer api2 h2
     , Serves api3 h3, BuildServer api3 h3
     , Serves api4 h4, BuildServer api4 h4
     )
  => h1 -> h2 -> h3 -> h4
  -> Service IO (Request ByteString) (Response ByteString)
combineServer4 h1 h2 h3 h4 = serve
  $ subRouter @api4 h4
  $ subRouter @api3 h3
  $ subRouter @api2 h2
  $ subRouter @api1 h1
  $ emptyRouter


-- | Combine 5 sub-APIs.
combineServer5
  :: forall api1 api2 api3 api4 api5 h1 h2 h3 h4 h5
   . ( Serves api1 h1, BuildServer api1 h1
     , Serves api2 h2, BuildServer api2 h2
     , Serves api3 h3, BuildServer api3 h3
     , Serves api4 h4, BuildServer api4 h4
     , Serves api5 h5, BuildServer api5 h5
     )
  => h1 -> h2 -> h3 -> h4 -> h5
  -> Service IO (Request ByteString) (Response ByteString)
combineServer5 h1 h2 h3 h4 h5 = serve
  $ subRouter @api5 h5
  $ subRouter @api4 h4
  $ subRouter @api3 h3
  $ subRouter @api2 h2
  $ subRouter @api1 h1
  $ emptyRouter


-- | Combine 6 sub-APIs.
combineServer6
  :: forall api1 api2 api3 api4 api5 api6 h1 h2 h3 h4 h5 h6
   . ( Serves api1 h1, BuildServer api1 h1
     , Serves api2 h2, BuildServer api2 h2
     , Serves api3 h3, BuildServer api3 h3
     , Serves api4 h4, BuildServer api4 h4
     , Serves api5 h5, BuildServer api5 h5
     , Serves api6 h6, BuildServer api6 h6
     )
  => h1 -> h2 -> h3 -> h4 -> h5 -> h6
  -> Service IO (Request ByteString) (Response ByteString)
combineServer6 h1 h2 h3 h4 h5 h6 = serve
  $ subRouter @api6 h6
  $ subRouter @api5 h5
  $ subRouter @api4 h4
  $ subRouter @api3 h3
  $ subRouter @api2 h2
  $ subRouter @api1 h1
  $ emptyRouter


-- | Combine 7 sub-APIs.
combineServer7
  :: forall api1 api2 api3 api4 api5 api6 api7 h1 h2 h3 h4 h5 h6 h7
   . ( Serves api1 h1, BuildServer api1 h1
     , Serves api2 h2, BuildServer api2 h2
     , Serves api3 h3, BuildServer api3 h3
     , Serves api4 h4, BuildServer api4 h4
     , Serves api5 h5, BuildServer api5 h5
     , Serves api6 h6, BuildServer api6 h6
     , Serves api7 h7, BuildServer api7 h7
     )
  => h1 -> h2 -> h3 -> h4 -> h5 -> h6 -> h7
  -> Service IO (Request ByteString) (Response ByteString)
combineServer7 h1 h2 h3 h4 h5 h6 h7 = serve
  $ subRouter @api7 h7
  $ subRouter @api6 h6
  $ subRouter @api5 h5
  $ subRouter @api4 h4
  $ subRouter @api3 h3
  $ subRouter @api2 h2
  $ subRouter @api1 h1
  $ emptyRouter


-- | Combine 8 sub-APIs.
combineServer8
  :: forall api1 api2 api3 api4 api5 api6 api7 api8 h1 h2 h3 h4 h5 h6 h7 h8
   . ( Serves api1 h1, BuildServer api1 h1
     , Serves api2 h2, BuildServer api2 h2
     , Serves api3 h3, BuildServer api3 h3
     , Serves api4 h4, BuildServer api4 h4
     , Serves api5 h5, BuildServer api5 h5
     , Serves api6 h6, BuildServer api6 h6
     , Serves api7 h7, BuildServer api7 h7
     , Serves api8 h8, BuildServer api8 h8
     )
  => h1 -> h2 -> h3 -> h4 -> h5 -> h6 -> h7 -> h8
  -> Service IO (Request ByteString) (Response ByteString)
combineServer8 h1 h2 h3 h4 h5 h6 h7 h8 = serve
  $ subRouter @api8 h8
  $ subRouter @api7 h7
  $ subRouter @api6 h6
  $ subRouter @api5 h5
  $ subRouter @api4 h4
  $ subRouter @api3 h3
  $ subRouter @api2 h2
  $ subRouter @api1 h1
  $ emptyRouter
