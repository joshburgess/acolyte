-- | @tower@ — composable service and middleware abstractions.
--
-- A standalone, framework-agnostic library. Use this for composable
-- middleware in any Haskell project — web servers, gRPC, message
-- queues, or anything else that processes requests.
--
-- @
-- import Tower
--
-- -- Define a service
-- echo :: Service IO String String
-- echo = Service pure
--
-- -- Define middleware
-- logging :: Middleware IO String String
-- logging = before $ \req -> putStrLn (">> " ++ req)
--
-- -- Compose
-- main :: IO ()
-- main = do
--   let svc = echo |> logging
--   result <- runService svc "hello"
--   putStrLn result
-- @
module Tower
  ( -- * Service
    Service (..)
  , mapRequest
  , mapResponse
  , mapResponseM
  , contramapRequest
  , dimap
  , hoistService

    -- * Layer
  , Layer (..)
  , Middleware

    -- * Construction
  , middleware

    -- * Application
  , applyLayer
  , (|>)
  , (<|)

    -- * Composition
  , composeLayer
  , identity

    -- * Combinators
  , before
  , after
  , around
  ) where

import Tower.Service
import Tower.Layer
