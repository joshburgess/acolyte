{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | 1-endpoint Servant benchmark with a richer combinator chain.
-- Mirrors Rich1 (path literal + Int capture + JSON request body +
-- JSON response with generic-derived FromJSON/ToJSON).
module Main (main) where

import Data.Text (Text)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant
import Network.Wai (Application)

data Body = Body { bName :: Text, bValue :: Int }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Resp = Resp { rId :: Int, rData :: Text }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type API
  = "res1" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp

server :: Server API
server = h
  where
    h :: Int -> Body -> Handler Resp
    h _ _ = return (Resp 0 "ok")

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = putStrLn "ok"
