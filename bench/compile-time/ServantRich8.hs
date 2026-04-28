{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | 8-endpoint Servant benchmark with a richer combinator chain.
-- Path literal + Int capture + JSON request body + JSON response.
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
  =    "res1" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
  :<|> "res2" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
  :<|> "res3" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
  :<|> "res4" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
  :<|> "res5" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
  :<|> "res6" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
  :<|> "res7" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
  :<|> "res8" :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp

server :: Server API
server
  =    h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  where
    h :: Int -> Body -> Handler Resp
    h _ _ = return (Resp 0 "ok")

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = putStrLn "ok"
