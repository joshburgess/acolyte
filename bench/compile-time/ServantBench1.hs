{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}
-- | 1-endpoint Servant benchmark.
module Main (main) where

import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Servant
import Network.Wai (Application)

type API = "path1" :> Get '[JSON] Text

server :: Server API
server = return ("ok" :: Text)

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = putStrLn "ok"
