{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}
-- | 8-endpoint Servant benchmark.
module Main (main) where

import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Servant
import Network.Wai (Application)

type API
  =    "path1" :> Get '[JSON] Text
  :<|> "path2" :> Get '[JSON] Text
  :<|> "path3" :> Get '[JSON] Text
  :<|> "path4" :> Get '[JSON] Text
  :<|> "path5" :> Get '[JSON] Text
  :<|> "path6" :> Get '[JSON] Text
  :<|> "path7" :> Get '[JSON] Text
  :<|> "path8" :> Get '[JSON] Text

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
    h = return ("ok" :: Text)

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = putStrLn "ok"
