{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}
-- | 32-endpoint Servant benchmark. Flat :<|> tree, no sub-API splitting,
-- to expose Servant's HasServer instance resolution cost.
module Main (main) where

import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Servant
import Network.Wai (Application)

type API
  =    "path1"  :> Get '[JSON] Text
  :<|> "path2"  :> Get '[JSON] Text
  :<|> "path3"  :> Get '[JSON] Text
  :<|> "path4"  :> Get '[JSON] Text
  :<|> "path5"  :> Get '[JSON] Text
  :<|> "path6"  :> Get '[JSON] Text
  :<|> "path7"  :> Get '[JSON] Text
  :<|> "path8"  :> Get '[JSON] Text
  :<|> "path9"  :> Get '[JSON] Text
  :<|> "path10" :> Get '[JSON] Text
  :<|> "path11" :> Get '[JSON] Text
  :<|> "path12" :> Get '[JSON] Text
  :<|> "path13" :> Get '[JSON] Text
  :<|> "path14" :> Get '[JSON] Text
  :<|> "path15" :> Get '[JSON] Text
  :<|> "path16" :> Get '[JSON] Text
  :<|> "path17" :> Get '[JSON] Text
  :<|> "path18" :> Get '[JSON] Text
  :<|> "path19" :> Get '[JSON] Text
  :<|> "path20" :> Get '[JSON] Text
  :<|> "path21" :> Get '[JSON] Text
  :<|> "path22" :> Get '[JSON] Text
  :<|> "path23" :> Get '[JSON] Text
  :<|> "path24" :> Get '[JSON] Text
  :<|> "path25" :> Get '[JSON] Text
  :<|> "path26" :> Get '[JSON] Text
  :<|> "path27" :> Get '[JSON] Text
  :<|> "path28" :> Get '[JSON] Text
  :<|> "path29" :> Get '[JSON] Text
  :<|> "path30" :> Get '[JSON] Text
  :<|> "path31" :> Get '[JSON] Text
  :<|> "path32" :> Get '[JSON] Text

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
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
  :<|> h
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
