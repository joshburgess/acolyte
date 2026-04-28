{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | 4-endpoint acolyte benchmark with a richer combinator chain.
-- Path literal + Int capture + JSON request body + JSON response.
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Spire (Service)
import Http.Core (Request, Response)
import Acolyte.Core
import Acolyte.Server

data Body = Body { bName :: Text, bValue :: Int }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Resp = Resp { rId :: Int, rData :: Text }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type Path1 = Param "res1" Int
type Path2 = Param "res2" Int
type Path3 = Param "res3" Int
type Path4 = Param "res4" Int

type BenchAPI =
  '[ Post Path1 (Json Body) (Json Resp)
   , Post Path2 (Json Body) (Json Resp)
   , Post Path3 (Json Body) (Json Resp)
   , Post Path4 (Json Body) (Json Resp)
   ]

h :: PathCapture Int -> JsonBody Body -> IO (Json Resp)
h _ _ = pure (Json (Resp 0 "ok"))

server :: Service IO (Request ByteString) (Response ByteString)
server = mkServer @BenchAPI
  ( wrapHandler @(Post Path1 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path2 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path3 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path4 (Json Body) (Json Resp)) (toHandler h)
  )

main :: IO ()
main = putStrLn "ok"
