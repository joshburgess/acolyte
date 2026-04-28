{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | 1-endpoint acolyte benchmark with a richer combinator chain.
-- Each endpoint has a path literal, an Int capture, a JSON request body,
-- and a JSON response with generic-derived FromJSON/ToJSON instances.
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

type BenchAPI =
  '[ Post Path1 (Json Body) (Json Resp)
   ]

h :: PathCapture Int -> JsonBody Body -> IO (Json Resp)
h _ _ = pure (Json (Resp 0 "ok"))

server :: Service IO (Request ByteString) (Response ByteString)
server = mkServer @BenchAPI
  ( wrapHandler @(Post Path1 (Json Body) (Json Resp)) (toHandler h)
  )

main :: IO ()
main = putStrLn "ok"
