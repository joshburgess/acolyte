{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | 16-endpoint acolyte benchmark with a richer combinator chain.
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
type Path5 = Param "res5" Int
type Path6 = Param "res6" Int
type Path7 = Param "res7" Int
type Path8 = Param "res8" Int
type Path9 = Param "res9" Int
type Path10 = Param "res10" Int
type Path11 = Param "res11" Int
type Path12 = Param "res12" Int
type Path13 = Param "res13" Int
type Path14 = Param "res14" Int
type Path15 = Param "res15" Int
type Path16 = Param "res16" Int

type BenchAPI =
  '[ Post Path1 (Json Body) (Json Resp)
   , Post Path2 (Json Body) (Json Resp)
   , Post Path3 (Json Body) (Json Resp)
   , Post Path4 (Json Body) (Json Resp)
   , Post Path5 (Json Body) (Json Resp)
   , Post Path6 (Json Body) (Json Resp)
   , Post Path7 (Json Body) (Json Resp)
   , Post Path8 (Json Body) (Json Resp)
   , Post Path9 (Json Body) (Json Resp)
   , Post Path10 (Json Body) (Json Resp)
   , Post Path11 (Json Body) (Json Resp)
   , Post Path12 (Json Body) (Json Resp)
   , Post Path13 (Json Body) (Json Resp)
   , Post Path14 (Json Body) (Json Resp)
   , Post Path15 (Json Body) (Json Resp)
   , Post Path16 (Json Body) (Json Resp)
   ]

h :: PathCapture Int -> JsonBody Body -> IO (Json Resp)
h _ _ = pure (Json (Resp 0 "ok"))

server :: Service IO (Request ByteString) (Response ByteString)
server = mkServer @BenchAPI
  ( wrapHandler @(Post Path1 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path2 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path3 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path4 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path5 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path6 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path7 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path8 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path9 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path10 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path11 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path12 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path13 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path14 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path15 (Json Body) (Json Resp)) (toHandler h)
  , wrapHandler @(Post Path16 (Json Body) (Json Resp)) (toHandler h)
  )

main :: IO ()
main = putStrLn "ok"
