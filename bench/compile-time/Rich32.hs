{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | 32-endpoint acolyte benchmark with a richer combinator chain.
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
type Path17 = Param "res17" Int
type Path18 = Param "res18" Int
type Path19 = Param "res19" Int
type Path20 = Param "res20" Int
type Path21 = Param "res21" Int
type Path22 = Param "res22" Int
type Path23 = Param "res23" Int
type Path24 = Param "res24" Int
type Path25 = Param "res25" Int
type Path26 = Param "res26" Int
type Path27 = Param "res27" Int
type Path28 = Param "res28" Int
type Path29 = Param "res29" Int
type Path30 = Param "res30" Int
type Path31 = Param "res31" Int
type Path32 = Param "res32" Int

-- Split into two sub-APIs since flat APIs support up to 25 endpoints.
type API1 =
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

type API2 =
  '[ Post Path17 (Json Body) (Json Resp)
   , Post Path18 (Json Body) (Json Resp)
   , Post Path19 (Json Body) (Json Resp)
   , Post Path20 (Json Body) (Json Resp)
   , Post Path21 (Json Body) (Json Resp)
   , Post Path22 (Json Body) (Json Resp)
   , Post Path23 (Json Body) (Json Resp)
   , Post Path24 (Json Body) (Json Resp)
   , Post Path25 (Json Body) (Json Resp)
   , Post Path26 (Json Body) (Json Resp)
   , Post Path27 (Json Body) (Json Resp)
   , Post Path28 (Json Body) (Json Resp)
   , Post Path29 (Json Body) (Json Resp)
   , Post Path30 (Json Body) (Json Resp)
   , Post Path31 (Json Body) (Json Resp)
   , Post Path32 (Json Body) (Json Resp)
   ]

h :: PathCapture Int -> JsonBody Body -> IO (Json Resp)
h _ _ = pure (Json (Resp 0 "ok"))

server :: Service IO (Request ByteString) (Response ByteString)
server = combineServer2 @API1 @API2
  (
      wrapHandler @(Post Path1 (Json Body) (Json Resp)) (toHandler h)
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
  (
      wrapHandler @(Post Path17 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path18 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path19 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path20 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path21 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path22 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path23 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path24 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path25 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path26 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path27 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path28 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path29 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path30 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path31 (Json Body) (Json Resp)) (toHandler h)
    , wrapHandler @(Post Path32 (Json Body) (Json Resp)) (toHandler h)
  )

main :: IO ()
main = putStrLn "ok"
