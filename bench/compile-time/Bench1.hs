{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Tower (Service)
import Http.Core (Request, Response)

import Acolyte.Core
import Acolyte.Server

type Path1 = '[ 'Lit "path1" ]

type BenchAPI =
  '[ Get Path1 Text
   ]

server :: Service IO (Request ByteString) (Response ByteString)
server = mkServer @BenchAPI
  ( wrapHandler @(Get Path1 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  )

main :: IO ()
main = putStrLn "ok"
