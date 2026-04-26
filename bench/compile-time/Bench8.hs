{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Spire (Service)
import Http.Core (Request, Response)

import Acolyte.Core
import Acolyte.Server

type Path1 = '[ 'Lit "path1" ]
type Path2 = '[ 'Lit "path2" ]
type Path3 = '[ 'Lit "path3" ]
type Path4 = '[ 'Lit "path4" ]
type Path5 = '[ 'Lit "path5" ]
type Path6 = '[ 'Lit "path6" ]
type Path7 = '[ 'Lit "path7" ]
type Path8 = '[ 'Lit "path8" ]

type BenchAPI =
  '[ Get Path1 Text
   , Get Path2 Text
   , Get Path3 Text
   , Get Path4 Text
   , Get Path5 Text
   , Get Path6 Text
   , Get Path7 Text
   , Get Path8 Text
   ]

server :: Service IO (Request ByteString) (Response ByteString)
server = mkServer @BenchAPI
  ( wrapHandler @(Get Path1 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path2 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path3 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path4 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path5 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path6 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path7 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path8 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  )

main :: IO ()
main = putStrLn "ok"
