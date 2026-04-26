{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Tower (Service)
import Http.Core (Request, Response)

import Acolyte.Core
import Acolyte.Server

type Path1  = '[ 'Lit "path1" ]
type Path2  = '[ 'Lit "path2" ]
type Path3  = '[ 'Lit "path3" ]
type Path4  = '[ 'Lit "path4" ]
type Path5  = '[ 'Lit "path5" ]
type Path6  = '[ 'Lit "path6" ]
type Path7  = '[ 'Lit "path7" ]
type Path8  = '[ 'Lit "path8" ]
type Path9  = '[ 'Lit "path9" ]
type Path10 = '[ 'Lit "path10" ]
type Path11 = '[ 'Lit "path11" ]
type Path12 = '[ 'Lit "path12" ]
type Path13 = '[ 'Lit "path13" ]
type Path14 = '[ 'Lit "path14" ]
type Path15 = '[ 'Lit "path15" ]
type Path16 = '[ 'Lit "path16" ]

type BenchAPI =
  '[ Get Path1  Text
   , Get Path2  Text
   , Get Path3  Text
   , Get Path4  Text
   , Get Path5  Text
   , Get Path6  Text
   , Get Path7  Text
   , Get Path8  Text
   , Get Path9  Text
   , Get Path10 Text
   , Get Path11 Text
   , Get Path12 Text
   , Get Path13 Text
   , Get Path14 Text
   , Get Path15 Text
   , Get Path16 Text
   ]

server :: Service IO (Request ByteString) (Response ByteString)
server = mkServer @BenchAPI
  ( wrapHandler @(Get Path1  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path2  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path3  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path4  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path5  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path6  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path7  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path8  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path9  Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path10 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path11 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path12 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path13 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path14 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path15 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path16 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  )

main :: IO ()
main = putStrLn "ok"
