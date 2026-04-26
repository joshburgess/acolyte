{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
-- | 32-endpoint benchmark using sub-API composition.
-- Split into two 16-endpoint sub-APIs since Serves supports up to 25
-- endpoints per flat API.
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Tower (Service)
import Http.Core (Request, Response)

import Acolyte.Core
import Acolyte.Server

-- Sub-API 1: paths 1-16
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

-- Sub-API 2: paths 17-32
type Path17 = '[ 'Lit "path17" ]
type Path18 = '[ 'Lit "path18" ]
type Path19 = '[ 'Lit "path19" ]
type Path20 = '[ 'Lit "path20" ]
type Path21 = '[ 'Lit "path21" ]
type Path22 = '[ 'Lit "path22" ]
type Path23 = '[ 'Lit "path23" ]
type Path24 = '[ 'Lit "path24" ]
type Path25 = '[ 'Lit "path25" ]
type Path26 = '[ 'Lit "path26" ]
type Path27 = '[ 'Lit "path27" ]
type Path28 = '[ 'Lit "path28" ]
type Path29 = '[ 'Lit "path29" ]
type Path30 = '[ 'Lit "path30" ]
type Path31 = '[ 'Lit "path31" ]
type Path32 = '[ 'Lit "path32" ]

type API1 =
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

type API2 =
  '[ Get Path17 Text
   , Get Path18 Text
   , Get Path19 Text
   , Get Path20 Text
   , Get Path21 Text
   , Get Path22 Text
   , Get Path23 Text
   , Get Path24 Text
   , Get Path25 Text
   , Get Path26 Text
   , Get Path27 Text
   , Get Path28 Text
   , Get Path29 Text
   , Get Path30 Text
   , Get Path31 Text
   , Get Path32 Text
   ]

server :: Service IO (Request ByteString) (Response ByteString)
server = combineServer2 @API1 @API2
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
  ( wrapHandler @(Get Path17 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path18 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path19 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path20 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path21 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path22 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path23 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path24 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path25 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path26 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path27 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path28 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path29 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path30 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path31 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  , wrapHandler @(Get Path32 Text) (\_ _ -> pure $ intoResponse ("ok" :: Text))
  )

main :: IO ()
main = putStrLn "ok"
