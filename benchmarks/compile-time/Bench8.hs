{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Bench8 where

import Acolyte.Core
import Acolyte.Server
import Data.Text (Text)
import Data.ByteString (ByteString)
import Http.Core (Request, Response)
import Tower.Service (Service)

-- | 8-endpoint API
type Path1 = '[ 'Lit "resource1" ]
type Path2 = '[ 'Lit "resource2" ]
type Path3 = '[ 'Lit "resource3" ]
type Path4 = '[ 'Lit "resource4" ]
type Path5 = '[ 'Lit "resource5" ]
type Path6 = '[ 'Lit "resource6" ]
type Path7 = '[ 'Lit "resource7" ]
type Path8 = '[ 'Lit "resource8" ]

type BenchAPI =
  '[
      Get Path1 Text
    , Get Path2 Text
    , Get Path3 Text
    , Get Path4 Text
    , Get Path5 Text
    , Get Path6 Text
    , Get Path7 Text
    , Get Path8 Text
   ]

-- Force the Serves constraint to be solved
benchHandlerCount :: Int
benchHandlerCount = handlerCount @BenchAPI @(H1, H2, H3, H4, H5, H6, H7, H8)

data H1
data H2
data H3
data H4
data H5
data H6
data H7
data H8
