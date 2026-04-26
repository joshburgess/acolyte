{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Bench4 where

import Acolyte.Core
import Acolyte.Server
import Data.Text (Text)
import Data.ByteString (ByteString)
import Http.Core (Request, Response)
import Spire.Service (Service)

-- | 4-endpoint API
type Path1 = '[ 'Lit "resource1" ]
type Path2 = '[ 'Lit "resource2" ]
type Path3 = '[ 'Lit "resource3" ]
type Path4 = '[ 'Lit "resource4" ]

type BenchAPI =
  '[
      Get Path1 Text
    , Get Path2 Text
    , Get Path3 Text
    , Get Path4 Text
   ]

-- Force the Serves constraint to be solved
benchHandlerCount :: Int
benchHandlerCount = handlerCount @BenchAPI @(H1, H2, H3, H4)

data H1
data H2
data H3
data H4
