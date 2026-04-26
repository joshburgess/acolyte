{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Bench1 where

import Acolyte.Core
import Acolyte.Server
import Data.Text (Text)
import Data.ByteString (ByteString)
import Http.Core (Request, Response)
import Tower.Service (Service)

-- | 1-endpoint API
type Path1 = '[ 'Lit "resource1" ]

type BenchAPI =
  '[
      Get Path1 Text
   ]

-- Force the Serves constraint to be solved
benchHandlerCount :: Int
benchHandlerCount = handlerCount @BenchAPI @H1

data H1
