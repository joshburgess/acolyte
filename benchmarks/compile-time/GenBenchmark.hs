#!/usr/bin/env runghc
-- | Generate compile-time benchmark modules with N endpoints.
--
-- Usage: runghc GenBenchmark.hs
-- Generates: Bench5.hs, Bench10.hs, Bench20.hs, Bench40.hs, Bench80.hs
module Main where

import System.IO

genModule :: Int -> String
genModule n = unlines $
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE TypeFamilies #-}"
  , "{-# LANGUAGE TypeOperators #-}"
  , "{-# LANGUAGE AllowAmbiguousTypes #-}"
  , "{-# LANGUAGE UndecidableInstances #-}"
  , "{-# LANGUAGE ConstraintKinds #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module Bench" ++ show n ++ " where"
  , ""
  , "import Acolyte.Core"
  , "import Acolyte.Server"
  , "import Data.Text (Text)"
  , "import Data.ByteString (ByteString)"
  , "import Http.Core (Request, Response)"
  , "import Spire.Service (Service)"
  , ""
  , "-- | " ++ show n ++ "-endpoint API"
  ] ++
  -- Path type aliases
  [ "type Path" ++ show i ++ " = '[ 'Lit \"resource" ++ show i ++ "\" ]"
  | i <- [1..n]
  ] ++
  [ ""
  , "type BenchAPI ="
  , "  '["
  ] ++
  -- Endpoint list (first has no comma, rest have leading comma)
  [ "    " ++ (if i == 1 then "  " else ", ") ++ "Get Path" ++ show i ++ " Text"
  | i <- [1..n]
  ] ++
  [ "   ]"
  , ""
  , "-- Force the Serves constraint to be solved"
  , "benchHandlerCount :: Int"
  , "benchHandlerCount = handlerCount @BenchAPI @" ++ handlerTuple n
  , ""
  ] ++
  -- Handler data types
  [ "data H" ++ show i
  | i <- [1..n]
  ]

handlerTuple :: Int -> String
handlerTuple 1 = "H1"
handlerTuple n = "(" ++ go 1 ++ ")"
  where
    go i | i == n    = "H" ++ show i
         | otherwise = "H" ++ show i ++ ", " ++ go (i + 1)

main :: IO ()
main = do
  -- Serves instances go up to 16. Test within that range.
  mapM_ (\n -> writeFile ("Bench" ++ show n ++ ".hs") (genModule n)) [1, 4, 8, 12, 16]
  putStrLn "Generated: Bench1.hs Bench4.hs Bench8.hs Bench12.hs Bench16.hs"
