{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
-- | Minimal servant-reimagined server: one GET endpoint, no middleware.
--
-- Run:  cabal run minimal
-- Test: curl http://localhost:3000/hello
module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Servant.Reimagined.Prelude

-- | The API: a single GET /hello endpoint returning plain text.
type HelloAPI = '[ Get (At "hello") Text ]

-- | The handler: just returns a greeting.
helloHandler :: IO Text
helloHandler = pure "Hello, world!"

-- | Wire up and run. mkApi matches handlers to endpoints positionally.
server :: Service IO (Request ByteString) (Response ByteString)
server = mkApi @HelloAPI helloHandler

main :: IO ()
main = do
  putStrLn "Listening on http://localhost:3000/hello"
  runServerBS 3000 server
