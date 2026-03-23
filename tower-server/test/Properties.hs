{-# LANGUAGE TemplateHaskell #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types
  ( Status, ResponseHeaders, Header
  , status200, status201, status204, status400, status404, status500
  , statusCode
  )
import Tower.Server.Render (renderFull)


-- ===================================================================
-- Generators
-- ===================================================================

genStatus :: Gen Status
genStatus = Gen.element [status200, status201, status204, status400, status404, status500]

genHeaderName :: Gen ByteString
genHeaderName = Gen.element
  [ "X-Custom", "X-Request-Id", "X-Trace", "Cache-Control", "Accept" ]

genHeaderValue :: Gen ByteString
genHeaderValue = Gen.bytes (Range.linear 1 50)

genHeader :: Gen Header
genHeader = do
  name <- genHeaderName
  value <- genHeaderValue
  pure (CI.mk name, value)

genHeaders :: Gen ResponseHeaders
genHeaders = Gen.list (Range.linear 0 5) genHeader

genBody :: Gen ByteString
genBody = Gen.bytes (Range.linear 0 10000)


-- ===================================================================
-- Properties
-- ===================================================================

-- | renderFull produces valid HTTP: starts with "HTTP/1.1 ", contains the
-- status code, contains "Content-Length: N" matching body length, and ends
-- with the body bytes.
prop_renderFullValidHttp :: Property
prop_renderFullValidHttp = property $ do
  st <- forAll genStatus
  hdrs <- forAll genHeaders
  body <- forAll genBody
  let rendered = renderFull st hdrs body
      renderedStr = BS8.unpack rendered
      codeStr = show (statusCode st)
      clHeader = "Content-Length: " ++ show (BS.length body)
  -- Starts with HTTP/1.1
  assert $ BS.isPrefixOf "HTTP/1.1 " rendered
  -- Contains status code
  assert $ clHeader `elem` lines (filter (/= '\r') renderedStr)
         || BS8.pack clHeader `BS.isInfixOf` rendered
  -- Contains status code in status line
  assert $ BS8.pack codeStr `BS.isInfixOf` rendered
  -- Ends with the body bytes
  assert $ BS.isSuffixOf body rendered


-- | The Content-Length header value in the rendered output matches the
-- body length exactly.
prop_contentLengthAccuracy :: Property
prop_contentLengthAccuracy = property $ do
  st <- forAll genStatus
  hdrs <- forAll genHeaders
  body <- forAll genBody
  let rendered = renderFull st hdrs body
      -- Split on lines to find Content-Length header
      linesList = BS8.lines rendered
      clLines = filter (\l -> BS.isPrefixOf "Content-Length: " l) linesList
  -- There should be exactly one Content-Length header
  assert $ not (null clLines)
  let clLine = case clLines of { (x:_) -> x; [] -> error "unreachable" }
      -- Extract the value after "Content-Length: ", stripping trailing \r
      valBS = BS.drop (BS.length "Content-Length: ") clLine
      valStr = BS8.unpack (stripCR valBS)
  read valStr === BS.length body
  where
    stripCR bs
      | BS.null bs = bs
      | BS.last bs == 0x0d = BS.init bs  -- strip \r
      | otherwise = bs


-- | renderFull is deterministic: same inputs always produce same output.
prop_renderFullDeterministic :: Property
prop_renderFullDeterministic = property $ do
  st <- forAll genStatus
  hdrs <- forAll genHeaders
  body <- forAll genBody
  renderFull st hdrs body === renderFull st hdrs body


tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  ok <- tests
  if ok then pure () else error "Property tests failed"
