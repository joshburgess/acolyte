{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Exception (evaluate, try, SomeException)
import qualified Data.Text as T

import Servant.Reimagined.Codegen.Proto
  ( parseProtoText, ProtoFile(..) )


-- ===================================================================
-- Fuzz: proto parser never crashes on arbitrary input
-- ===================================================================

-- | Feed random unicode text to parseProtoText and assert it returns
-- Left or Right (never throws an uncaught exception).
prop_protoParserDoesNotCrash :: Property
prop_protoParserDoesNotCrash = property $ do
  txt <- forAll $ Gen.text (Range.linear 0 5000) Gen.unicode
  _ <- evalIO $ try @SomeException $ case parseProtoText txt of
    Left _  -> evaluate ()
    Right pf -> evaluate (length (protoServices pf)
      `seq` length (protoMessages pf)
      `seq` length (protoEnums pf)
      `seq` T.length (protoPackage pf)
      `seq` ())
  success


tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  ok <- tests
  if ok then pure () else error "Property tests failed"
