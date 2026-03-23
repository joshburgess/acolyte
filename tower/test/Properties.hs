{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Tower


-- | Identity layer does not change service behavior.
prop_identityLayer :: Property
prop_identityLayer = property $ do
  input <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
  let echo = Service pure :: Service IO String String
      wrapped = applyLayer identity echo
  result <- evalIO $ runService wrapped input
  expected <- evalIO $ runService echo input
  result === expected


-- | mapResponse composes covariantly: mapResponse g . mapResponse f == mapResponse (g . f)
prop_mapResponseComposition :: Property
prop_mapResponseComposition = property $ do
  input <- forAll $ Gen.int (Range.linear (-1000) 1000)
  let svc = Service pure :: Service IO Int Int
      f = (+ 1)
      g = (* 2)
      composed = mapResponse g (mapResponse f svc)
      fused    = mapResponse (g . f) svc
  r1 <- evalIO $ runService composed input
  r2 <- evalIO $ runService fused input
  r1 === r2


-- | mapRequest composes contravariantly: mapRequest f . mapRequest g == mapRequest (g . f)
prop_mapRequestContravariant :: Property
prop_mapRequestContravariant = property $ do
  input <- forAll $ Gen.int (Range.linear (-1000) 1000)
  let svc = Service pure :: Service IO Int Int
      f = (+ 1)
      g = (* 2)
      composed = mapRequest f (mapRequest g svc)
      fused    = mapRequest (g . f) svc
  r1 <- evalIO $ runService composed input
  r2 <- evalIO $ runService fused input
  r1 === r2


-- | dimap fusion: dimap f g (dimap h k svc) == dimap (h . f) (g . k) svc
prop_dimapFusion :: Property
prop_dimapFusion = property $ do
  input <- forAll $ Gen.int (Range.linear (-1000) 1000)
  let svc = Service pure :: Service IO Int Int
      f = (+ 1)
      g = (* 2)
      h = (+ 3)
      k = (* 5)
      nested = dimap f g (dimap h k svc)
      fused  = dimap (h . f) (g . k) svc
  r1 <- evalIO $ runService nested input
  r2 <- evalIO $ runService fused input
  r1 === r2


-- | Layer application order: (svc |> layer1) |> layer2 processes correctly.
-- Use 'before' layers that prepend to a list to verify ordering.
prop_layerApplicationOrder :: Property
prop_layerApplicationOrder = property $ do
  input <- forAll $ Gen.string (Range.linear 0 50) Gen.unicode
  let svc = Service (\req -> pure [req, "base"]) :: Service IO String [String]
      layer1 = middleware $ \(Service inner) -> Service $ \req -> do
        rs <- inner req
        pure ("layer1" : rs)
      layer2 = middleware $ \(Service inner) -> Service $ \req -> do
        rs <- inner req
        pure ("layer2" : rs)
      composed = (svc |> layer1) |> layer2
  result <- evalIO $ runService composed input
  -- layer2 wraps layer1 wraps svc, so layer2 prepends last (outermost)
  result === ["layer2", "layer1", input, "base"]


-- | before does not change the service's response.
prop_beforeDoesNotChangeResponse :: Property
prop_beforeDoesNotChangeResponse = property $ do
  input <- forAll $ Gen.int (Range.linear (-1000) 1000)
  let svc = Service pure :: Service IO Int Int
      wrapped = applyLayer (before (\_ -> pure ())) svc
  r1 <- evalIO $ runService svc input
  r2 <- evalIO $ runService wrapped input
  r1 === r2


-- | after does not change the service's response.
prop_afterDoesNotChangeResponse :: Property
prop_afterDoesNotChangeResponse = property $ do
  input <- forAll $ Gen.int (Range.linear (-1000) 1000)
  let svc = Service pure :: Service IO Int Int
      wrapped = applyLayer (after (\_ _ -> pure ())) svc
  r1 <- evalIO $ runService svc input
  r2 <- evalIO $ runService wrapped input
  r1 === r2


main :: IO Bool
main = checkParallel $$(discover)
