module Main (main) where

import Spire
import Data.IORef


-- ===================================================================
-- Test helpers
-- ===================================================================

assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


-- ===================================================================
-- 1. Basic Service creation and execution
-- ===================================================================

testServiceBasic :: IO ()
testServiceBasic = do
  -- Pure echo
  let echo :: Service IO String String
      echo = Service pure
  result <- runService echo "hello"
  assert "echo service returns input" (result == "hello")

  -- Service with computation
  let double :: Service IO Int Int
      double = Service $ \n -> pure (n * 2)
  result2 <- runService double 21
  assert "double service computes" (result2 == 42)


-- ===================================================================
-- 2. mapResponse
-- ===================================================================

testMapResponse :: IO ()
testMapResponse = do
  let svc :: Service IO Int Int
      svc = Service $ \n -> pure (n + 1)
      svc' = mapResponse (* 10) svc
  result <- runService svc' 5
  assert "mapResponse transforms output" (result == 60)  -- (5+1)*10


-- ===================================================================
-- 3. mapRequest
-- ===================================================================

testMapRequest :: IO ()
testMapRequest = do
  let svc :: Service IO Int String
      svc = Service $ \n -> pure (show n)
      svc' = mapRequest length svc  -- String -> Int via length
  result <- runService svc' "hello"
  assert "mapRequest transforms input" (result == "5")


-- ===================================================================
-- 4. dimap
-- ===================================================================

testDimap :: IO ()
testDimap = do
  let svc :: Service IO Int Int
      svc = Service $ \n -> pure (n * 2)
      svc' = dimap length show svc  -- String -> show(length*2)
  result <- runService svc' "hey"
  assert "dimap transforms both" (result == "6")  -- length "hey" = 3, *2 = 6


-- ===================================================================
-- 5. Middleware via `middleware` constructor
-- ===================================================================

testMiddleware :: IO ()
testMiddleware = do
  ref <- newIORef ([] :: [String])
  let svc :: Service IO String String
      svc = Service $ \req -> do
        modifyIORef' ref (++ ["handler:" ++ req])
        pure ("resp:" ++ req)

      logging :: Middleware IO String String
      logging = middleware $ \inner -> Service $ \req -> do
        modifyIORef' ref (++ ["before"])
        resp <- runService inner req
        modifyIORef' ref (++ ["after"])
        pure resp

  let svc' = svc |> logging
  result <- runService svc' "test"
  log <- readIORef ref

  assert "middleware wraps service" (result == "resp:test")
  assert "middleware runs before/after" (log == ["before", "handler:test", "after"])


-- ===================================================================
-- 6. before combinator
-- ===================================================================

testBefore :: IO ()
testBefore = do
  ref <- newIORef ([] :: [String])
  let svc :: Service IO String String
      svc = Service $ \req -> do
        modifyIORef' ref (++ ["handler"])
        pure req

      logBefore :: Middleware IO String String
      logBefore = before $ \req -> modifyIORef' ref (++ ["before:" ++ req])

  let svc' = svc |> logBefore
  _ <- runService svc' "x"
  log <- readIORef ref
  assert "before runs before handler" (log == ["before:x", "handler"])


-- ===================================================================
-- 7. after combinator
-- ===================================================================

testAfter :: IO ()
testAfter = do
  ref <- newIORef ([] :: [String])
  let svc :: Service IO String String
      svc = Service $ \req -> do
        modifyIORef' ref (++ ["handler"])
        pure ("resp:" ++ req)

      logAfter :: Middleware IO String String
      logAfter = after $ \_ resp -> modifyIORef' ref (++ ["after:" ++ resp])

  let svc' = svc |> logAfter
  _ <- runService svc' "x"
  log <- readIORef ref
  assert "after runs after handler" (log == ["handler", "after:resp:x"])


-- ===================================================================
-- 8. around combinator
-- ===================================================================

testAround :: IO ()
testAround = do
  ref <- newIORef ([] :: [String])
  let svc :: Service IO String String
      svc = Service $ \req -> do
        modifyIORef' ref (++ ["handler:" ++ req])
        pure req

      wrap :: Middleware IO String String
      wrap = around $ \req callInner -> do
        modifyIORef' ref (++ ["around:before"])
        resp <- callInner (req ++ "!")
        modifyIORef' ref (++ ["around:after"])
        pure (resp ++ "!!")

  let svc' = svc |> wrap
  result <- runService svc' "hi"
  log <- readIORef ref

  assert "around wraps call" (result == "hi!!!")
  assert "around sees modified request" (log == ["around:before", "handler:hi!", "around:after"])


-- ===================================================================
-- 9. Layer composition with |>
-- ===================================================================

testPipeComposition :: IO ()
testPipeComposition = do
  ref <- newIORef ([] :: [String])
  let svc :: Service IO String String
      svc = Service $ \req -> do
        modifyIORef' ref (++ ["handler"])
        pure req

      layer1 :: Middleware IO String String
      layer1 = before $ \_ -> modifyIORef' ref (++ ["L1"])

      layer2 :: Middleware IO String String
      layer2 = before $ \_ -> modifyIORef' ref (++ ["L2"])

  -- svc |> layer1 |> layer2
  -- layer2 wraps (layer1 wraps svc)
  -- Request flow: layer2 -> layer1 -> handler
  let svc' = svc |> layer1 |> layer2
  _ <- runService svc' "x"
  log <- readIORef ref
  assert "|> applies layers inside-out" (log == ["L2", "L1", "handler"])


-- ===================================================================
-- 10. composeLayer
-- ===================================================================

testComposeLayer :: IO ()
testComposeLayer = do
  ref <- newIORef ([] :: [String])
  let svc :: Service IO String String
      svc = Service $ \req -> do
        modifyIORef' ref (++ ["handler"])
        pure req

      layer1 :: Middleware IO String String
      layer1 = before $ \_ -> modifyIORef' ref (++ ["L1"])

      layer2 :: Middleware IO String String
      layer2 = before $ \_ -> modifyIORef' ref (++ ["L2"])

      -- composed applies layer1 first (inner), then layer2 (outer)
      composed = composeLayer layer2 layer1

  let svc' = svc |> composed
  _ <- runService svc' "x"
  log <- readIORef ref
  assert "composeLayer: outer wraps inner" (log == ["L2", "L1", "handler"])


-- ===================================================================
-- 11. identity layer
-- ===================================================================

testIdentity :: IO ()
testIdentity = do
  let svc :: Service IO Int Int
      svc = Service $ \n -> pure (n + 1)
      svc' = svc |> identity
  result <- runService svc' 5
  assert "identity layer is no-op" (result == 6)


-- ===================================================================
-- 12. <| operator (right-to-left)
-- ===================================================================

testRightToLeft :: IO ()
testRightToLeft = do
  ref <- newIORef ([] :: [String])
  let svc :: Service IO String String
      svc = Service $ \req -> do
        modifyIORef' ref (++ ["handler"])
        pure req

      layer1 :: Middleware IO String String
      layer1 = before $ \_ -> modifyIORef' ref (++ ["L1"])

  let svc' = layer1 <| svc
  _ <- runService svc' "x"
  log <- readIORef ref
  assert "<| applies layer" (log == ["L1", "handler"])


-- ===================================================================
-- Main
-- ===================================================================

main :: IO ()
main = do
  putStrLn "Spire tests:"
  putStrLn ""
  putStrLn "Service basics:"
  testServiceBasic
  putStrLn ""
  putStrLn "Transformers:"
  testMapResponse
  testMapRequest
  testDimap
  putStrLn ""
  putStrLn "Middleware:"
  testMiddleware
  testBefore
  testAfter
  testAround
  putStrLn ""
  putStrLn "Composition:"
  testPipeComposition
  testComposeLayer
  testIdentity
  testRightToLeft
  putStrLn ""
  putStrLn "All spire tests passed."
