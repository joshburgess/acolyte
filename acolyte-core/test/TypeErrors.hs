-- | Compile-time error tests for Named endpoint validation.
--
-- Each test case is a separate .hs file in typeerror-cases/ that should
-- fail to compile. This runner invokes GHC (via cabal exec) on each and
-- verifies the compilation fails with the expected error message.
module Main (main) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Directory (getCurrentDirectory)


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  OK: " ++ label
assert label False = error   $ "FAIL: " ++ label


shouldFailToCompile :: FilePath -> String -> IO Bool
shouldFailToCompile file expectedFragment = do
  cwd <- getCurrentDirectory
  let casePath = cwd ++ "/test/typeerror-cases/" ++ file

  (exitCode, _stdout, stderr) <- readProcessWithExitCode "cabal"
    [ "exec", "--", "ghc"
    , "-fno-code"
    , "-no-keep-hi-files"
    , "-package", "acolyte-core"
    , casePath
    ] ""
  case exitCode of
    ExitFailure _ ->
      if expectedFragment `isInfixOf'` stderr
        then pure True
        else do
          putStrLn $ "    ERROR (wrong message): " ++ take 500 stderr
          putStrLn $ "    EXPECTED: " ++ expectedFragment
          pure False
    ExitSuccess -> do
      putStrLn $ "    UNEXPECTED: " ++ file ++ " compiled successfully"
      pure False


isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf' needle) (tails' haystack)

isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _          = True
isPrefixOf' _ []          = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs@(_:xs') = xs : tails' xs'


main :: IO ()
main = do
  putStrLn "Named endpoint compile-time error tests (core):"
  putStrLn ""

  putStrLn "AllNamed (negative):"
  shouldFailToCompile "AllNamedMixed.hs" "Expected a Named endpoint"
    >>= assert "AllNamed rejects mixed API (some unnamed)"
  shouldFailToCompile "AllNamedBare.hs" "Expected a Named endpoint"
    >>= assert "AllNamed rejects fully bare API"
  putStrLn ""

  putStrLn "NoDuplicateNames (negative):"
  shouldFailToCompile "DuplicateNames.hs" "Duplicate endpoint name"
    >>= assert "NoDuplicateNames rejects duplicate names"
  shouldFailToCompile "DuplicateNamesAdjacent.hs" "Duplicate endpoint name"
    >>= assert "NoDuplicateNames rejects adjacent duplicates"
  putStrLn ""

  putStrLn "Named arity mismatch (negative):"
  shouldFailToCompile "NamedArityMismatch.hs" "endpoint(s) but 3 handler(s)"
    >>= assert "Serves rejects wrong arity with Named endpoints"
  putStrLn ""

  putStrLn "LookupNamed (negative):"
  shouldFailToCompile "LookupNamedMissing.hs" "not found in API"
    >>= assert "LookupNamed rejects missing endpoint name"
  putStrLn ""

  putStrLn "All Named compile-time error tests (core) passed."
