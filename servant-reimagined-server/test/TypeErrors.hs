-- | Compile-time error tests for mkRecordApi and BuildRecordApi.
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
    , "-package", "servant-reimagined-core"
    , "-package", "servant-reimagined-server"
    , "-package", "tower"
    , "-package", "http-core"
    , "-package", "bytestring"
    , "-package", "text"
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
  putStrLn "Named endpoint server compile-time error tests:"
  putStrLn ""

  putStrLn "mkRecordApi with non-Named endpoints (negative):"
  shouldFailToCompile "RecordApiBare.hs" "Expected a Named endpoint"
    >>= assert "mkRecordApi rejects API without Named wrappers"
  putStrLn ""

  putStrLn "mkRecordApi with duplicate names (negative):"
  shouldFailToCompile "RecordApiDupNames.hs" "Duplicate endpoint name"
    >>= assert "mkRecordApi rejects duplicate endpoint names"
  putStrLn ""

  putStrLn "mkRecordApi with missing record field (negative):"
  shouldFailToCompile "RecordApiMissingField.hs" "No instance for"
    >>= assert "mkRecordApi rejects record with wrong/missing fields"
  putStrLn ""

  putStrLn "All Named server compile-time error tests passed."
