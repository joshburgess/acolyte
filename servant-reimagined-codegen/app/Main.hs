{-# LANGUAGE OverloadedStrings #-}
-- | CLI for servant-reimagined-codegen.
--
-- Usage:
--   servant-reimagined-codegen input.json                    # print to stdout
--   servant-reimagined-codegen input.json -o Generated/API.hs  # write to file
--   servant-reimagined-codegen input.json -m MyApp.API        # custom module name
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Servant.Reimagined.Codegen


main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> do
      putStrLn "servant-reimagined-codegen — generate API types from OpenAPI/Swagger specs"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  servant-reimagined-codegen <spec.json> [-o output.hs] [-m Module.Name]"
      putStrLn ""
      putStrLn "Supports Swagger 2.0 and OpenAPI 3.x (auto-detected)."
      exitFailure
    Just (inputFile, mOutput, moduleName) -> do
      result <- readSpecFile inputFile
      case result of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          exitFailure
        Right api -> do
          let cfg = defaultEmitConfig { emitModuleName = moduleName }
              code = emitModule cfg api
          case mOutput of
            Nothing   -> TIO.putStr code
            Just path -> do
              TIO.writeFile path code
              putStrLn $ "Generated: " ++ path


parseArgs :: [String] -> Maybe (FilePath, Maybe FilePath, T.Text)
parseArgs [] = Nothing
parseArgs (input:rest) = Just (input, findFlag "-o" rest, findFlagT "-m" rest)
  where
    findFlag _ [] = Nothing
    findFlag flag (f:val:_) | f == flag = Just val
    findFlag flag (_:xs) = findFlag flag xs

    findFlagT flag xs = case findFlag flag xs of
      Just v  -> T.pack v
      Nothing -> "Generated.API"
