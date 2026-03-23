{-# LANGUAGE OverloadedStrings #-}
-- | CLI for servant-reimagined-codegen.
--
-- Usage:
--   servant-reimagined-codegen generate spec.json [-o output.hs] [-m Module.Name]
--   servant-reimagined-codegen scaffold spec.json --name my-app [--dir .]
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Servant.Reimagined.Codegen
import Servant.Reimagined.Codegen.Scaffold
import Servant.Reimagined.Codegen.Proto (parseProtoFile, protoToIR)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("scaffold" : rest) -> runScaffold rest
    ("generate" : rest) -> runGenerate rest
    (f:rest) | not ("-" `isPrefixOf` f) -> runGenerate (f:rest)  -- default to generate
    _ -> usage


usage :: IO ()
usage = do
  putStrLn "servant-reimagined-codegen — generate API types from OpenAPI/Swagger specs"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  generate <spec> [-o output.hs] [-m Module.Name]"
  putStrLn "    Generate a Haskell module from an OpenAPI/Swagger spec."
  putStrLn ""
  putStrLn "  generate --proto <file.proto> [-o output.hs] [-m Module.Name]"
  putStrLn "    Generate a Haskell module from a proto3 service definition."
  putStrLn ""
  putStrLn "  scaffold <spec> --name <project-name> [--dir <output-dir>]"
  putStrLn "    Generate a complete cabal project from a spec."
  putStrLn ""
  putStrLn "Supports JSON, YAML, and proto3. Swagger 2.0 and OpenAPI 3.x auto-detected."
  exitFailure


runGenerate :: [String] -> IO ()
runGenerate args = case args of
  [] -> usage
  ("--proto":protoFile:rest) -> do
    result <- parseProtoFile protoFile
    case result of
      Left err -> putStrLn ("Error: " ++ show err) >> exitFailure
      Right proto -> do
        let api = protoToIR proto
            moduleName = findFlagT "-m" rest
            cfg = defaultEmitConfig { emitModuleName = moduleName }
            code = emitModule cfg api
        case findFlag "-o" rest of
          Nothing   -> TIO.putStr code
          Just path -> TIO.writeFile path code >> putStrLn ("Generated: " ++ path)
  (inputFile:rest) -> do
    result <- readSpecFile inputFile
    case result of
      Left err -> putStrLn ("Error: " ++ show err) >> exitFailure
      Right api -> do
        let moduleName = findFlagT "-m" rest
            cfg = defaultEmitConfig { emitModuleName = moduleName }
            code = emitModule cfg api
        case findFlag "-o" rest of
          Nothing   -> TIO.putStr code
          Just path -> TIO.writeFile path code >> putStrLn ("Generated: " ++ path)


runScaffold :: [String] -> IO ()
runScaffold args = case args of
  [] -> usage
  (inputFile:rest) -> do
    result <- readSpecFile inputFile
    case result of
      Left err -> putStrLn ("Error: " ++ show err) >> exitFailure
      Right api -> do
        let name = findFlagT "--name" rest
            dir  = maybe "." id (findFlag "--dir" rest)
            cfg  = ScaffoldConfig
              { scaffoldName = name
              , scaffoldDir  = dir
              }
        scaffoldProject cfg api


-- ===================================================================
-- Arg parsing helpers
-- ===================================================================

findFlag :: String -> [String] -> Maybe String
findFlag _ [] = Nothing
findFlag flag (f:val:_) | f == flag = Just val
findFlag flag (_:xs) = findFlag flag xs

findFlagT :: String -> [String] -> T.Text
findFlagT flag xs = case findFlag flag xs of
  Just v  -> T.pack v
  Nothing -> "Generated.API"

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (a:as) (b:bs) = a == b && isPrefixOf as bs
