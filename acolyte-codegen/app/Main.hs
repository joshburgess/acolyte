{-# LANGUAGE OverloadedStrings #-}
-- | CLI for acolyte-codegen.
--
-- Usage:
--   acolyte-codegen generate spec.json [-o output.hs] [-m Module.Name]
--   acolyte-codegen scaffold spec.json --name my-app [--dir .]
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Acolyte.Codegen
import Acolyte.Codegen.Scaffold
import Acolyte.Codegen.Proto (parseProtoFile, protoToIR)
import Acolyte.Codegen.ProtoDiff (diffProtos, ProtoDiff (..), DiffSeverity (..))


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("scaffold" : rest) -> runScaffold rest
    ("generate" : rest) -> runGenerate rest
    ("diff" : rest)     -> runDiff rest
    (f:rest) | not ("-" `isPrefixOf` f) -> runGenerate (f:rest)  -- default to generate
    _ -> usage


usage :: IO ()
usage = do
  putStrLn "acolyte-codegen — generate API types from OpenAPI/Swagger specs"
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
  putStrLn "  diff <old.proto> <new.proto>"
  putStrLn "    Compare two proto files and report breaking/non-breaking changes."
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


runDiff :: [String] -> IO ()
runDiff (oldFile:newFile:_) = do
  oldResult <- parseProtoFile oldFile
  newResult <- parseProtoFile newFile
  case (oldResult, newResult) of
    (Left err, _) -> putStrLn ("Error parsing old file: " ++ show err) >> exitFailure
    (_, Left err) -> putStrLn ("Error parsing new file: " ++ show err) >> exitFailure
    (Right oldProto, Right newProto) -> do
      let diffs = diffProtos oldProto newProto
      if null diffs
        then putStrLn "No differences found."
        else mapM_ printDiff diffs
  where
    printDiff (ProtoDiff sev desc) =
      TIO.putStrLn $ severityTag sev <> " " <> desc
    severityTag Breaking    = "[BREAKING]"
    severityTag NonBreaking  = "[non-breaking]"
runDiff _ = usage


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
