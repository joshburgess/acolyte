-- | Project scaffolding: generate a complete project from an OpenAPI spec.
--
-- Creates a ready-to-build cabal project with:
-- * .cabal file with all dependencies
-- * Generated/API.hs from the spec
-- * Handlers.hs with stubs
-- * Main.hs with server setup and middleware
-- * README.md
module Servant.Reimagined.Codegen.Scaffold
  ( scaffoldProject
  , ScaffoldConfig (..)
  , defaultScaffoldConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Servant.Reimagined.Codegen.IR
import Servant.Reimagined.Codegen.Emit


-- | Configuration for project scaffolding (project name and output directory).
data ScaffoldConfig = ScaffoldConfig
  { scaffoldName    :: !Text    -- ^ Project/package name
  , scaffoldDir     :: !FilePath -- ^ Output directory
  } deriving (Show)

-- | Default scaffold configuration: project name @"my-api"@ in the current directory.
defaultScaffoldConfig :: ScaffoldConfig
defaultScaffoldConfig = ScaffoldConfig
  { scaffoldName = "my-api"
  , scaffoldDir  = "."
  }


-- | Generate a complete cabal project from an API IR and scaffold configuration.
scaffoldProject :: ScaffoldConfig -> ApiIR -> IO ()
scaffoldProject cfg api = do
  let name = scaffoldName cfg
      dir  = scaffoldDir cfg </> T.unpack name
      srcDir = dir </> "src"

  createDirectoryIfMissing True srcDir

  -- Generated API module
  let apiCode = emitModule (defaultEmitConfig { emitModuleName = "API" }) api
  TIO.writeFile (srcDir </> "API.hs") apiCode

  -- Handlers module
  TIO.writeFile (srcDir </> "Handlers.hs") (handlersModule api)

  -- Main module
  TIO.writeFile (dir </> "Main.hs") (mainModule name api)

  -- Cabal file
  TIO.writeFile (dir </> (T.unpack name ++ ".cabal")) (cabalFile name)

  -- README
  TIO.writeFile (dir </> "README.md") (readmeFile name api)

  putStrLn $ "Scaffolded project: " ++ dir
  putStrLn $ "  " ++ dir ++ "/" ++ T.unpack name ++ ".cabal"
  putStrLn $ "  " ++ dir ++ "/Main.hs"
  putStrLn $ "  " ++ srcDir ++ "/API.hs"
  putStrLn $ "  " ++ srcDir ++ "/Handlers.hs"
  putStrLn $ "  " ++ dir ++ "/README.md"
  putStrLn ""
  putStrLn $ "Next: cd " ++ dir ++ " && cabal build"


-- ===================================================================
-- Generated files
-- ===================================================================

handlersModule :: ApiIR -> Text
handlersModule api = T.unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , "-- | Handler implementations."
  , "--"
  , "-- Generated stubs — implement the TODO functions."
  , "module Handlers where"
  , ""
  , "import Servant.Reimagined.Server (HandlerFn, intoResponse, mkError)"
  , "import Network.HTTP.Types (status501)"
  , "import Data.Text (Text)"
  , ""
  , T.unlines (map handlerImpl (apiEndpoints api))
  ]

handlerImpl :: EndpointIR -> Text
handlerImpl ep =
  let name = stubNameIR ep
  in T.unlines
    [ "-- | " <> T.pack (show (epMethod ep)) <> " " <> reconstructPathIR (epPath ep)
    , name <> " :: HandlerFn"
    , name <> " _parts _body = pure $ intoResponse (mkError status501 \"not implemented\")"
    ]

stubNameIR :: EndpointIR -> Text
stubNameIR ep = case epOperationId ep of
  Just opId -> camelCaseT opId
  Nothing -> T.pack $ camelCaseS $
    map toLowerC (show (epMethod ep)) ++ concatMap segWordIR (epPath ep)

segWordIR :: PathSegmentIR -> String
segWordIR (LitSegment s) = pascalS (T.unpack s)
segWordIR (CaptureSegment n _) = "By" ++ pascalS (T.unpack n)

reconstructPathIR :: [PathSegmentIR] -> Text
reconstructPathIR segs = "/" <> T.intercalate "/" (map segT segs)
  where
    segT (LitSegment s) = s
    segT (CaptureSegment n _) = "{" <> n <> "}"

camelCaseT :: Text -> Text
camelCaseT = T.pack . camelCaseS . T.unpack

camelCaseS :: String -> String
camelCaseS [] = []
camelCaseS (c:cs) = toLowerC c : cs

pascalS :: String -> String
pascalS [] = []
pascalS (c:cs) = toUpperC c : cs

toLowerC :: Char -> Char
toLowerC c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
           | otherwise = c

toUpperC :: Char -> Char
toUpperC c | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
           | otherwise = c


mainModule :: Text -> ApiIR -> Text
mainModule name api = T.unlines
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE TypeOperators #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE AllowAmbiguousTypes #-}"
  , "module Main (main) where"
  , ""
  , "import Tower"
  , "import Tower.Http"
  , "import Tower.Server (runServerBS)"
  , "import Servant.Reimagined.Core"
  , "import Servant.Reimagined.Server"
  , ""
  , "import API"
  , "import Handlers"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  putStrLn \"" <> name <> " running on http://localhost:3000\""
  , "  ridLayer <- requestIdLayer"
  , "  let svc = undefined -- TODO: wire handlers to API using mkServer or subRouter"
  , "      app = svc"
  , "            |> ridLayer"
  , "            |> corsLayer permissiveCors"
  , "            |> secureHeadersLayer defaultSecureHeaders"
  , "  runServerBS 3000 app"
  ]


cabalFile :: Text -> Text
cabalFile name = T.unlines
  [ "cabal-version:   3.0"
  , "name:            " <> name
  , "version:         0.1.0.0"
  , "build-type:      Simple"
  , ""
  , "executable " <> name
  , "  main-is: Main.hs"
  , "  other-modules: API, Handlers"
  , "  hs-source-dirs: ., src"
  , "  default-language: GHC2024"
  , "  default-extensions:"
  , "    DataKinds"
  , "    TypeFamilies"
  , "    TypeOperators"
  , "    OverloadedStrings"
  , "    AllowAmbiguousTypes"
  , "    ScopedTypeVariables"
  , "    DeriveGeneric"
  , "    DeriveAnyClass"
  , ""
  , "  build-depends:"
  , "      base                       >= 4.20 && < 5"
  , "    , servant-reimagined-core"
  , "    , servant-reimagined-server"
  , "    , tower"
  , "    , tower-http"
  , "    , tower-server"
  , "    , http-core"
  , "    , aeson                      >= 2.1  && < 2.3"
  , "    , bytestring                 >= 0.11 && < 0.13"
  , "    , text                       >= 2.0  && < 2.2"
  , "    , http-types                 >= 0.12 && < 0.13"
  ]


readmeFile :: Text -> ApiIR -> Text
readmeFile name api = T.unlines
  [ "# " <> name
  , ""
  , "Generated from: " <> apiTitle api <> " v" <> apiVersion api
  , ""
  , "## Building"
  , ""
  , "```sh"
  , "cabal build"
  , "cabal run " <> name
  , "```"
  , ""
  , "## Endpoints"
  , ""
  , T.unlines (map epDoc (apiEndpoints api))
  ]

epDoc :: EndpointIR -> Text
epDoc ep = "- `" <> T.pack (show (epMethod ep)) <> " " <> reconstructPathIR (epPath ep) <> "`"
  <> maybe "" (\s -> " — " <> s) (epSummary ep)
