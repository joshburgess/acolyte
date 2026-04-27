{-# LANGUAGE OverloadedStrings #-}
-- | RealWorld (Conduit) backend — acolyte edition.
--
-- Run:  cabal run realworld
-- Test: curl -s http://localhost:3000/api/tags
--       curl -s -X POST http://localhost:3000/api/users -H 'Content-Type: application/json' \
--            -d '{"user":{"username":"jake","email":"jake@jake.jake","password":"jakejake"}}'
--       curl -s http://localhost:3000/api/profiles/jake
--       curl -s -H 'Authorization: Token tok-jake' http://localhost:3000/api/user
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Acolyte.Prelude

import API
import Store
import Handlers


-- ===================================================================
-- Wire handlers to the API type (compile-time checked)
-- ===================================================================

buildService :: Store -> Service IO (Request ByteString) (Response ByteString)
buildService store = run
  $ provide @Auth authMw
  $ effectfulApi @RealWorldAPI
    ( loginHandler store
    , registerHandler store
    , getCurrentUserHandler store
    , updateUserHandler store
    , getProfileHandler store
    , followHandler store
    , unfollowHandler store
    , listArticlesHandler store
    , feedHandler store
    , getArticleHandler store
    , createArticleHandler store
    , updateArticleHandler store
    , deleteArticleHandler store
    , favoriteHandler store
    , unfavoriteHandler store
    , getCommentsHandler store
    , createCommentHandler store
    , deleteCommentHandler store
    , tagsHandler store
    )
  where
    -- Auth middleware: no-op (auth is checked in handlers via Authorization header)
    authMw :: Middleware IO (Request ByteString) (Response ByteString)
    authMw = before $ \_ -> pure ()


-- ===================================================================
-- Middleware stack + run
-- ===================================================================

main :: IO ()
main = do
  store <- newStore

  ridLayer <- requestIdLayer
  let traceFn entry = BS8.putStrLn $
        traceMethod entry <> " " <> tracePath entry
        <> " -> " <> BS8.pack (show (traceStatus entry))

  let svc = buildService store
        |> ridLayer
        |> traceLayer traceFn
        |> corsLayer permissiveCors
        |> secureHeadersLayer defaultSecureHeaders

  putStrLn "RealWorld backend on http://localhost:3000"
  putStrLn "Implements the full Conduit API: 19 endpoints, spec-compliant JSON envelopes."
  putStrLn ""
  putStrLn "Try:"
  putStrLn "  # Register"
  putStrLn "  curl -s -X POST http://localhost:3000/api/users \\"
  putStrLn "    -H 'Content-Type: application/json' \\"
  putStrLn "    -d '{\"user\":{\"username\":\"jake\",\"email\":\"jake@jake.jake\",\"password\":\"jakejake\"}}'"
  putStrLn ""
  putStrLn "  # Get profile"
  putStrLn "  curl -s http://localhost:3000/api/profiles/jake"
  putStrLn ""
  putStrLn "  # Get current user (auth)"
  putStrLn "  curl -s -H 'Authorization: Token tok-jake' http://localhost:3000/api/user"
  putStrLn ""
  putStrLn "  # Create article (auth)"
  putStrLn "  curl -s -X POST http://localhost:3000/api/articles \\"
  putStrLn "    -H 'Content-Type: application/json' \\"
  putStrLn "    -H 'Authorization: Token tok-jake' \\"
  putStrLn "    -d '{\"article\":{\"title\":\"How to train your dragon\",\"description\":\"Ever wonder?\",\"body\":\"Very carefully.\",\"tagList\":[\"dragons\",\"training\"]}}'"
  putStrLn ""
  putStrLn "  # Favorite an article (auth)"
  putStrLn "  curl -s -X POST -H 'Authorization: Token tok-jake' \\"
  putStrLn "    http://localhost:3000/api/articles/how-to-train-your-dragon/favorite"
  putStrLn ""
  putStrLn "  # List articles"
  putStrLn "  curl -s http://localhost:3000/api/articles"
  putStrLn ""
  putStrLn "  # Tags"
  putStrLn "  curl -s http://localhost:3000/api/tags"
  putStrLn ""

  runServerBS 3000 svc
