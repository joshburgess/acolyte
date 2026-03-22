{-# LANGUAGE OverloadedStrings #-}
-- | RealWorld (Conduit) backend — servant-reimagined edition.
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

import Tower
import Tower.Service (Service (..))
import Tower.Http
import Tower.Server (runServerBS)
import Http.Core

import Servant.Reimagined.Core
import Servant.Reimagined.Server

import API
import Types
import Store
import Handlers


-- ===================================================================
-- Wire handlers to the API type (compile-time checked)
-- ===================================================================

buildService :: Store -> Service IO (Request ByteString) (Response ByteString)
buildService store = run
  $ provide @Auth authMw
  $ effectfulServer @RealWorldAPI
    ( wrapHandler @(Post LoginPath (Json LoginRequest) (Json User))
        (loginHandler store)
    , wrapHandler @(Post RegisterPath (Json RegisterRequest) (Json User))
        (registerHandler store)
    , wrapHandler @(Requires Auth (Get UserPath (Json User)))
        (getCurrentUserHandler store)
    , wrapHandler @(Requires Auth (Put UserPath (Json UpdateUserRequest) (Json User)))
        (updateUserHandler store)
    , wrapHandler @(Get ProfilePath (Json Profile))
        (getProfileHandler store)
    , wrapHandler @(Requires Auth (Post FollowPath (Json ()) (Json Profile)))
        (followHandler store)
    , wrapHandler @(Requires Auth (Delete FollowPath (Json Profile)))
        (unfollowHandler store)
    , wrapHandler @(Get ArticlesPath (Json ArticlesResponse))
        (listArticlesHandler store)
    , wrapHandler @(Requires Auth (Get ArticleFeedPath (Json ArticlesResponse)))
        (feedHandler store)
    , wrapHandler @(Get ArticlePath (Json Article))
        (getArticleHandler store)
    , wrapHandler @(Requires Auth (Post ArticlesPath (Json CreateArticleRequest) (Json Article)))
        (createArticleHandler store)
    , wrapHandler @(Requires Auth (Delete ArticlePath (Json Article)))
        (deleteArticleHandler store)
    , wrapHandler @(Get CommentsPath (Json [Comment]))
        (getCommentsHandler store)
    , wrapHandler @(Requires Auth (Post CommentsPath (Json CreateCommentRequest) (Json Comment)))
        (createCommentHandler store)
    , wrapHandler @(Get TagsPath (Json TagsResponse))
        (tagsHandler store)
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
  putStrLn "  # List articles"
  putStrLn "  curl -s http://localhost:3000/api/articles"
  putStrLn ""
  putStrLn "  # Tags"
  putStrLn "  curl -s http://localhost:3000/api/tags"
  putStrLn ""

  runServerBS 3000 svc
