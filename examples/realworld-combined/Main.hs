{-# LANGUAGE OverloadedStrings #-}
-- | RealWorld backend using sub-API composition.
--
-- Same functionality as the realworld example, but the API is split
-- into 6 logical sub-APIs. Each sub-API is wired independently with
-- O(1) compile time, and effects are checked across the combined type.
--
-- Run:  cabal run realworld-combined
-- Test: same curl commands as the realworld example
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)

import Servant.Reimagined.Prelude

import API
import Types
import Store
import Handlers


-- ===================================================================
-- Wire handlers per sub-API, then combine
-- ===================================================================

buildService :: Store -> Service IO (Request ByteString) (Response ByteString)
buildService store =
  -- Effect tracking across the FULL combined API.
  -- If Auth is required by any sub-API and we forget provideEffect,
  -- this won't compile.
  runCombined @FullAPI
  $ provideEffect @Auth authMw
  $ combinedFromRouter @FullAPI
  -- Merge all sub-API routers. Each subRouter call is O(1).
  $ subRouter @TagsAPI
      ( wrapHandler @(Get TagsPath (Json TagsResponse))
          (toHandler (tagsHandler store))
      )
  $ subRouter @CommentsAPI
      ( wrapHandler @(Get CommentsPath (Json [Comment]))
          (toHandler (getCommentsHandler store))
      , wrapHandler @(Requires Auth (Post CommentsPath (Json CreateCommentRequest) (Json Comment)))
          (toHandler (createCommentHandler store))
      )
  $ subRouter @ArticlesAPI
      ( wrapHandler @(WithParams '[QP "tag" Text, QP "author" Text, QP "limit" Int, QP "offset" Int]
                        (Get ArticlesPath (Json ArticlesResponse)))
          (toHandler (listArticlesHandler store))
      , wrapHandler @(Requires Auth (Get ArticleFeedPath (Json ArticlesResponse)))
          (toHandler (feedHandler store))
      , wrapHandler @(Get ArticlePath (Json Article))
          (toHandler (getArticleHandler store))
      , wrapHandler @(Requires Auth (Post ArticlesPath (Json CreateArticleRequest) (Json Article)))
          (toHandler (createArticleHandler store))
      , wrapHandler @(Requires Auth (DeleteNoContent ArticlePath))
          (toHandler (deleteArticleHandler store))
      )
  $ subRouter @ProfilesAPI
      ( wrapHandler @(Get ProfilePath (Json Profile))
          (toHandler (getProfileHandler store))
      , wrapHandler @(Requires Auth (Post FollowPath (Json ()) (Json Profile)))
          (toHandler (followHandler store))
      , wrapHandler @(Requires Auth (Delete FollowPath (Json Profile)))
          (toHandler (unfollowHandler store))
      )
  $ subRouter @UserAPI
      ( wrapHandler @(Requires Auth (Get UserPath (Json User)))
          (toHandler (getCurrentUserHandler store))
      , wrapHandler @(Requires Auth (Put UserPath (Json UpdateUserRequest) (Json User)))
          (toHandler (updateUserHandler store))
      )
  $ subRouter @AuthAPI
      ( wrapHandler @(Describe "Log in an existing user"
                        (Post LoginPath (Json LoginRequest) (Json User)))
          (toHandler (loginHandler store))
      , wrapHandler @(Describe "Register a new user"
                        (PostCreated RegisterPath (Json RegisterRequest) (Json User)))
          (toHandler (registerHandler store))
      )
  $ emptyRouter
  where
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

  putStrLn "RealWorld backend (sub-API composition) on http://localhost:3000"
  putStrLn ""
  putStrLn "Sub-APIs:"
  putStrLn "  AuthAPI     — 2 endpoints (POST /api/users/login, POST /api/users)"
  putStrLn "  UserAPI     — 2 endpoints (GET/PUT /api/user) [auth required]"
  putStrLn "  ProfilesAPI — 3 endpoints (GET/POST/DELETE /api/profiles/:username)"
  putStrLn "  ArticlesAPI — 5 endpoints (articles CRUD + feed) [mixed auth]"
  putStrLn "  CommentsAPI — 2 endpoints (GET/POST /api/articles/:slug/comments)"
  putStrLn "  TagsAPI     — 1 endpoint  (GET /api/tags)"
  putStrLn ""
  putStrLn "Total: 15 endpoints across 6 sub-APIs, combined with O(1) compile time."
  putStrLn "Effects (Auth) checked across FullAPI = all 6 sub-APIs."
  putStrLn ""

  runServerBS 3000 svc
