{-# LANGUAGE OverloadedStrings #-}
-- | RealWorld API type definition.
--
-- The entire API is described as a single Haskell type. This drives
-- server routing, handler wiring, and compile-time checks.
--
-- Path types use a shared prefix ('ApiV') and the 'At', 'At2',
-- 'Param' helpers with '++' for concise, composable definitions.
module API where

import Data.Text (Text)
import Acolyte.Core
import Acolyte.Server (Json)
import Types


-- ===================================================================
-- Path types — shared prefix + At/Param helpers
-- ===================================================================

-- | All paths share the @/api@ prefix.
type ApiV = At "api"

-- Auth
type LoginPath    = ApiV ++ At2 "users" "login"
type RegisterPath = ApiV ++ At "users"

-- User
type UserPath     = ApiV ++ At "user"

-- Profiles
type ProfilePath  = ApiV ++ Param "profiles" Text
type FollowPath   = ApiV ++ Param "profiles" Text ++ At "follow"

-- Articles
type ArticlesPath    = ApiV ++ At "articles"
type ArticleFeedPath = ApiV ++ At2 "articles" "feed"
type ArticlePath     = ApiV ++ Param "articles" Text
type FavoritePath    = ApiV ++ Param "articles" Text ++ At "favorite"

-- Comments
type CommentsPath = ApiV ++ Param "articles" Text ++ At "comments"
type CommentPath  = ApiV ++ Param "articles" Text ++ At "comments" ++ '[ 'Capture Int ]

-- Tags
type TagsPath = ApiV ++ At "tags"


-- ===================================================================
-- API type: the single source of truth
-- ===================================================================

-- We split into sub-APIs to stay within the 16-endpoint tuple limit,
-- then would nest them. For this example, we'll define the core
-- endpoints that demonstrate the framework's features.

type RealWorldAPI =
  '[ -- Auth (public)
     Describe "Log in an existing user"
       (Post LoginPath (Json LoginRequest) (Json User))           -- POST /api/users/login
   , Describe "Register a new user"
       (PostCreated RegisterPath (Json RegisterRequest) (Json User)) -- POST /api/users (201)

     -- User (auth required)
   , Requires Auth (Get UserPath (Json User))                     -- GET  /api/user
   , Requires Auth (Put UserPath (Json UpdateUserRequest) (Json User))  -- PUT  /api/user

     -- Profiles
   , Get ProfilePath (Json Profile)                               -- GET  /api/profiles/:username
   , Requires Auth (Post FollowPath (Json ()) (Json Profile))     -- POST /api/profiles/:username/follow
   , Requires Auth (Delete FollowPath (Json Profile))             -- DELETE /api/profiles/:username/follow

     -- Articles
   , Describe "List articles, optionally filtered"
       (WithParams '[QP "tag" Text, QP "author" Text, QP "limit" Int, QP "offset" Int]
         (Get ArticlesPath (Json ArticlesResponse)))              -- GET  /api/articles
   , Requires Auth (Get ArticleFeedPath (Json ArticlesResponse))  -- GET  /api/articles/feed
   , Get ArticlePath (Json Article)                               -- GET  /api/articles/:slug
   , Requires Auth (Post ArticlesPath (Json CreateArticleRequest) (Json Article))  -- POST /api/articles
   , Requires Auth (DeleteNoContent ArticlePath)                  -- DELETE /api/articles/:slug (204)

     -- Comments
   , Get CommentsPath (Json [Comment])                            -- GET  /api/articles/:slug/comments
   , Requires Auth (Post CommentsPath (Json CreateCommentRequest) (Json Comment))  -- POST /api/articles/:slug/comments

     -- Tags
   , Get TagsPath (Json TagsResponse)                             -- GET  /api/tags
   ]
