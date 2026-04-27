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

-- The full RealWorld (Conduit) API: all 19 endpoints in a single
-- type-level list. Response shapes use envelope newtypes
-- ('ArticleResponse', 'CommentResponse', 'CommentsResponse') so they
-- match the spec exactly: e.g. @{"article": {...}}@ for single-item
-- responses, @{"comments": [...]}@ for the comment list.

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
   , Get ArticlePath (Json ArticleResponse)                       -- GET  /api/articles/:slug
   , Requires Auth (Post ArticlesPath (Json CreateArticleRequest) (Json ArticleResponse))  -- POST /api/articles
   , Requires Auth (Put ArticlePath (Json UpdateArticleRequest) (Json ArticleResponse))    -- PUT  /api/articles/:slug
   , Requires Auth (DeleteNoContent ArticlePath)                  -- DELETE /api/articles/:slug (204)
   , Requires Auth (Post FavoritePath (Json ()) (Json ArticleResponse))  -- POST /api/articles/:slug/favorite
   , Requires Auth (Delete FavoritePath (Json ArticleResponse))   -- DELETE /api/articles/:slug/favorite

     -- Comments
   , Get CommentsPath (Json CommentsResponse)                     -- GET  /api/articles/:slug/comments
   , Requires Auth (Post CommentsPath (Json CreateCommentRequest) (Json CommentResponse))  -- POST /api/articles/:slug/comments
   , Requires Auth (DeleteNoContent CommentPath)                  -- DELETE /api/articles/:slug/comments/:id (204)

     -- Tags
   , Get TagsPath (Json TagsResponse)                             -- GET  /api/tags
   ]
