{-# LANGUAGE OverloadedStrings #-}
-- | RealWorld API type definition.
--
-- The entire API is described as a single Haskell type. This drives
-- server routing, handler wiring, and compile-time checks.
module API where

import Data.Text (Text)
import Servant.Reimagined.Core
import Servant.Reimagined.Server (Json)
import Types


-- ===================================================================
-- Path types (using GHC.TypeLits — no TH needed)
-- ===================================================================

-- Auth
type LoginPath    = '[ 'Lit "api", 'Lit "users", 'Lit "login" ]
type RegisterPath = '[ 'Lit "api", 'Lit "users" ]

-- User
type UserPath     = '[ 'Lit "api", 'Lit "user" ]

-- Profiles
type ProfilePath  = '[ 'Lit "api", 'Lit "profiles", 'Capture Text ]
type FollowPath   = '[ 'Lit "api", 'Lit "profiles", 'Capture Text, 'Lit "follow" ]

-- Articles
type ArticlesPath    = '[ 'Lit "api", 'Lit "articles" ]
type ArticleFeedPath = '[ 'Lit "api", 'Lit "articles", 'Lit "feed" ]
type ArticlePath     = '[ 'Lit "api", 'Lit "articles", 'Capture Text ]
type FavoritePath    = '[ 'Lit "api", 'Lit "articles", 'Capture Text, 'Lit "favorite" ]

-- Comments
type CommentsPath = '[ 'Lit "api", 'Lit "articles", 'Capture Text, 'Lit "comments" ]
type CommentPath  = '[ 'Lit "api", 'Lit "articles", 'Capture Text, 'Lit "comments", 'Capture Int ]

-- Tags
type TagsPath = '[ 'Lit "api", 'Lit "tags" ]


-- ===================================================================
-- API type: the single source of truth
-- ===================================================================

-- We split into sub-APIs to stay within the 16-endpoint tuple limit,
-- then would nest them. For this example, we'll define the core
-- endpoints that demonstrate the framework's features.

type RealWorldAPI =
  '[ -- Auth (public)
     Post LoginPath    (Json LoginRequest)    (Json User)      -- POST /api/users/login
   , Post RegisterPath (Json RegisterRequest) (Json User)      -- POST /api/users

     -- User (auth required)
   , Requires Auth (Get UserPath (Json User))                   -- GET  /api/user
   , Requires Auth (Put UserPath (Json UpdateUserRequest) (Json User))  -- PUT  /api/user

     -- Profiles
   , Get ProfilePath (Json Profile)                             -- GET  /api/profiles/:username
   , Requires Auth (Post FollowPath (Json ()) (Json Profile))   -- POST /api/profiles/:username/follow
   , Requires Auth (Delete FollowPath (Json Profile))           -- DELETE /api/profiles/:username/follow

     -- Articles
   , Get ArticlesPath (Json ArticlesResponse)                   -- GET  /api/articles
   , Requires Auth (Get ArticleFeedPath (Json ArticlesResponse)) -- GET  /api/articles/feed
   , Get ArticlePath (Json Article)                             -- GET  /api/articles/:slug
   , Requires Auth (Post ArticlesPath (Json CreateArticleRequest) (Json Article))  -- POST /api/articles
   , Requires Auth (Delete ArticlePath (Json Article))          -- DELETE /api/articles/:slug

     -- Comments
   , Get CommentsPath (Json [Comment])                          -- GET  /api/articles/:slug/comments
   , Requires Auth (Post CommentsPath (Json CreateCommentRequest) (Json Comment))  -- POST /api/articles/:slug/comments

     -- Tags
   , Get TagsPath (Json TagsResponse)                           -- GET  /api/tags
   ]
