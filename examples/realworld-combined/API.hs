{-# LANGUAGE OverloadedStrings #-}
-- | RealWorld API split into logical sub-APIs.
--
-- Each sub-API is a separate type-level list, small enough for flat
-- tuple instances (O(1) compile time). The combined FullAPI type
-- drives OpenAPI spec and client generation.
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
-- Sub-APIs: each is a small, focused group
-- ===================================================================

-- | Authentication endpoints (public, no auth required).
type AuthAPI =
  '[ Describe "Log in an existing user"
       (Post LoginPath (Json LoginRequest) (Json User))
   , Describe "Register a new user"
       (PostCreated RegisterPath (Json RegisterRequest) (Json User))
   ]

-- | Current user endpoints (auth required).
type UserAPI =
  '[ Requires Auth (Get UserPath (Json User))
   , Requires Auth (Put UserPath (Json UpdateUserRequest) (Json User))
   ]

-- | Profile endpoints (mixed auth).
type ProfilesAPI =
  '[ Get ProfilePath (Json Profile)
   , Requires Auth (Post FollowPath (Json ()) (Json Profile))
   , Requires Auth (Delete FollowPath (Json Profile))
   ]

-- | Article endpoints (mixed auth).
type ArticlesAPI =
  '[ WithParams '[QP "tag" Text, QP "author" Text, QP "limit" Int, QP "offset" Int]
       (Get ArticlesPath (Json ArticlesResponse))
   , Requires Auth (Get ArticleFeedPath (Json ArticlesResponse))
   , Get ArticlePath (Json ArticleResponse)
   , Requires Auth (Post ArticlesPath (Json CreateArticleRequest) (Json ArticleResponse))
   , Requires Auth (Put ArticlePath (Json UpdateArticleRequest) (Json ArticleResponse))
   , Requires Auth (DeleteNoContent ArticlePath)
   , Requires Auth (Post FavoritePath (Json ()) (Json ArticleResponse))
   , Requires Auth (Delete FavoritePath (Json ArticleResponse))
   ]

-- | Comment endpoints (mixed auth).
type CommentsAPI =
  '[ Get CommentsPath (Json CommentsResponse)
   , Requires Auth (Post CommentsPath (Json CreateCommentRequest) (Json CommentResponse))
   , Requires Auth (DeleteNoContent CommentPath)
   ]

-- | Tag endpoints (public).
type TagsAPI =
  '[ Get TagsPath (Json TagsResponse)
   ]


-- ===================================================================
-- Combined API: the single type for OpenAPI / client generation
-- ===================================================================

-- | The full API — all sub-APIs concatenated at the type level.
--
-- This type is used for:
-- * OpenAPI spec generation: @generateSpec \@FullAPI "RealWorld" "1.0"@
-- * Client generation: @callEndpoint \@(Get ArticlePath ...) client slug@
-- * Effect verification: @runCombined \@FullAPI@ checks all effects
type FullAPI = AuthAPI ++ UserAPI ++ ProfilesAPI ++ ArticlesAPI ++ CommentsAPI ++ TagsAPI
