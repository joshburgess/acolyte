{-# LANGUAGE OverloadedStrings #-}
-- | RealWorld API split into logical sub-APIs.
--
-- Each sub-API is a separate type-level list, small enough for flat
-- tuple instances (O(1) compile time). The combined FullAPI type
-- drives OpenAPI spec and client generation.
module API where

import Data.Text (Text)
import Servant.Reimagined.Core
import Servant.Reimagined.Server (Json)
import Types


-- ===================================================================
-- Path types
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

-- Comments
type CommentsPath = '[ 'Lit "api", 'Lit "articles", 'Capture Text, 'Lit "comments" ]

-- Tags
type TagsPath = '[ 'Lit "api", 'Lit "tags" ]


-- ===================================================================
-- Sub-APIs: each is a small, focused group
-- ===================================================================

-- | Authentication endpoints (public, no auth required).
type AuthAPI =
  '[ Post LoginPath    (Json LoginRequest)    (Json User)
   , Post RegisterPath (Json RegisterRequest) (Json User)
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
  '[ Get ArticlesPath (Json ArticlesResponse)
   , Requires Auth (Get ArticleFeedPath (Json ArticlesResponse))
   , Get ArticlePath (Json Article)
   , Requires Auth (Post ArticlesPath (Json CreateArticleRequest) (Json Article))
   , Requires Auth (Delete ArticlePath (Json Article))
   ]

-- | Comment endpoints (mixed auth).
type CommentsAPI =
  '[ Get CommentsPath (Json [Comment])
   , Requires Auth (Post CommentsPath (Json CreateCommentRequest) (Json Comment))
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
