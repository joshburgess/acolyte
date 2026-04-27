{-# LANGUAGE OverloadedStrings #-}
-- | Domain types for the RealWorld (Conduit) app.
-- JSON shapes match the RealWorld spec.
module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


-- ===================================================================
-- User
-- ===================================================================

data User = User
  { userEmail    :: !Text
  , userToken    :: !Text
  , userUsername :: !Text
  , userBio      :: !Text
  , userImage    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON User where
  toJSON u = object
    [ "user" .= object
      [ "email"    .= userEmail u
      , "token"    .= userToken u
      , "username" .= userUsername u
      , "bio"      .= userBio u
      , "image"    .= userImage u
      ]
    ]

data LoginRequest = LoginRequest
  { loginEmail    :: !Text
  , loginPassword :: !Text
  } deriving (Show, Generic)

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> do
    u <- o .: "user"
    LoginRequest <$> u .: "email" <*> u .: "password"

data RegisterRequest = RegisterRequest
  { regUsername :: !Text
  , regEmail    :: !Text
  , regPassword :: !Text
  } deriving (Show, Generic)

instance FromJSON RegisterRequest where
  parseJSON = withObject "RegisterRequest" $ \o -> do
    u <- o .: "user"
    RegisterRequest <$> u .: "username" <*> u .: "email" <*> u .: "password"

data UpdateUserRequest = UpdateUserRequest
  { updateEmail    :: !(Maybe Text)
  , updateUsername :: !(Maybe Text)
  , updateBio      :: !(Maybe Text)
  , updateImage    :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON UpdateUserRequest where
  parseJSON = withObject "UpdateUserRequest" $ \o -> do
    u <- o .: "user"
    UpdateUserRequest
      <$> u .:? "email"
      <*> u .:? "username"
      <*> u .:? "bio"
      <*> u .:? "image"


-- ===================================================================
-- Profile
-- ===================================================================

data Profile = Profile
  { profileUsername  :: !Text
  , profileBio       :: !Text
  , profileImage     :: !(Maybe Text)
  , profileFollowing :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON Profile where
  toJSON p = object
    [ "profile" .= object
      [ "username"  .= profileUsername p
      , "bio"       .= profileBio p
      , "image"     .= profileImage p
      , "following" .= profileFollowing p
      ]
    ]


-- ===================================================================
-- Article
-- ===================================================================

data Article = Article
  { articleSlug           :: !Text
  , articleTitle          :: !Text
  , articleDescription   :: !Text
  , articleBody          :: !Text
  , articleTagList       :: ![Text]
  , articleCreatedAt     :: !UTCTime
  , articleUpdatedAt     :: !UTCTime
  , articleFavorited     :: !Bool
  , articleFavoritesCount :: !Int
  , articleAuthor        :: !Profile
  } deriving (Show, Generic)

instance ToJSON Article where
  toJSON a = object
    [ "slug"           .= articleSlug a
    , "title"          .= articleTitle a
    , "description"    .= articleDescription a
    , "body"           .= articleBody a
    , "tagList"        .= articleTagList a
    , "createdAt"      .= articleCreatedAt a
    , "updatedAt"      .= articleUpdatedAt a
    , "favorited"      .= articleFavorited a
    , "favoritesCount" .= articleFavoritesCount a
    , "author"         .= authorJson (articleAuthor a)
    ]

authorJson :: Profile -> Value
authorJson p = object
  [ "username"  .= profileUsername p
  , "bio"       .= profileBio p
  , "image"     .= profileImage p
  , "following" .= profileFollowing p
  ]

data ArticlesResponse = ArticlesResponse
  { arArticles      :: ![Article]
  , arArticlesCount :: !Int
  } deriving (Show, Generic)

instance ToJSON ArticlesResponse where
  toJSON ar = object
    [ "articles"      .= map toJSON (arArticles ar)
    , "articlesCount" .= arArticlesCount ar
    ]

data CreateArticleRequest = CreateArticleRequest
  { caTitle       :: !Text
  , caDescription :: !Text
  , caBody        :: !Text
  , caTagList     :: ![Text]
  } deriving (Show, Generic)

instance FromJSON CreateArticleRequest where
  parseJSON = withObject "CreateArticleRequest" $ \o -> do
    a <- o .: "article"
    CreateArticleRequest
      <$> a .: "title"
      <*> a .: "description"
      <*> a .: "body"
      <*> a .:? "tagList" .!= []

data UpdateArticleRequest = UpdateArticleRequest
  { uaTitle       :: !(Maybe Text)
  , uaDescription :: !(Maybe Text)
  , uaBody        :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON UpdateArticleRequest where
  parseJSON = withObject "UpdateArticleRequest" $ \o -> do
    a <- o .: "article"
    UpdateArticleRequest
      <$> a .:? "title"
      <*> a .:? "description"
      <*> a .:? "body"

-- | Single-article response envelope: @{"article": {...}}@.
newtype ArticleResponse = ArticleResponse Article
  deriving (Show, Generic)

instance ToJSON ArticleResponse where
  toJSON (ArticleResponse a) = object ["article" .= toJSON a]


-- ===================================================================
-- Comment
-- ===================================================================

data Comment = Comment
  { commentId        :: !Int
  , commentCreatedAt :: !UTCTime
  , commentUpdatedAt :: !UTCTime
  , commentBody      :: !Text
  , commentAuthor    :: !Profile
  } deriving (Show, Generic)

instance ToJSON Comment where
  toJSON c = object
    [ "id"        .= commentId c
    , "createdAt" .= commentCreatedAt c
    , "updatedAt" .= commentUpdatedAt c
    , "body"      .= commentBody c
    , "author"    .= authorJson (commentAuthor c)
    ]

data CreateCommentRequest = CreateCommentRequest
  { ccBody :: !Text
  } deriving (Show, Generic)

instance FromJSON CreateCommentRequest where
  parseJSON = withObject "CreateCommentRequest" $ \o -> do
    c <- o .: "comment"
    CreateCommentRequest <$> c .: "body"

-- | Single-comment response envelope: @{"comment": {...}}@.
newtype CommentResponse = CommentResponse Comment
  deriving (Show, Generic)

instance ToJSON CommentResponse where
  toJSON (CommentResponse c) = object ["comment" .= toJSON c]

-- | Multi-comment response envelope: @{"comments": [...]}@.
newtype CommentsResponse = CommentsResponse [Comment]
  deriving (Show, Generic)

instance ToJSON CommentsResponse where
  toJSON (CommentsResponse cs) = object ["comments" .= map toJSON cs]


-- ===================================================================
-- Tags
-- ===================================================================

newtype TagsResponse = TagsResponse { trTags :: [Text] }
  deriving (Show, Generic)

instance ToJSON TagsResponse where
  toJSON (TagsResponse ts) = object ["tags" .= ts]
