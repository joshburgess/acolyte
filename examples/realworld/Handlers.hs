{-# LANGUAGE OverloadedStrings #-}
-- | Request handlers for the RealWorld API.
-- Uses in-memory Store — no database.
module Handlers where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import Data.Time (getCurrentTime)
import Network.HTTP.Types (status401, status404, status422)

import Http.Core (RequestParts (..), lookupExtension)
import Servant.Reimagined.Server

import Types
import Store
import API


-- ===================================================================
-- Auth helper: extract username from "Token xxx" header
-- ===================================================================

getAuthUser :: Store -> FullRequest -> IO (Maybe StoredUser)
getAuthUser store (FullRequest parts) = do
  let mAuth = lookup "authorization" (rpHeaders parts)
  case mAuth of
    Just authHeader -> do
      let token = BS8.drop 6 authHeader  -- drop "Token "
      users <- readIORef (storeUsers store)
      pure $ findByToken (TE.decodeUtf8 token) (Map.elems users)
    Nothing -> pure Nothing

findByToken :: Text -> [StoredUser] -> Maybe StoredUser
findByToken _ [] = Nothing
findByToken tok (u:us)
  | suToken u == tok = Just u
  | otherwise = findByToken tok us


-- ===================================================================
-- User -> Types conversion
-- ===================================================================

storedToUser :: StoredUser -> User
storedToUser su = User
  { userEmail    = suEmail su
  , userToken    = suToken su
  , userUsername = suUsername su
  , userBio      = suBio su
  , userImage    = suImage su
  }

storedToProfile :: Store -> Maybe Text -> StoredUser -> IO Profile
storedToProfile store mViewer su = do
  following <- case mViewer of
    Nothing -> pure False
    Just viewer -> do
      follows <- readIORef (storeFollows store)
      pure $ suUsername su `elem` fromMaybe [] (Map.lookup viewer follows)
  pure Profile
    { profileUsername  = suUsername su
    , profileBio       = suBio su
    , profileImage     = suImage su
    , profileFollowing = following
    }

slugify :: Text -> Text
slugify = T.intercalate "-" . T.words . T.toLower


-- ===================================================================
-- Handlers
-- ===================================================================

-- POST /api/users/login
loginHandler :: Store -> JsonBody LoginRequest -> IO (Either ServerError (Json User))
loginHandler store (JsonBody (LoginRequest email pass)) = do
  users <- readIORef (storeUsers store)
  case findByEmail email (Map.elems users) of
    Just u | suPassword u == pass ->
      pure $ Right (Json (storedToUser u))
    _ -> pure $ Left (mkError status401 "invalid email or password")

findByEmail :: Text -> [StoredUser] -> Maybe StoredUser
findByEmail _ [] = Nothing
findByEmail email (u:us)
  | suEmail u == email = Just u
  | otherwise = findByEmail email us

-- POST /api/users (register)
registerHandler :: Store -> JsonBody RegisterRequest -> IO (Json User)
registerHandler store (JsonBody (RegisterRequest username email pass)) = do
  let token = "tok-" <> username  -- fake token
      su = StoredUser username email pass "" Nothing token
  modifyIORef' (storeUsers store) (Map.insert username su)
  pure (Json (storedToUser su))

-- GET /api/user (current user, auth required)
getCurrentUserHandler :: Store -> FullRequest -> IO (Either ServerError (Json User))
getCurrentUserHandler store req = do
  mUser <- getAuthUser store req
  case mUser of
    Just u  -> pure $ Right (Json (storedToUser u))
    Nothing -> pure $ Left (mkError status401 "not authenticated")

-- PUT /api/user (update user, auth required)
updateUserHandler :: Store -> FullRequest -> JsonBody UpdateUserRequest -> IO (Either ServerError (Json User))
updateUserHandler store req (JsonBody (UpdateUserRequest mEmail mUsername mBio mImage)) = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkError status401 "not authenticated")
    Just u -> do
      let updated = u
            { suEmail    = fromMaybe (suEmail u) mEmail
            , suUsername = fromMaybe (suUsername u) mUsername
            , suBio      = fromMaybe (suBio u) mBio
            , suImage    = mImage
            }
      modifyIORef' (storeUsers store) (Map.insert (suUsername updated) updated)
      pure $ Right (Json (storedToUser updated))

-- GET /api/profiles/:username
getProfileHandler :: Store -> PathCapture Text -> IO (Either ServerError (Json Profile))
getProfileHandler store (PathCapture username) = do
  users <- readIORef (storeUsers store)
  case Map.lookup username users of
    Just u -> do
      profile <- storedToProfile store Nothing u
      pure $ Right (Json profile)
    Nothing -> pure $ Left (mkError status404 "profile not found")

-- POST /api/profiles/:username/follow
followHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either ServerError (Json Profile))
followHandler store (PathCapture target) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkError status401 "not authenticated")
    Just viewer -> do
      modifyIORef' (storeFollows store) $ \follows ->
        Map.insertWith (\_ old -> if target `elem` old then old else target : old)
          (suUsername viewer) [target] follows
      users <- readIORef (storeUsers store)
      case Map.lookup target users of
        Just u -> do
          profile <- storedToProfile store (Just (suUsername viewer)) u
          pure $ Right (Json profile)
        Nothing -> pure $ Left (mkError status404 "user not found")

-- DELETE /api/profiles/:username/follow
unfollowHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either ServerError (Json Profile))
unfollowHandler store (PathCapture target) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkError status401 "not authenticated")
    Just viewer -> do
      modifyIORef' (storeFollows store) $ \follows ->
        Map.adjust (filter (/= target)) (suUsername viewer) follows
      users <- readIORef (storeUsers store)
      case Map.lookup target users of
        Just u -> do
          profile <- storedToProfile store (Just (suUsername viewer)) u
          pure $ Right (Json profile)
        Nothing -> pure $ Left (mkError status404 "user not found")

-- GET /api/articles
listArticlesHandler :: Store -> IO (Json ArticlesResponse)
listArticlesHandler store = do
  articles <- readIORef (storeArticles store)
  users <- readIORef (storeUsers store)
  let arts = Map.elems articles
  jsonArts <- mapM (storedToArticle store Nothing users) arts
  pure (Json (ArticlesResponse jsonArts (length jsonArts)))

-- GET /api/articles/feed (auth required)
feedHandler :: Store -> FullRequest -> IO (Either ServerError (Json ArticlesResponse))
feedHandler store req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkError status401 "not authenticated")
    Just viewer -> do
      follows <- readIORef (storeFollows store)
      let following = fromMaybe [] (Map.lookup (suUsername viewer) follows)
      articles <- readIORef (storeArticles store)
      users <- readIORef (storeUsers store)
      let arts = filter (\a -> saAuthor a `elem` following) (Map.elems articles)
      jsonArts <- mapM (storedToArticle store (Just (suUsername viewer)) users) arts
      pure $ Right (Json (ArticlesResponse jsonArts (length jsonArts)))

-- GET /api/articles/:slug
getArticleHandler :: Store -> PathCapture Text -> IO (Either ServerError (Json Article))
getArticleHandler store (PathCapture slug) = do
  articles <- readIORef (storeArticles store)
  users <- readIORef (storeUsers store)
  case Map.lookup slug articles of
    Just a -> do
      art <- storedToArticle store Nothing users a
      pure $ Right (Json art)
    Nothing -> pure $ Left (mkError status404 "article not found")

-- POST /api/articles (auth required)
createArticleHandler :: Store -> FullRequest -> JsonBody CreateArticleRequest -> IO (Either ServerError (Json Article))
createArticleHandler store req (JsonBody (CreateArticleRequest title desc artBody tags)) = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkError status401 "not authenticated")
    Just viewer -> do
      now <- getCurrentTime
      let slug = slugify title
          sa = StoredArticle slug title desc artBody tags (suUsername viewer) now now []
      modifyIORef' (storeArticles store) (Map.insert slug sa)
      users <- readIORef (storeUsers store)
      art <- storedToArticle store (Just (suUsername viewer)) users sa
      pure $ Right (Json art)

-- DELETE /api/articles/:slug (auth required)
deleteArticleHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either ServerError (Json Article))
deleteArticleHandler store (PathCapture slug) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkError status401 "not authenticated")
    Just _viewer -> do
      articles <- readIORef (storeArticles store)
      users <- readIORef (storeUsers store)
      case Map.lookup slug articles of
        Just a -> do
          modifyIORef' (storeArticles store) (Map.delete slug)
          art <- storedToArticle store Nothing users a
          pure $ Right (Json art)
        Nothing -> pure $ Left (mkError status404 "article not found")

-- GET /api/articles/:slug/comments
getCommentsHandler :: Store -> PathCapture Text -> IO (Json [Comment])
getCommentsHandler store (PathCapture slug) = do
  comments <- readIORef (storeComments store)
  users <- readIORef (storeUsers store)
  let slugComments = filter (\c -> scArticle c == slug) comments
  jsonComments <- mapM (storedToComment store Nothing users) slugComments
  pure (Json jsonComments)

-- POST /api/articles/:slug/comments (auth required)
createCommentHandler :: Store -> PathCapture Text -> FullRequest -> JsonBody CreateCommentRequest -> IO (Either ServerError (Json Comment))
createCommentHandler store (PathCapture slug) req (JsonBody (CreateCommentRequest cBody)) = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkError status401 "not authenticated")
    Just viewer -> do
      now <- getCurrentTime
      cid <- atomicModifyIORef' (storeNextId store) (\n -> (n + 1, n))
      let sc = StoredComment cid cBody (suUsername viewer) slug now
      modifyIORef' (storeComments store) (sc :)
      users <- readIORef (storeUsers store)
      comment <- storedToComment store (Just (suUsername viewer)) users sc
      pure $ Right (Json comment)

-- GET /api/tags
tagsHandler :: Store -> IO (Json TagsResponse)
tagsHandler store = do
  articles <- readIORef (storeArticles store)
  let allTags = concatMap saTagList (Map.elems articles)
      uniqueTags = Map.keys (Map.fromList [(t, ()) | t <- allTags])
  pure (Json (TagsResponse uniqueTags))


-- ===================================================================
-- Conversion helpers
-- ===================================================================

storedToArticle :: Store -> Maybe Text -> Map Text StoredUser -> StoredArticle -> IO Article
storedToArticle store mViewer users sa = do
  let authorUser = Map.lookup (saAuthor sa) users
  authorProfile <- case authorUser of
    Just u  -> storedToProfile store mViewer u
    Nothing -> pure Profile
      { profileUsername = saAuthor sa, profileBio = ""
      , profileImage = Nothing, profileFollowing = False }
  pure Article
    { articleSlug           = saSlug sa
    , articleTitle          = saTitle sa
    , articleDescription   = saDescription sa
    , articleBody          = saBody sa
    , articleTagList       = saTagList sa
    , articleCreatedAt     = saCreatedAt sa
    , articleUpdatedAt     = saUpdatedAt sa
    , articleFavorited     = maybe False (`elem` saFavorites sa) mViewer
    , articleFavoritesCount = length (saFavorites sa)
    , articleAuthor        = authorProfile
    }

storedToComment :: Store -> Maybe Text -> Map Text StoredUser -> StoredComment -> IO Comment
storedToComment store mViewer users sc = do
  let authorUser = Map.lookup (scAuthor sc) users
  authorProfile <- case authorUser of
    Just u  -> storedToProfile store mViewer u
    Nothing -> pure Profile
      { profileUsername = scAuthor sc, profileBio = ""
      , profileImage = Nothing, profileFollowing = False }
  pure Comment
    { commentId        = scId sc
    , commentCreatedAt = scCreatedAt sc
    , commentUpdatedAt = scCreatedAt sc
    , commentBody      = scBody sc
    , commentAuthor    = authorProfile
    }
