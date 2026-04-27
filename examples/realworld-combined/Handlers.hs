{-# LANGUAGE OverloadedStrings #-}
-- | Request handlers for the RealWorld API.
-- Uses in-memory Store — no database.
module Handlers where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Network.HTTP.Types (Status, status204, status401, status403, status404)

import Http.Core (RequestParts (..), Response (..))
import Acolyte.Server

import Types
import Store


-- ===================================================================
-- RealWorld error type: produces spec-compliant error JSON.
--
-- The RealWorld spec mandates errors look like:
--
--   {"errors": {"body": ["error message"]}}
--
-- We use a local type with a custom 'IntoResponse' instance instead
-- of the framework's 'ServerError' (which uses a different shape).
-- ===================================================================

data RealWorldError = RealWorldError
  { rwStatus  :: !Status
  , rwMessage :: !Text
  } deriving (Show)

mkRwError :: Status -> Text -> RealWorldError
mkRwError = RealWorldError

instance IntoResponse RealWorldError where
  intoResponse (RealWorldError s msg) =
    let body = Aeson.encode $ Aeson.object
          [ "errors" Aeson..= Aeson.object
              [ "body" Aeson..= [msg] ]
          ]
    in Response s [("Content-Type", "application/json")] (LBS.toStrict body)


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
loginHandler :: Store -> JsonBody LoginRequest -> IO (Either RealWorldError (Json User))
loginHandler store (JsonBody (LoginRequest email pass)) = do
  users <- readIORef (storeUsers store)
  case findByEmail email (Map.elems users) of
    Just u | suPassword u == pass ->
      pure $ Right (Json (storedToUser u))
    _ -> pure $ Left (mkRwError status401 "invalid email or password")

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
getCurrentUserHandler :: Store -> FullRequest -> IO (Either RealWorldError (Json User))
getCurrentUserHandler store req = do
  mUser <- getAuthUser store req
  case mUser of
    Just u  -> pure $ Right (Json (storedToUser u))
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")

-- PUT /api/user (update user, auth required)
updateUserHandler :: Store -> FullRequest -> JsonBody UpdateUserRequest -> IO (Either RealWorldError (Json User))
updateUserHandler store req (JsonBody (UpdateUserRequest mEmail mUsername mBio mImage)) = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just u -> do
      let updated = u
            { suEmail    = fromMaybe (suEmail u) mEmail
            , suUsername = fromMaybe (suUsername u) mUsername
            , suBio      = fromMaybe (suBio u) mBio
            , suImage    = mImage
            }
      modifyIORef' (storeUsers store) (Map.insert (suUsername updated) updated)
      pure $ Right (Json (storedToUser updated))

-- GET /api/profiles/:username (optional auth: 'following' reflects viewer)
getProfileHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either RealWorldError (Json Profile))
getProfileHandler store (PathCapture username) req = do
  mViewer <- fmap suUsername <$> getAuthUser store req
  users <- readIORef (storeUsers store)
  case Map.lookup username users of
    Just u -> do
      profile <- storedToProfile store mViewer u
      pure $ Right (Json profile)
    Nothing -> pure $ Left (mkRwError status404 "profile not found")

-- POST /api/profiles/:username/follow
followHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either RealWorldError (Json Profile))
followHandler store (PathCapture target) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      modifyIORef' (storeFollows store) $ \follows ->
        Map.insertWith (\_ old -> if target `elem` old then old else target : old)
          (suUsername viewer) [target] follows
      users <- readIORef (storeUsers store)
      case Map.lookup target users of
        Just u -> do
          profile <- storedToProfile store (Just (suUsername viewer)) u
          pure $ Right (Json profile)
        Nothing -> pure $ Left (mkRwError status404 "user not found")

-- DELETE /api/profiles/:username/follow
unfollowHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either RealWorldError (Json Profile))
unfollowHandler store (PathCapture target) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      modifyIORef' (storeFollows store) $ \follows ->
        Map.adjust (filter (/= target)) (suUsername viewer) follows
      users <- readIORef (storeUsers store)
      case Map.lookup target users of
        Just u -> do
          profile <- storedToProfile store (Just (suUsername viewer)) u
          pure $ Right (Json profile)
        Nothing -> pure $ Left (mkRwError status404 "user not found")

-- GET /api/articles (optional auth: 'favorited'/'following' reflect viewer)
listArticlesHandler :: Store -> FullRequest -> IO (Json ArticlesResponse)
listArticlesHandler store req = do
  mViewer <- fmap suUsername <$> getAuthUser store req
  articles <- readIORef (storeArticles store)
  users <- readIORef (storeUsers store)
  let arts = Map.elems articles
  jsonArts <- mapM (storedToArticle store mViewer users) arts
  pure (Json (ArticlesResponse jsonArts (length jsonArts)))

-- GET /api/articles/feed (auth required)
feedHandler :: Store -> FullRequest -> IO (Either RealWorldError (Json ArticlesResponse))
feedHandler store req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      follows <- readIORef (storeFollows store)
      let following = fromMaybe [] (Map.lookup (suUsername viewer) follows)
      articles <- readIORef (storeArticles store)
      users <- readIORef (storeUsers store)
      let arts = filter (\a -> saAuthor a `elem` following) (Map.elems articles)
      jsonArts <- mapM (storedToArticle store (Just (suUsername viewer)) users) arts
      pure $ Right (Json (ArticlesResponse jsonArts (length jsonArts)))

-- GET /api/articles/:slug (optional auth)
getArticleHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either RealWorldError (Json ArticleResponse))
getArticleHandler store (PathCapture slug) req = do
  mViewer <- fmap suUsername <$> getAuthUser store req
  articles <- readIORef (storeArticles store)
  users <- readIORef (storeUsers store)
  case Map.lookup slug articles of
    Just a -> do
      art <- storedToArticle store mViewer users a
      pure $ Right (Json (ArticleResponse art))
    Nothing -> pure $ Left (mkRwError status404 "article not found")

-- POST /api/articles (auth required)
createArticleHandler :: Store -> FullRequest -> JsonBody CreateArticleRequest -> IO (Either RealWorldError (Json ArticleResponse))
createArticleHandler store req (JsonBody (CreateArticleRequest title desc artBody tags)) = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      now <- getCurrentTime
      let slug = slugify title
          sa = StoredArticle slug title desc artBody tags (suUsername viewer) now now []
      modifyIORef' (storeArticles store) (Map.insert slug sa)
      users <- readIORef (storeUsers store)
      art <- storedToArticle store (Just (suUsername viewer)) users sa
      pure $ Right (Json (ArticleResponse art))

-- PUT /api/articles/:slug (auth required, author only)
updateArticleHandler :: Store -> PathCapture Text -> FullRequest -> JsonBody UpdateArticleRequest -> IO (Either RealWorldError (Json ArticleResponse))
updateArticleHandler store (PathCapture slug) req (JsonBody (UpdateArticleRequest mTitle mDesc mBody)) = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      articles <- readIORef (storeArticles store)
      case Map.lookup slug articles of
        Nothing -> pure $ Left (mkRwError status404 "article not found")
        Just a
          | saAuthor a /= suUsername viewer ->
              pure $ Left (mkRwError status403 "only the author can edit this article")
          | otherwise -> do
              now <- getCurrentTime
              let newTitle = fromMaybe (saTitle a) mTitle
                  newSlug  = if newTitle /= saTitle a then slugify newTitle else slug
                  updated = a
                    { saTitle       = newTitle
                    , saSlug        = newSlug
                    , saDescription = fromMaybe (saDescription a) mDesc
                    , saBody        = fromMaybe (saBody a) mBody
                    , saUpdatedAt   = now
                    }
              modifyIORef' (storeArticles store) $
                Map.insert newSlug updated . Map.delete slug
              users <- readIORef (storeUsers store)
              art <- storedToArticle store (Just (suUsername viewer)) users updated
              pure $ Right (Json (ArticleResponse art))

-- DELETE /api/articles/:slug (auth required, 204 No Content)
deleteArticleHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either RealWorldError Status)
deleteArticleHandler store (PathCapture slug) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      articles <- readIORef (storeArticles store)
      case Map.lookup slug articles of
        Nothing -> pure $ Left (mkRwError status404 "article not found")
        Just a
          | saAuthor a /= suUsername viewer ->
              pure $ Left (mkRwError status403 "only the author can delete this article")
          | otherwise -> do
              modifyIORef' (storeArticles store) (Map.delete slug)
              modifyIORef' (storeComments store) (filter (\c -> scArticle c /= slug))
              pure $ Right status204

-- POST /api/articles/:slug/favorite (auth required)
favoriteHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either RealWorldError (Json ArticleResponse))
favoriteHandler store (PathCapture slug) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      let viewerName = suUsername viewer
      articles <- readIORef (storeArticles store)
      case Map.lookup slug articles of
        Nothing -> pure $ Left (mkRwError status404 "article not found")
        Just a -> do
          let favs    = saFavorites a
              newFavs = if viewerName `elem` favs then favs else viewerName : favs
              updated = a { saFavorites = newFavs }
          modifyIORef' (storeArticles store) (Map.insert slug updated)
          users <- readIORef (storeUsers store)
          art <- storedToArticle store (Just viewerName) users updated
          pure $ Right (Json (ArticleResponse art))

-- DELETE /api/articles/:slug/favorite (auth required)
unfavoriteHandler :: Store -> PathCapture Text -> FullRequest -> IO (Either RealWorldError (Json ArticleResponse))
unfavoriteHandler store (PathCapture slug) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      let viewerName = suUsername viewer
      articles <- readIORef (storeArticles store)
      case Map.lookup slug articles of
        Nothing -> pure $ Left (mkRwError status404 "article not found")
        Just a -> do
          let updated = a { saFavorites = filter (/= viewerName) (saFavorites a) }
          modifyIORef' (storeArticles store) (Map.insert slug updated)
          users <- readIORef (storeUsers store)
          art <- storedToArticle store (Just viewerName) users updated
          pure $ Right (Json (ArticleResponse art))

-- GET /api/articles/:slug/comments (optional auth)
getCommentsHandler :: Store -> PathCapture Text -> FullRequest -> IO (Json CommentsResponse)
getCommentsHandler store (PathCapture slug) req = do
  mViewer <- fmap suUsername <$> getAuthUser store req
  comments <- readIORef (storeComments store)
  users <- readIORef (storeUsers store)
  let slugComments = filter (\c -> scArticle c == slug) comments
  jsonComments <- mapM (storedToComment store mViewer users) slugComments
  pure (Json (CommentsResponse jsonComments))

-- POST /api/articles/:slug/comments (auth required)
createCommentHandler :: Store -> PathCapture Text -> FullRequest -> JsonBody CreateCommentRequest -> IO (Either RealWorldError (Json CommentResponse))
createCommentHandler store (PathCapture slug) req (JsonBody (CreateCommentRequest cBody)) = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      now <- getCurrentTime
      cid <- atomicModifyIORef' (storeNextId store) (\n -> (n + 1, n))
      let sc = StoredComment cid cBody (suUsername viewer) slug now
      modifyIORef' (storeComments store) (sc :)
      users <- readIORef (storeUsers store)
      comment <- storedToComment store (Just (suUsername viewer)) users sc
      pure $ Right (Json (CommentResponse comment))

-- DELETE /api/articles/:slug/comments/:id (auth required, author only, 204)
deleteCommentHandler :: Store -> PathCapture Text -> PathCapture Int -> FullRequest -> IO (Either RealWorldError Status)
deleteCommentHandler store (PathCapture slug) (PathCapture cid) req = do
  mUser <- getAuthUser store req
  case mUser of
    Nothing -> pure $ Left (mkRwError status401 "not authenticated")
    Just viewer -> do
      comments <- readIORef (storeComments store)
      case filter (\c -> scId c == cid && scArticle c == slug) comments of
        [] -> pure $ Left (mkRwError status404 "comment not found")
        (c : _)
          | scAuthor c /= suUsername viewer ->
              pure $ Left (mkRwError status403 "only the author can delete this comment")
          | otherwise -> do
              modifyIORef' (storeComments store) (filter (\x -> scId x /= cid))
              pure $ Right status204

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
