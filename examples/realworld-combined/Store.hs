{-# LANGUAGE OverloadedStrings #-}
-- | In-memory store for the RealWorld app.
-- No database dependency — uses IORef + Map.
module Store
  ( Store (..)
  , newStore
  , StoredUser (..)
  , StoredArticle (..)
  , StoredComment (..)
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)


-- | Stored user record (includes password hash placeholder).
data StoredUser = StoredUser
  { suUsername :: !Text
  , suEmail    :: !Text
  , suPassword :: !Text   -- plaintext for demo; real app would hash
  , suBio      :: !Text
  , suImage    :: !(Maybe Text)
  , suToken    :: !Text   -- fake JWT token
  } deriving (Show)

-- | Stored article record.
data StoredArticle = StoredArticle
  { saSlug        :: !Text
  , saTitle       :: !Text
  , saDescription :: !Text
  , saBody        :: !Text
  , saTagList     :: ![Text]
  , saAuthor      :: !Text    -- username
  , saCreatedAt   :: !UTCTime
  , saUpdatedAt   :: !UTCTime
  , saFavorites   :: ![Text]  -- usernames who favorited
  } deriving (Show)

-- | Stored comment record.
data StoredComment = StoredComment
  { scId        :: !Int
  , scBody      :: !Text
  , scAuthor    :: !Text    -- username
  , scArticle   :: !Text    -- slug
  , scCreatedAt :: !UTCTime
  } deriving (Show)

-- | In-memory application state.
data Store = Store
  { storeUsers    :: !(IORef (Map Text StoredUser))       -- keyed by username
  , storeArticles :: !(IORef (Map Text StoredArticle))    -- keyed by slug
  , storeComments :: !(IORef [StoredComment])
  , storeFollows  :: !(IORef (Map Text [Text]))           -- follower -> [following]
  , storeNextId   :: !(IORef Int)
  }

newStore :: IO Store
newStore = do
  users    <- newIORef Map.empty
  articles <- newIORef Map.empty
  comments <- newIORef []
  follows  <- newIORef Map.empty
  nextId   <- newIORef 1
  pure Store
    { storeUsers    = users
    , storeArticles = articles
    , storeComments = comments
    , storeFollows  = follows
    , storeNextId   = nextId
    }
