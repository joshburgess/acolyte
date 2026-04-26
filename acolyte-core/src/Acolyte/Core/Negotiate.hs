{-# LANGUAGE OverloadedStrings #-}
-- | Type-level content negotiation.
--
-- A single endpoint can serve multiple content types. The handler
-- returns the domain value; the framework selects the serialization
-- format based on the @Accept@ header.
--
-- @
-- type GetArticle = Get '[ Lit "articles", Capture Int ]
--                       (Negotiate '[JsonFormat, XmlFormat, TextFormat] Article)
-- @
--
-- All formats appear in the OpenAPI spec. The handler just returns
-- @Article@ — no format awareness needed.
module Acolyte.Core.Negotiate
  ( -- * Negotiated response type
    Negotiate
    -- * Content format class
  , ContentFormat (..)
    -- * Built-in format tags
  , JsonFormat
  , XmlFormat
  , TextFormat
  , HtmlFormat
  , CsvFormat
  ) where

import Data.Kind (Type)
import Data.Text (Text)


-- | A response that supports multiple content types via negotiation.
--
-- @formats@ is a type-level list of format tags (e.g., @'[JsonFormat, XmlFormat]@).
-- @a@ is the domain type that can be rendered in each format.
--
-- The server package implements the runtime negotiation: parse the
-- @Accept@ header, select the best format, and serialize @a@ using
-- the appropriate @RenderAs@ instance.
type Negotiate :: [Type] -> Type -> Type
data Negotiate (formats :: [Type]) (a :: Type)


-- | A content format tag.
--
-- Each format declares its MIME type and quality. The server package
-- uses this to match against @Accept@ headers.
class ContentFormat (fmt :: Type) where
  -- | The MIME type for this format (e.g., "application/json").
  contentType :: Text

  -- | Default quality weight for this format (0.0–1.0).
  -- Used to break ties when the client has no preference.
  defaultQuality :: Double
  defaultQuality = 1.0


-- ===================================================================
-- Built-in format tags
-- ===================================================================

-- | JSON format (@application/json@).
data JsonFormat

instance ContentFormat JsonFormat where
  contentType = "application/json"

-- | XML format (@application/xml@).
data XmlFormat

instance ContentFormat XmlFormat where
  contentType = "application/xml"
  defaultQuality = 0.9

-- | Plain text format (@text/plain@).
data TextFormat

instance ContentFormat TextFormat where
  contentType = "text/plain"
  defaultQuality = 0.5

-- | HTML format (@text/html@).
data HtmlFormat

instance ContentFormat HtmlFormat where
  contentType = "text/html"
  defaultQuality = 0.8

-- | CSV format (@text/csv@).
data CsvFormat

instance ContentFormat CsvFormat where
  contentType = "text/csv"
  defaultQuality = 0.3
