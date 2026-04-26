-- | Server-side content negotiation for @Negotiate@ types.
--
-- Parses the @Accept@ header, matches against the format list,
-- and serializes the response using the best matching format.
module Acolyte.Server.Negotiate
  ( -- * Content format serialization
    FormatEncoder (..)
    -- * Negotiation
  , negotiate
  , parseAccept
  , matchFormat
    -- * Negotiated response wrapper
  , NegotiatedResponse (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (sortBy)
import Data.Ord (Down (..))
import Data.Char (isSpace, isDigit)
import Network.HTTP.Types (status200, status406)

import qualified Data.Aeson as Aeson

import Http.Core (Response (..))
import Acolyte.Core.Negotiate
  ( ContentFormat (..)
  , JsonFormat
  , XmlFormat
  , TextFormat
  , HtmlFormat
  )


-- | Encode a value in a specific content format.
--
-- Each @(format, value-type)@ pair needs an instance.
class ContentFormat fmt => FormatEncoder fmt a where
  formatEncode :: a -> ByteString


-- | JSON encoding via aeson.
instance Aeson.ToJSON a => FormatEncoder JsonFormat a where
  formatEncode = LBS.toStrict . Aeson.encode


-- | Plain text encoding for 'Text'.
instance FormatEncoder TextFormat Text where
  formatEncode = TE.encodeUtf8


-- | Plain text encoding for 'String'.
instance FormatEncoder TextFormat String where
  formatEncode = TE.encodeUtf8 . T.pack


-- | Simple XML wrapping. Not a full XML serializer — wraps the value
-- in a @\<value\>@ tag using its 'Show' instance.
instance Show a => FormatEncoder XmlFormat a where
  formatEncode a =
    TE.encodeUtf8 $ "<?xml version=\"1.0\"?><value>" <> T.pack (show a) <> "</value>"


-- | HTML encoding for 'Text' — wraps in a minimal HTML document.
instance FormatEncoder HtmlFormat Text where
  formatEncode t =
    TE.encodeUtf8 $ "<html><body>" <> t <> "</body></html>"


-- | A response produced by content negotiation, pairing the selected
-- content type with the serialized body.
data NegotiatedResponse = NegotiatedResponse
  { nrContentType :: !ByteString
  , nrBody        :: !ByteString
  } deriving (Show, Eq)


-- | Parse an @Accept@ header into a list of @(media-type, quality)@ pairs,
-- sorted by descending quality.
--
-- Examples:
--
-- @
-- parseAccept "application/json, text/plain;q=0.9"
--   == [("application/json", 1.0), ("text/plain", 0.9)]
-- @
--
-- Missing quality values default to 1.0. The @*/*@ wildcard is preserved
-- as-is for downstream matching.
parseAccept :: ByteString -> [(ByteString, Double)]
parseAccept hdr
  | BS.null hdr = []
  | otherwise   = sortBy (\a b -> compare (Down (snd a)) (Down (snd b)))
                $ map parseEntry (BS8.split ',' hdr)
  where
    parseEntry :: ByteString -> (ByteString, Double)
    parseEntry entry =
      case BS8.break (== ';') (trimBS entry) of
        (mediaType, rest)
          | BS.null rest -> (trimBS mediaType, 1.0)
          | otherwise    -> (trimBS mediaType, parseQuality rest)

    parseQuality :: ByteString -> Double
    parseQuality bs =
      -- rest starts with ';', so drop it, then look for q=
      let params = BS8.drop 1 bs  -- drop the ';'
          parts  = BS8.split ';' params
      in case filter (isQParam . trimBS) parts of
           (p : _) -> readQ (trimBS p)
           []       -> 1.0

    isQParam :: ByteString -> Bool
    isQParam bs =
      let stripped = trimBS bs
      in BS8.take 2 stripped == "q=" || BS8.take 2 stripped == "Q="

    readQ :: ByteString -> Double
    readQ bs =
      let valStr = BS8.unpack (BS8.drop 2 bs)
          cleaned = takeWhile (\c -> isDigit c || c == '.') valStr
      in case reads cleaned of
           ((d, _) : _) -> d
           []            -> 1.0

    trimBS :: ByteString -> ByteString
    trimBS = BS8.dropWhile isSpaceByte . BS8.reverse . BS8.dropWhile isSpaceByte . BS8.reverse

    isSpaceByte :: Char -> Bool
    isSpaceByte = isSpace


-- | Given a parsed Accept header and a list of available
-- @(content-type, encoder)@ pairs, find the best matching encoder.
--
-- Returns 'Nothing' if no match is found (the caller should produce 406).
matchFormat
  :: ByteString                          -- ^ Raw Accept header
  -> [(Text, a -> ByteString)]           -- ^ Available formats: (content-type, encoder)
  -> Maybe (Text, a -> ByteString)       -- ^ Best match
matchFormat acceptHdr available
  | BS.null acceptHdr = case available of
      []    -> Nothing
      (f:_) -> Just f
  | otherwise =
      let parsed = parseAccept acceptHdr
      in firstMatch parsed available

  where
    firstMatch :: [(ByteString, Double)] -> [(Text, a -> ByteString)] -> Maybe (Text, a -> ByteString)
    firstMatch [] avail = Nothing
    firstMatch ((mediaType, _q) : rest) avail =
      case findAvailable mediaType avail of
        Just found -> Just found
        Nothing
          | mediaType == "*/*" -> case avail of
              []    -> Nothing
              (f:_) -> Just f
          | isWildcardSubtype mediaType -> findWildcardMatch mediaType avail rest
          | otherwise -> firstMatch rest avail

    findAvailable :: ByteString -> [(Text, a -> ByteString)] -> Maybe (Text, a -> ByteString)
    findAvailable _  [] = Nothing
    findAvailable mt ((ct, enc) : rest)
      | TE.encodeUtf8 ct == mt = Just (ct, enc)
      | otherwise               = findAvailable mt rest

    isWildcardSubtype :: ByteString -> Bool
    isWildcardSubtype bs = BS8.isSuffixOf "/*" bs

    findWildcardMatch :: ByteString -> [(Text, a -> ByteString)] -> [(ByteString, Double)] -> Maybe (Text, a -> ByteString)
    findWildcardMatch wildcard avail rest =
      let prefix = BS8.takeWhile (/= '/') wildcard  -- e.g. "text" from "text/*"
      in case filter (\(ct, _) -> TE.encodeUtf8 (T.takeWhile (/= '/') ct) == prefix) avail of
           (f:_) -> Just f
           []    -> firstMatch rest avail


-- | Perform content negotiation: given an Accept header, a list of
-- available formats, and a value, produce an HTTP response.
--
-- If no format matches, returns 406 Not Acceptable.
negotiate
  :: ByteString                          -- ^ Raw Accept header
  -> [(Text, a -> ByteString)]           -- ^ Available formats: (content-type, encoder)
  -> a                                   -- ^ The value to serialize
  -> Response ByteString
negotiate acceptHdr available val =
  case matchFormat acceptHdr available of
    Just (ct, encoder) ->
      Response status200
        [("Content-Type", TE.encodeUtf8 ct)]
        (encoder val)
    Nothing ->
      Response status406
        [("Content-Type", "text/plain")]
        "406 Not Acceptable"
