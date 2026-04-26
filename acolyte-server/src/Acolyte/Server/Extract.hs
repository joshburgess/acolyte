-- | Request extraction: turning raw requests into typed handler arguments.
--
-- Two extraction protocols mirror the typeway/axum pattern:
--
-- * 'FromRequestParts' — extracts from method, path, query, headers,
--   extensions (can be called multiple times per request)
-- * 'FromRequest' — extracts from the body (consumed once)
module Acolyte.Server.Extract
  ( -- * Extraction protocols
    FromRequestParts (..)
  , FromRequest (..)
    -- * Built-in extractors
  , PathCapture (..)
  , JsonBody (..)
  , ValidatedBody (..)
  , AppState (..)
  , RawBody (..)
  , ReqHeader (..)
  , Extension (..)
  , Optional (..)
  , ReqMethod (..)
    -- * Query parameter extractors
  , QueryParam (..)
  , QueryParams (..)
  , OptionalParam (..)
    -- * Header extractors
  , HeaderMap (..)
    -- * Body extractors
  , BodyBytes (..)
  , StringBody (..)
  , Form (..)
  , FromForm (..)
  , RawForm (..)
  , Multipart (..)
  , FilePart (..)
    -- * Form/Multipart parsing (internal, exported for testing)
  , parseFormUrlEncoded
  , parseMultipart
    -- * Request access
  , FullRequest (..)
  , RawQuery (..)
  , OriginalUri (..)
  , MatchedPath (..)
  , NestedPath (..)
  , ConnectInfo (..)
    -- * Capture support
  , CaptureList (..)
  , RawPathParams (..)
  , ParseCapture (..)
    -- * Server error
  , ServerError (..)
  , mkError
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy (Proxy (..))
import Network.HTTP.Types (Status, status400, status422, status500, urlDecode)

import qualified Data.Aeson as Aeson

import Http.Core (RequestParts (..), Extensions, lookupExtension, insertExtension)
import Acolyte.Core.Wrapper (Validate (..))


-- | A structured server error with status and message.
data ServerError = ServerError
  { seStatus  :: !Status
  , seMessage :: !Text
  } deriving (Show)

-- | Construct a 'ServerError' from a status code and message.
mkError :: Status -> Text -> ServerError
mkError = ServerError


-- | Extract a value from request data available via 'RequestParts'.
--
-- Originally designed for non-body extractions (path, query, headers,
-- extensions), but body types ('JsonBody', 'RawBody', 'StringBody',
-- 'Form', 'Multipart') also have instances — they read from
-- 'BodyBytes' stored in request extensions by the router. This means
-- 'FromRequestParts' is the universal extraction protocol used by
-- 'ToHandler' to peel handler arguments, regardless of whether the
-- data comes from the URL, headers, or body.
--
-- Can be called multiple times (nothing is consumed).
class FromRequestParts a where
  fromRequestParts :: RequestParts -> IO (Either ServerError a)


-- | Extract a value from the request body (legacy protocol).
--
-- This is the original body extraction interface, taking the raw body
-- bytes as a parameter. It is used by the low-level 'HandlerFn' path
-- ('mkHandler1Body', 'mkHandler2PartsBody'). For ergonomic handlers
-- via 'ToHandler', body extraction goes through 'FromRequestParts'
-- instead (via 'BodyBytes' in extensions).
class FromRequest a where
  fromRequest :: RequestParts -> ByteString -> IO (Either ServerError a)


-- ===================================================================
-- ParseCapture: text -> typed value parsing
-- ===================================================================

-- | Parse a text capture into a typed value.
class ParseCapture a where
  parseCapture :: Text -> Maybe a

instance ParseCapture Int where
  parseCapture t = case T.decimal t of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

instance ParseCapture Text where
  parseCapture = Just

instance ParseCapture String where
  parseCapture = Just . T.unpack


-- ===================================================================
-- BodyBytes: raw body stored in Extensions by the router
-- ===================================================================

-- | Raw request body bytes, stored in Extensions by the router.
-- Enables body extractors to work through 'FromRequestParts'.
newtype BodyBytes = BodyBytes { unBodyBytes :: ByteString }
  deriving (Show, Eq, Typeable)


-- ===================================================================
-- CaptureList: raw captures stored in Extensions by the router
-- ===================================================================

-- | Raw captured path segments stored in Extensions by the router.
newtype CaptureList = CaptureList { unCaptureList :: [Text] }
  deriving (Typeable)


-- ===================================================================
-- PathCapture: extract typed captures from the URL path
-- ===================================================================

-- | A typed path capture extracted from the URL.
--
-- For a path like @'[Lit "users", Capture Int]@, use
-- @PathCapture Int@ in your handler to get the parsed value:
--
-- @
-- getUser :: PathCapture Int -> IO (Json User)
-- getUser (PathCapture uid) = ...
-- @
newtype PathCapture a = PathCapture { unPathCapture :: a }
  deriving (Show, Eq, Typeable)

instance (Typeable a, ParseCapture a) => FromRequestParts (PathCapture a) where
  fromRequestParts parts = do
    mCaps <- lookupExtension @CaptureList (rpExtensions parts)
    case mCaps of
      Just (CaptureList (t : rest)) ->
        case parseCapture @a t of
          Just val -> do
            -- Consume the capture: update CaptureList to remove the head
            insertExtension (CaptureList rest) (rpExtensions parts)
            pure $ Right (PathCapture val)
          Nothing  -> pure $ Left (mkError status400 "Invalid path capture")
      Just (CaptureList []) ->
        pure $ Left (mkError status400 "Missing path capture")
      Nothing ->
        pure $ Left (mkError status500 "CaptureList not found in extensions (router bug)")


-- ===================================================================
-- JsonBody: parse JSON request body
-- ===================================================================

-- | A JSON-decoded request body.
--
-- Works as both a 'FromRequest' extractor (with the old HandlerFn API)
-- and a 'FromRequestParts' extractor (with ergonomic handlers via
-- 'ToHandler'), since the router stores body bytes in Extensions.
--
-- @
-- createUser :: JsonBody CreateUserReq -> IO (Json User)
-- createUser (JsonBody req) = ...
-- @
newtype JsonBody a = JsonBody { unJsonBody :: a }
  deriving (Show, Eq)

instance Aeson.FromJSON a => FromRequest (JsonBody a) where
  fromRequest _parts body =
    case Aeson.eitherDecodeStrict' body of
      Right val -> pure (Right (JsonBody val))
      Left err  -> pure (Left (mkError status422 (T.pack ("JSON parse error: " ++ err))))

instance Aeson.FromJSON a => FromRequestParts (JsonBody a) where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    pure $ case mBody of
      Just (BodyBytes body) ->
        case Aeson.eitherDecodeStrict' body of
          Right val -> Right (JsonBody val)
          Left err  -> Left (mkError status422 (T.pack ("JSON parse error: " ++ err)))
      Nothing -> Left (mkError status500 "Request body not available (router bug)")


-- ===================================================================
-- ValidatedBody: JSON body with validation
-- ===================================================================

-- | A JSON-deserialized request body that has been validated.
--
-- Uses the 'Validate' type class from core to run validation after
-- deserialization. Returns 422 if validation fails.
--
-- @
-- data CreateUserValidator
-- instance Validate CreateUserValidator CreateUser where
--   validate u
--     | T.null (userName u) = Left "name is required"
--     | otherwise           = Right u
--
-- handler :: ValidatedBody CreateUserValidator CreateUser -> IO (Json User)
-- handler (ValidatedBody user) = ...
-- @
newtype ValidatedBody v a = ValidatedBody { unValidatedBody :: a }
  deriving (Show, Eq)

instance (Aeson.FromJSON a, Validate v a) => FromRequest (ValidatedBody v a) where
  fromRequest _parts body =
    case Aeson.eitherDecodeStrict' body of
      Left err -> pure (Left (mkError status422 (T.pack ("JSON parse error: " ++ err))))
      Right a  -> case validate @v a of
        Left msg -> pure (Left (mkError status422 (T.pack msg)))
        Right a' -> pure (Right (ValidatedBody a'))

instance (Aeson.FromJSON a, Validate v a) => FromRequestParts (ValidatedBody v a) where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    pure $ case mBody of
      Just (BodyBytes body) ->
        case Aeson.eitherDecodeStrict' body of
          Left err -> Left (mkError status422 (T.pack ("JSON parse error: " ++ err)))
          Right a  -> case validate @v a of
            Left msg -> Left (mkError status422 (T.pack msg))
            Right a' -> Right (ValidatedBody a')
      Nothing -> Left (mkError status500 "Request body not available (router bug)")


-- ===================================================================
-- AppState: shared application state from Extensions
-- ===================================================================

-- | Shared application state injected via 'serveWithState'.
--
-- @
-- handler :: AppState DbPool -> PathCapture Int -> IO (Json User)
-- @
newtype AppState a = AppState { unAppState :: a }
  deriving (Show, Eq)

instance Typeable a => FromRequestParts (AppState a) where
  fromRequestParts parts = do
    mVal <- lookupExtension @(AppState a) (rpExtensions parts)
    pure $ case mVal of
      Just st -> Right st
      Nothing -> Left (mkError status500 "AppState not found in extensions (forgot serveWithState?)")


-- ===================================================================
-- RawBody: raw request body bytes
-- ===================================================================

-- | The raw request body as a ByteString.
--
-- Works as both a 'FromRequest' and 'FromRequestParts' extractor.
--
-- @
-- handler :: RawBody -> IO (Response ByteString)
-- handler (RawBody bytes) = ...
-- @
newtype RawBody = RawBody { unRawBody :: ByteString }
  deriving (Show, Eq)

instance FromRequest RawBody where
  fromRequest _parts body = pure (Right (RawBody body))

instance FromRequestParts RawBody where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    pure $ case mBody of
      Just (BodyBytes body) -> Right (RawBody body)
      Nothing -> Left (mkError status500 "Request body not available (router bug)")


-- ===================================================================
-- ReqHeader: extract a specific header by type-level name
-- ===================================================================

-- | Extract a specific request header by its type-level name.
--
-- @
-- handler :: ReqHeader "Authorization" -> IO (Json User)
-- @
newtype ReqHeader (name :: Symbol) = ReqHeader { unReqHeader :: ByteString }
  deriving (Show, Eq)

instance KnownSymbol name => FromRequestParts (ReqHeader name) where
  fromRequestParts parts = do
    let headerName = CI.mk (TE.encodeUtf8 (T.pack (symbolVal (Proxy @name))))
    pure $ case lookup headerName (rpHeaders parts) of
      Just val -> Right (ReqHeader val)
      Nothing  -> Left (mkError status400
        (T.pack ("Missing required header: " ++ symbolVal (Proxy @name))))


-- ===================================================================
-- QueryParam: extract a required query parameter
-- ===================================================================

-- | Extract a required, typed query parameter.
--
-- @
-- handler :: QueryParam "page" Int -> IO (Json [Item])
-- handler (QueryParam page) = ...
-- @
--
-- Returns 400 if the parameter is missing or unparseable.
newtype QueryParam (name :: Symbol) a = QueryParam { unQueryParam :: a }
  deriving (Show, Eq)

instance (KnownSymbol name, ParseCapture a) => FromRequestParts (QueryParam name a) where
  fromRequestParts parts = do
    let name = TE.encodeUtf8 (T.pack (symbolVal (Proxy @name)))
    pure $ case lookup name (rpQuery parts) of
      Just (Just val) ->
        case parseCapture @a (TE.decodeUtf8Lenient val) of
          Just v  -> Right (QueryParam v)
          Nothing -> Left (mkError status400
            ("Invalid query parameter: " <> T.pack (symbolVal (Proxy @name))))
      _ -> Left (mkError status400
        ("Missing required query parameter: " <> T.pack (symbolVal (Proxy @name))))


-- ===================================================================
-- QueryParams: extract all values for a repeated query parameter
-- ===================================================================

-- | Extract all values for a repeated query parameter.
--
-- @
-- -- /search?tag=haskell&tag=web
-- handler :: QueryParams "tag" Text -> IO (Json [Article])
-- handler (QueryParams tags) = ...
-- @
--
-- Returns an empty list if the parameter is not present.
newtype QueryParams (name :: Symbol) a = QueryParams { unQueryParams :: [a] }
  deriving (Show, Eq)

instance (KnownSymbol name, ParseCapture a) => FromRequestParts (QueryParams name a) where
  fromRequestParts parts = do
    let name = TE.encodeUtf8 (T.pack (symbolVal (Proxy @name)))
        vals = [ v | (k, Just raw) <- rpQuery parts
                   , k == name
                   , Just v <- [parseCapture @a (TE.decodeUtf8Lenient raw)]
               ]
    pure (Right (QueryParams vals))


-- ===================================================================
-- OptionalParam: extract an optional query parameter
-- ===================================================================

-- | Extract an optional, typed query parameter.
--
-- @
-- handler :: OptionalParam "limit" Int -> IO (Json [Item])
-- handler (OptionalParam mLimit) = do
--   let limit = fromMaybe 20 mLimit
--   ...
-- @
newtype OptionalParam (name :: Symbol) a = OptionalParam { unOptionalParam :: Maybe a }
  deriving (Show, Eq)

instance (KnownSymbol name, ParseCapture a) => FromRequestParts (OptionalParam name a) where
  fromRequestParts parts = do
    let name = TE.encodeUtf8 (T.pack (symbolVal (Proxy @name)))
    pure $ Right $ OptionalParam $ case lookup name (rpQuery parts) of
      Just (Just val) -> parseCapture @a (TE.decodeUtf8Lenient val)
      _               -> Nothing


-- ===================================================================
-- Extension: extract typed data from request extensions
-- ===================================================================

-- | Extract typed data from request extensions.
-- Middleware stores data via 'insertExtension'; handlers retrieve it here.
--
-- @
-- handler :: Extension RequestId -> IO Response
-- @
newtype Extension a = Extension { unExtension :: a }
  deriving (Show, Eq)

instance Typeable a => FromRequestParts (Extension a) where
  fromRequestParts parts = do
    mVal <- lookupExtension @a (rpExtensions parts)
    pure $ case mVal of
      Just val -> Right (Extension val)
      Nothing  -> Left (mkError status500 "Extension not found in request")


-- ===================================================================
-- Optional: make any FromRequestParts extractor optional
-- ===================================================================

-- | Wraps any 'FromRequestParts' extractor to make it optional.
-- Returns 'Nothing' instead of failing if extraction fails.
--
-- @
-- handler :: Optional (ReqHeader "Authorization") -> IO Response
-- @
newtype Optional a = Optional { unOptional :: Maybe a }
  deriving (Show, Eq)

instance FromRequestParts a => FromRequestParts (Optional a) where
  fromRequestParts parts = do
    result <- fromRequestParts @a parts
    pure $ Right $ Optional $ case result of
      Right val -> Just val
      Left _    -> Nothing


-- ===================================================================
-- ReqMethod: extract the HTTP method
-- ===================================================================

-- | Extract the HTTP method from the request.
newtype ReqMethod = ReqMethod { unReqMethod :: ByteString }
  deriving (Show, Eq)

instance FromRequestParts ReqMethod where
  fromRequestParts parts = pure (Right (ReqMethod (rpMethod parts)))


-- ===================================================================
-- HeaderMap: all request headers
-- ===================================================================

-- | All request headers as an association list.
--
-- @
-- handler :: HeaderMap -> IO (Json Value)
-- handler (HeaderMap hdrs) = ...
-- @
newtype HeaderMap = HeaderMap { unHeaderMap :: [(CI.CI ByteString, ByteString)] }
  deriving (Show, Eq)

instance FromRequestParts HeaderMap where
  fromRequestParts parts = pure (Right (HeaderMap (rpHeaders parts)))


-- ===================================================================
-- FullRequest: access the complete RequestParts
-- ===================================================================

-- | Access the full 'RequestParts' — method, path, query, headers,
-- extensions. Use when no specific extractor fits.
--
-- @
-- handler :: FullRequest -> IO (Response ByteString)
-- handler (FullRequest parts) = ...
-- @
newtype FullRequest = FullRequest { unFullRequest :: RequestParts }

instance FromRequestParts FullRequest where
  fromRequestParts parts = pure (Right (FullRequest parts))


-- ===================================================================
-- RawQuery: unparsed query string
-- ===================================================================

-- | The raw query string from the request path (everything after @?@).
--
-- @
-- handler :: RawQuery -> IO Text
-- handler (RawQuery q) = ...
-- @
newtype RawQuery = RawQuery { unRawQuery :: ByteString }
  deriving (Show, Eq)

instance FromRequestParts RawQuery where
  fromRequestParts parts =
    let raw = rpPathRaw parts
        q = BS.drop 1 (snd (BS.break (== 0x3F) raw))  -- 0x3F = '?'
    in pure (Right (RawQuery q))


-- ===================================================================
-- RawPathParams: untyped path captures as text
-- ===================================================================

-- | The raw captured path segments as a list of 'Text', before
-- any type-level parsing. Useful for logging or debugging.
--
-- @
-- handler :: RawPathParams -> IO Text
-- handler (RawPathParams segs) = ...
-- @
newtype RawPathParams = RawPathParams { unRawPathParams :: [Text] }
  deriving (Show, Eq)

instance FromRequestParts RawPathParams where
  fromRequestParts parts = do
    mCaps <- lookupExtension @CaptureList (rpExtensions parts)
    pure $ Right $ RawPathParams $ case mCaps of
      Just (CaptureList caps) -> caps
      Nothing                 -> []


-- ===================================================================
-- StringBody: request body as Text (UTF-8)
-- ===================================================================

-- | The request body decoded as UTF-8 'Text'.
--
-- @
-- handler :: StringBody -> IO Text
-- handler (StringBody txt) = pure ("Echo: " <> txt)
-- @
newtype StringBody = StringBody { unStringBody :: Text }
  deriving (Show, Eq)

instance FromRequest StringBody where
  fromRequest _parts body = pure (Right (StringBody (TE.decodeUtf8Lenient body)))

instance FromRequestParts StringBody where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    pure $ case mBody of
      Just (BodyBytes body) -> Right (StringBody (TE.decodeUtf8Lenient body))
      Nothing -> Left (mkError status500 "Request body not available (router bug)")


-- ===================================================================
-- OriginalUri: the original request URI
-- ===================================================================

-- | The original request URI before any routing transformations.
-- Stored in Extensions by the router.
--
-- @
-- handler :: OriginalUri -> IO Text
-- handler (OriginalUri uri) = ...
-- @
newtype OriginalUri = OriginalUri { unOriginalUri :: ByteString }
  deriving (Show, Eq, Typeable)

instance FromRequestParts OriginalUri where
  fromRequestParts parts =
    -- If stored by the router, use that; otherwise fall back to rpPathRaw
    do mUri <- lookupExtension @OriginalUri (rpExtensions parts)
       pure $ Right $ case mUri of
         Just uri -> uri
         Nothing  -> OriginalUri (rpPathRaw parts)


-- ===================================================================
-- MatchedPath: the route pattern that matched
-- ===================================================================

-- | The route pattern that matched this request (e.g. @\/users\/{capture}@).
-- Stored in Extensions by the router.
--
-- @
-- handler :: MatchedPath -> IO Text
-- handler (MatchedPath pat) = ...  -- e.g. "/users/{capture}"
-- @
newtype MatchedPath = MatchedPath { unMatchedPath :: Text }
  deriving (Show, Eq, Typeable)

instance FromRequestParts MatchedPath where
  fromRequestParts parts = do
    mPath <- lookupExtension @MatchedPath (rpExtensions parts)
    pure $ case mPath of
      Just mp -> Right mp
      Nothing -> Left (mkError status500 "MatchedPath not found in extensions (router bug)")


-- ===================================================================
-- NestedPath: nesting context for sub-routers
-- ===================================================================

-- | The path prefix consumed by parent routers when using sub-API
-- composition. Useful for building relative URLs.
--
-- @
-- handler :: NestedPath -> IO Text
-- handler (NestedPath prefix) = ...
-- @
newtype NestedPath = NestedPath { unNestedPath :: [Text] }
  deriving (Show, Eq, Typeable)

instance FromRequestParts NestedPath where
  fromRequestParts parts = do
    mNested <- lookupExtension @NestedPath (rpExtensions parts)
    pure $ Right $ case mNested of
      Just np -> np
      Nothing -> NestedPath []


-- ===================================================================
-- ConnectInfo: client connection information
-- ===================================================================

-- | Client connection metadata (IP address, port).
-- Stored in Extensions by the backend adapter (tower-server, tower-wai).
--
-- @
-- handler :: ConnectInfo -> IO Text
-- handler (ConnectInfo host port) = ...
-- @
data ConnectInfo = ConnectInfo
  { ciHost :: !Text
  , ciPort :: !Int
  } deriving (Show, Eq, Typeable)

instance FromRequestParts ConnectInfo where
  fromRequestParts parts = do
    mInfo <- lookupExtension @ConnectInfo (rpExtensions parts)
    pure $ case mInfo of
      Just ci -> Right ci
      Nothing -> Left (mkError status500
        "ConnectInfo not available (backend adapter must inject it)")


-- ===================================================================
-- Form: url-encoded form body
-- ===================================================================

-- | A form-decoded request body (@application\/x-www-form-urlencoded@).
--
-- @
-- data LoginForm = LoginForm { lfUser :: Text, lfPass :: Text }
--
-- handler :: Form LoginForm -> IO (Json Session)
-- handler (Form form) = ...
-- @
--
-- The inner type must have a 'FromForm' instance (provided by the user)
-- which converts @[(ByteString, ByteString)]@ to the desired type.
newtype Form a = Form { unForm :: a }
  deriving (Show, Eq)

-- | Convert parsed form key-value pairs into a typed value.
class FromForm a where
  fromForm :: [(ByteString, ByteString)] -> Either Text a

instance FromForm a => FromRequest (Form a) where
  fromRequest _parts body =
    let pairs = parseFormUrlEncoded body
    in pure $ case fromForm pairs of
      Right val -> Right (Form val)
      Left err  -> Left (mkError status422 ("Form parse error: " <> err))

instance FromForm a => FromRequestParts (Form a) where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    pure $ case mBody of
      Just (BodyBytes body) ->
        case fromForm (parseFormUrlEncoded body) of
          Right val -> Right (Form val)
          Left err  -> Left (mkError status422 ("Form parse error: " <> err))
      Nothing -> Left (mkError status500 "Request body not available (router bug)")

-- | Parse @application\/x-www-form-urlencoded@ bytes into key-value pairs.
--
-- Uses 'Network.HTTP.Types.urlDecode' for proper percent-decoding
-- (handles @%XX@ encoding and @+@ as space).
parseFormUrlEncoded :: ByteString -> [(ByteString, ByteString)]
parseFormUrlEncoded bs
  | BS.null bs = []
  | otherwise  = map parsePair (BS.split 0x26 bs)  -- 0x26 = '&'
  where
    parsePair p =
      let (k, rest) = BS.break (== 0x3D) p  -- 0x3D = '='
      in (urlDecode True k, urlDecode True (BS.drop 1 rest))


-- ===================================================================
-- RawForm: unparsed form key-value pairs
-- ===================================================================

-- | Raw form data as key-value pairs without typed parsing.
-- For @application\/x-www-form-urlencoded@ bodies.
--
-- @
-- handler :: RawForm -> IO Text
-- handler (RawForm pairs) = ...
-- @
newtype RawForm = RawForm { unRawForm :: [(ByteString, ByteString)] }
  deriving (Show, Eq)

instance FromRequest RawForm where
  fromRequest _parts body = pure (Right (RawForm (parseFormUrlEncoded body)))

instance FromRequestParts RawForm where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    pure $ case mBody of
      Just (BodyBytes body) -> Right (RawForm (parseFormUrlEncoded body))
      Nothing -> Left (mkError status500 "Request body not available (router bug)")


-- ===================================================================
-- Multipart: file uploads (multipart/form-data)
-- ===================================================================

-- | A parsed multipart form body, containing fields and file uploads.
--
-- @
-- handler :: Multipart -> IO (Json UploadResult)
-- handler (Multipart parts) = do
--   let files = [p | p <- parts, fpFileName p /= Nothing]
--   ...
-- @
--
-- Each part contains the field name, optional filename, content type,
-- and body bytes.
newtype Multipart = Multipart { unMultipart :: [FilePart] }
  deriving (Show, Eq)

-- | A single part of a multipart form submission.
data FilePart = FilePart
  { fpFieldName   :: !Text
  , fpFileName    :: !(Maybe Text)
  , fpContentType :: !ByteString
  , fpBody        :: !ByteString
  } deriving (Show, Eq)

instance FromRequest Multipart where
  fromRequest parts body = do
    let ct = lookup (CI.mk "content-type") (rpHeaders parts)
    pure $ case ct >>= extractBoundary of
      Just boundary -> Right (Multipart (parseMultipart boundary body))
      Nothing       -> Left (mkError status400
        "Missing or invalid Content-Type for multipart/form-data")

instance FromRequestParts Multipart where
  fromRequestParts parts = do
    mBody <- lookupExtension @BodyBytes (rpExtensions parts)
    let ct = lookup (CI.mk "content-type") (rpHeaders parts)
    pure $ case (mBody, ct >>= extractBoundary) of
      (Just (BodyBytes body), Just boundary) ->
        Right (Multipart (parseMultipart boundary body))
      (Nothing, _) ->
        Left (mkError status500 "Request body not available (router bug)")
      (_, Nothing) ->
        Left (mkError status400
          "Missing or invalid Content-Type for multipart/form-data")

-- | Extract the boundary string from a multipart Content-Type header.
extractBoundary :: ByteString -> Maybe ByteString
extractBoundary ct
  | "multipart/form-data" `BS.isPrefixOf` ct =
      let parts = BS.split 0x3B ct  -- 0x3B = ';'
      in case filter (hasBoundary . BS.dropWhile (== 0x20)) parts of
           (p : _) -> Just (BS.drop 1 (snd (BS.break (== 0x3D) (BS.dropWhile (== 0x20) p))))
           []       -> Nothing
  | otherwise = Nothing
  where
    hasBoundary s = "boundary=" `BS.isPrefixOf` s || "boundary=" `BS.isPrefixOf` BS.map toLowerW8 s
    toLowerW8 w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise              = w

-- | Maximum number of parts allowed in a multipart body.
-- Prevents excessive memory/CPU usage from adversarial inputs with
-- thousands of parts.
maxMultipartParts :: Int
maxMultipartParts = 1000

-- | Parse a multipart body given the boundary string.
-- This is a simplified parser that handles the common case.
-- Output is capped at 'maxMultipartParts' parts.
parseMultipart :: ByteString -> ByteString -> [FilePart]
parseMultipart boundary body =
  let delim = "--" <> boundary
      chunks = splitOnBoundary delim body
  in take maxMultipartParts (concatMap parsePart chunks)

-- | Split the body on boundary delimiters.
splitOnBoundary :: ByteString -> ByteString -> [ByteString]
splitOnBoundary delim body = go body
  where
    go bs
      | BS.null bs = []
      | otherwise  = case BS.breakSubstring delim bs of
          (_, rest) | BS.null rest -> []
          (_, rest) ->
            let afterDelim = BS.drop (BS.length delim) rest
                -- Skip CRLF after boundary
                afterCrlf = if "\r\n" `BS.isPrefixOf` afterDelim
                            then BS.drop 2 afterDelim
                            else if "\n" `BS.isPrefixOf` afterDelim
                            then BS.drop 1 afterDelim
                            else afterDelim
            in if "--" `BS.isPrefixOf` afterDelim
               then []  -- final boundary
               else case BS.breakSubstring delim afterCrlf of
                 (chunk, _) ->
                   let trimmed = if "\r\n" `BS.isSuffixOf` chunk
                                 then BS.take (BS.length chunk - 2) chunk
                                 else chunk
                   in trimmed : go (BS.drop (BS.length chunk) afterCrlf)

-- | Parse a single multipart part (headers + body).
parsePart :: ByteString -> [FilePart]
parsePart chunk
  | BS.null chunk = []
  | otherwise =
    let (headerSection, bodySection) = splitHeaders chunk
        headers = parsePartHeaders headerSection
        fieldName = lookupPartHeader "name" headers
        fileName  = lookupPartHeader "filename" headers
        contentType = maybe "application/octet-stream" id
                      (lookup "content-type" headers >>= id)
    in case fieldName of
      Just name -> [FilePart
        { fpFieldName   = TE.decodeUtf8Lenient name
        , fpFileName    = TE.decodeUtf8Lenient <$> fileName
        , fpContentType = contentType
        , fpBody        = bodySection
        }]
      Nothing -> []

-- | Split part into headers and body at the blank line.
splitHeaders :: ByteString -> (ByteString, ByteString)
splitHeaders bs = case BS.breakSubstring "\r\n\r\n" bs of
  (h, rest) | not (BS.null rest) -> (h, BS.drop 4 rest)
  _ -> case BS.breakSubstring "\n\n" bs of
    (h, rest) | not (BS.null rest) -> (h, BS.drop 2 rest)
    _ -> (bs, "")

-- | Parse part headers into key-value pairs.
parsePartHeaders :: ByteString -> [(ByteString, Maybe ByteString)]
parsePartHeaders bs =
  let ls = filter (not . BS.null) (BS.split 0x0A bs)  -- split on LF
  in concatMap parseOneHeader ls
  where
    parseOneHeader line =
      let cleaned = if "\r" `BS.isSuffixOf` line then BS.init line else line
          (name, rest) = BS.break (== 0x3A) cleaned  -- ':'
          val = BS.dropWhile (== 0x20) (BS.drop 1 rest)
      in if "content-disposition" `BS.isPrefixOf` BS.map toLowerW8 name
         then [("content-disposition", Just val)] ++ parseDisposition val
         else if "content-type" `BS.isPrefixOf` BS.map toLowerW8 name
         then [("content-type", Just val)]
         else [(BS.map toLowerW8 name, Just val)]

    toLowerW8 w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise              = w

-- | Parse Content-Disposition parameters (name, filename).
parseDisposition :: ByteString -> [(ByteString, Maybe ByteString)]
parseDisposition val =
  let parts = BS.split 0x3B val  -- ';'
  in concatMap parseParam (drop 1 parts)
  where
    parseParam p =
      let trimmed = BS.dropWhile (== 0x20) p
          (key, rest) = BS.break (== 0x3D) trimmed  -- '='
          rawVal = BS.drop 1 rest
          unquoted = if BS.length rawVal >= 2
                     && BS.head rawVal == 0x22  -- '"'
                     && BS.last rawVal == 0x22
                     then BS.init (BS.tail rawVal)
                     else rawVal
      in [(BS.map toLowerW8 key, Just unquoted)]

    toLowerW8 w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise              = w

-- | Lookup a parameter from parsed disposition headers.
lookupPartHeader :: ByteString -> [(ByteString, Maybe ByteString)] -> Maybe ByteString
lookupPartHeader key headers = do
  (_, mVal) <- find (\(k, _) -> k == key) headers
  mVal
  where
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs
