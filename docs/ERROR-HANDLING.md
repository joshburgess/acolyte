# Error handling patterns

servant-reimagined has no `ExceptT`, no `throwError`, and no monad
transformer stack. Errors are values. This guide shows the patterns
that replace them.

## The basics

Any type with an `IntoResponse` instance can be a handler return type.
The framework provides instances for common error patterns:

```haskell
-- These all have IntoResponse instances:
instance IntoResponse Text                    -- 200, text/plain
instance IntoResponse (Json a)                -- 200, application/json
instance IntoResponse Status                  -- status code, empty body
instance IntoResponse ServerError             -- status + JSON error body
instance IntoResponse JsonError               -- status + JSON error body
instance IntoResponse (Either e a)            -- Left -> error, Right -> success
instance IntoResponse (Status, a)             -- custom status + body
instance IntoResponse (Response ByteString)   -- full control
```

## Pattern 1: Either for fallible handlers

The most common pattern. Return `Either ErrorType SuccessType`:

```haskell
getUser :: PathCapture Int -> IO (Either ServerError (Json User))
getUser (PathCapture uid) = do
  mUser <- lookupUser uid
  pure $ case mUser of
    Nothing -> Left (mkError status404 "User not found")
    Just u  -> Right (Json u)
```

`ServerError` produces a JSON error body:
```json
{"error": {"status": 404, "message": "User not found"}}
```

## Pattern 2: Early return with IO

For handlers with multiple failure points, use standard Haskell
control flow:

```haskell
createUser :: JsonBody CreateReq -> IO (Either ServerError (Json User))
createUser (JsonBody req) = do
  -- Validate
  case validateName (crName req) of
    Left err -> pure $ Left (mkError status422 err)
    Right name -> do
      -- Check uniqueness
      exists <- userExists name
      if exists
        then pure $ Left (mkError status409 "User already exists")
        else do
          -- Create
          user <- insertUser name (crEmail req)
          pure $ Right (Json user)
```

Or with a helper to flatten the nesting:

```haskell
createUser :: JsonBody CreateReq -> IO (Either ServerError (Json User))
createUser (JsonBody req) = runEither $ do
  name  <- hoistEither $ validateName (crName req)
  _     <- checkM (not <$> userExists name)
             (mkError status409 "User already exists")
  user  <- liftIO $ insertUser name (crEmail req)
  pure (Json user)

-- Helpers (define once in your project)
newtype EitherIO e a = EitherIO { runEither :: IO (Either e a) }

instance Functor (EitherIO e) where
  fmap f (EitherIO m) = EitherIO $ fmap (fmap f) m

instance Applicative (EitherIO e) where
  pure = EitherIO . pure . Right
  EitherIO mf <*> EitherIO ma = EitherIO $ do
    ef <- mf
    case ef of
      Left e  -> pure (Left e)
      Right f -> fmap f <$> ma

instance Monad (EitherIO e) where
  EitherIO m >>= f = EitherIO $ do
    ea <- m
    case ea of
      Left e  -> pure (Left e)
      Right a -> runEither (f a)

liftIO :: IO a -> EitherIO e a
liftIO m = EitherIO (Right <$> m)

hoistEither :: Either e a -> EitherIO e a
hoistEither = EitherIO . pure

checkM :: IO Bool -> e -> EitherIO e ()
checkM cond err = EitherIO $ do
  ok <- cond
  pure $ if ok then Right () else Left err
```

This gives you early return without `ExceptT` in the handler type
signature. The handler still returns `IO (Either ServerError (Json User))`.

## Pattern 3: Custom error types

Define your own error type with an `IntoResponse` instance:

```haskell
data AppError
  = NotFound Text
  | Forbidden Text
  | ValidationError [Text]
  | Conflict Text

instance IntoResponse AppError where
  intoResponse (NotFound msg) =
    Response status404 [jsonCT] (encodeError 404 msg)
  intoResponse (Forbidden msg) =
    Response status403 [jsonCT] (encodeError 403 msg)
  intoResponse (ValidationError errs) =
    Response status422 [jsonCT] $ LBS.toStrict $ Aeson.encode $
      Aeson.object ["errors" .= errs]
  intoResponse (Conflict msg) =
    Response status409 [jsonCT] (encodeError 409 msg)

jsonCT = ("Content-Type", "application/json")

encodeError :: Int -> Text -> ByteString
encodeError code msg = LBS.toStrict $ Aeson.encode $
  Aeson.object ["error" .= Aeson.object ["status" .= code, "message" .= msg]]
```

Use it in handlers:

```haskell
getUser :: PathCapture Int -> IO (Either AppError (Json User))
getUser (PathCapture uid) = do
  mUser <- lookupUser uid
  pure $ case mUser of
    Nothing -> Left (NotFound "User not found")
    Just u  -> Right (Json u)

updateUser :: PathCapture Int -> JsonBody UpdateReq -> IO (Either AppError (Json User))
updateUser (PathCapture uid) (JsonBody req) = do
  case validateUpdate req of
    Left errs -> pure $ Left (ValidationError errs)
    Right validated -> do
      mUser <- applyUpdate uid validated
      pure $ case mUser of
        Nothing -> Left (NotFound "User not found")
        Just u  -> Right (Json u)
```

## Pattern 4: Status code tuples

For simple cases where you just need a different status:

```haskell
createUser :: JsonBody CreateReq -> IO (Status, Json User)
createUser (JsonBody req) = do
  user <- insertUser req
  pure (status201, Json user)
```

## Pattern 5: Full Response control

When you need complete control over headers and status:

```haskell
getUser :: PathCapture Int -> IO (Response ByteString)
getUser (PathCapture uid) = do
  mUser <- lookupUser uid
  pure $ case mUser of
    Nothing ->
      Response status404
        [("Content-Type", "application/json")]
        "{\"error\":\"not found\"}"
    Just u ->
      Response status200
        [ ("Content-Type", "application/json")
        , ("X-User-Version", "2")
        ]
        (LBS.toStrict (Aeson.encode u))
```

## Extractor errors

When a `FromRequestParts` extractor fails (missing query param, bad
path capture, missing header), it returns a `ServerError` which becomes
a 400-level JSON response automatically. You don't need to handle these
in your handler. The `ToHandler` machinery short-circuits before your
function is called.

```haskell
-- If ?page is missing or not an Int, the handler never runs.
-- The client gets:
-- 400 {"error": {"status": 400, "message": "Missing required query parameter: page"}}
search :: QueryParam "page" Int -> IO (Json [Result])
search (QueryParam page) = ...  -- page is guaranteed to be a valid Int here
```

To customize extractor error responses, use `Optional` and handle it
yourself:

```haskell
search :: OptionalParam "page" Int -> IO (Either AppError (Json [Result]))
search (OptionalParam mPage) = do
  let page = fromMaybe 1 mPage
  if page < 1
    then pure $ Left (ValidationError ["page must be >= 1"])
    else do
      results <- queryResults page
      pure $ Right (Json results)
```

## Composing fallible operations

Multiple database calls, each of which can fail:

```haskell
getArticleWithAuthor
  :: PathCapture Int
  -> IO (Either AppError (Json ArticleWithAuthor))
getArticleWithAuthor (PathCapture articleId) = do
  mArticle <- lookupArticle articleId
  case mArticle of
    Nothing -> pure $ Left (NotFound "Article not found")
    Just article -> do
      mAuthor <- lookupUser (articleAuthorId article)
      case mAuthor of
        Nothing -> pure $ Left (NotFound "Author not found")
        Just author -> pure $ Right $ Json $
          ArticleWithAuthor article author
```

With the `EitherIO` helper from Pattern 2:

```haskell
getArticleWithAuthor
  :: PathCapture Int
  -> IO (Either AppError (Json ArticleWithAuthor))
getArticleWithAuthor (PathCapture articleId) = runEither $ do
  article <- lookupOrFail (lookupArticle articleId) "Article not found"
  author  <- lookupOrFail (lookupUser (articleAuthorId article)) "Author not found"
  pure $ Json $ ArticleWithAuthor article author

lookupOrFail :: IO (Maybe a) -> Text -> EitherIO AppError a
lookupOrFail action msg = EitherIO $ do
  result <- action
  pure $ case result of
    Nothing -> Left (NotFound msg)
    Just a  -> Right a
```

## Summary

| Servant | servant-reimagined |
|---------|--------------------|
| `throwError err404` | `pure $ Left (mkError status404 "...")` |
| `catchError` | `case ... of Left e -> ...; Right a -> ...` |
| `Handler a` (ExceptT) | `IO (Either AppError (Json a))` |
| `ServerError` | `ServerError`, `JsonError`, or your own type |
| Middleware error handlers | `IntoResponse` instance on your error type |
| `errBody`, `errHeaders` | Build a `Response ByteString` directly |

The key insight: **errors are return values, not effects**. Your handler
type signature tells you exactly what can go wrong. The compiler ensures
every error path produces a valid HTTP response.
