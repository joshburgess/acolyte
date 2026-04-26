# Migrating from Servant

A side-by-side guide for Servant users. Every section shows the Servant
way, then the acolyte equivalent.

## API definition

**Servant:**
```haskell
type API = "users" :> Get '[JSON] [User]
      :<|> "users" :> Capture "id" Int :> Get '[JSON] User
      :<|> "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
      :<|> "health" :> Get '[PlainText] Text
```

**acolyte:**
```haskell
type UsersPath    = At "users"              -- '[ 'Lit "users" ]
type UserByIdPath = Param "users" Int       -- '[ 'Lit "users", 'Capture Int ]
type HealthPath   = At "health"             -- '[ 'Lit "health" ]

type API =
  '[ Get  UsersPath    (Json [User])
   , Get  UserByIdPath (Json User)
   , Post UsersPath    (Json CreateUser) (Json User)
   , Get  HealthPath   Text
   ]
```

What changed:

- `:<|>` tree becomes a flat promoted list `'[...]`
- Path combinators (`"users" :> Capture "id" Int :>`) become concise
  helpers: `At "users"` for literal segments, `Param "users" Int` for
  a literal + capture. Compose with `++` for multi-segment paths.
- `ReqBody '[JSON] CreateUser` becomes a parameter on `Post`: the third
  type argument is the request body
- Content type is implicit in the response wrapper (`Json`, `Text`)
- Compile time stays constant as endpoints grow (no exponential `:<|>` blowup)

## Handlers

**Servant:**
```haskell
getUsers :: Handler [User]
getUsers = liftIO $ readIORef usersRef

getUser :: Int -> Handler User
getUser uid = do
  users <- liftIO $ readIORef usersRef
  case lookup uid users of
    Just u  -> pure u
    Nothing -> throwError err404

createUser :: CreateUser -> Handler User
createUser body = liftIO $ do
  let user = User (cuName body)
  modifyIORef usersRef (user :)
  pure user
```

**acolyte:**
```haskell
getUsers :: IO (Json [User])
getUsers = Json <$> readIORef usersRef

getUser :: PathCapture Int -> IO (Either ServerError (Json User))
getUser (PathCapture uid) = do
  users <- readIORef usersRef
  pure $ case lookup uid users of
    Just u  -> Right (Json u)
    Nothing -> Left (mkError status404 "User not found")

createUser :: JsonBody CreateUser -> IO (Json User)
createUser (JsonBody body) = do
  let user = User (cuName body)
  modifyIORef usersRef (user :)
  pure (Json user)
```

What changed:

- `Handler` monad becomes plain `IO`
- No `liftIO` (you're already in `IO`)
- No `throwError` (return `Left error` instead)
- Path captures are explicit arguments (`PathCapture Int`)
- Request body is an explicit argument (`JsonBody CreateUser`)
- Return types use `Json` wrapper for JSON responses
- `Either` in the return type handles errors (both branches have
  `IntoResponse` instances)

## Wiring handlers to the API

**Servant:**
```haskell
server :: Server API
server = getUsers :<|> getUser :<|> createUser :<|> health
```

**acolyte:**
```haskell
server :: Service IO (Request ByteString) (Response ByteString)
server = mkApi @API (getUsers, getUser, createUser, health)
```

What changed:

- `:<|>` becomes a tuple
- `mkApi` matches handlers positionally to endpoints (no annotations needed)
- Wrong handler count = compile error with a clear message:
  `API has 4 endpoint(s) but 3 handler(s) were provided.`

For explicit control, the lower-level pattern is still available:

```haskell
server = mkServer @API
  ( wrapHandler @(Get UsersPath (Json [User]))    (toHandler getUsers)
  , wrapHandler @(Get UserByIdPath (Json User))   (toHandler getUser)
  , wrapHandler @(Post UsersPath (Json CreateUser) (Json User)) (toHandler createUser)
  , wrapHandler @(Get HealthPath Text)            (toHandler health)
  )
```

For large APIs, wrap endpoints with `Named` and use `mkRecordApi` to
match handlers by field name instead of position. No manual instance
needed:

```haskell
type API =
  '[ Named "getUsers"   (Get UsersPath (Json [User]))
   , Named "getUser"    (Get UserByIdPath (Json User))
   , Named "createUser" (Post UsersPath (Json CreateUser) (Json User))
   ]

-- Field order doesn't matter (matched by name via GHC.Records.HasField)
data Handlers = Handlers
  { createUser :: JsonBody CreateUser -> IO (Json User)
  , getUsers   :: IO (Json [User])
  , getUser    :: PathCapture Int -> IO (Json User)
  }

server = mkRecordApi @API Handlers { ... }
```

`Named` is transparent to routing: you can still use tuples with Named
endpoints if you prefer. It also sets `operationId` in OpenAPI specs.

For clients, `callNamed` lets you call an endpoint by its type-level
name instead of by index:

```haskell
-- Look up "getUser" in the API by name and call it:
result <- callNamed @"getUser" @API client 42
```

This uses a `LookupNamed` type family to find the endpoint in the API
list by its `Named` label, so you get a compile error if the name
doesn't exist.

The older `mkNamedApi` + manual `NamedApi` instance approach is still
available for APIs without `Named` wrappers.

## Query parameters

**Servant:**
```haskell
type API = "users" :> QueryParam "page" Int :> Get '[JSON] [User]

getUsers :: Maybe Int -> Handler [User]
getUsers mPage = ...
```

**acolyte:**
```haskell
type API = '[ Get UsersPath (Json [User]) ]

-- Required:
getUsers :: QueryParam "page" Int -> IO (Json [User])
getUsers (QueryParam page) = ...

-- Optional:
getUsers :: OptionalParam "page" Int -> IO (Json [User])
getUsers (OptionalParam mPage) = ...

-- Repeated (?tag=a&tag=b):
search :: QueryParams "tag" Text -> IO (Json [Article])
search (QueryParams tags) = ...
```

What changed:

- Query params are not in the API type. They're extractor arguments
  in the handler
- `QueryParam` is required (400 if missing). Use `OptionalParam` for
  the Servant `QueryParam` behavior (returns `Maybe`)
- `QueryParams` handles repeated params (Servant's `QueryParams`)
- For OpenAPI/client generation, you can declare params at the type level
  with `WithParams`. This is optional and doesn't affect routing. The
  OpenAPI generator produces real query parameter schemas from these
  declarations, and request/response body types are turned into JSON
  schemas via `ToSchema` constraints (with `Generic`-based automatic
  derivation for record types):
  ```haskell
  WithParams '[QP "page" Int, QP "limit" Int] (Get UsersPath (Json [User]))
  ```

## Headers

**Servant:**
```haskell
type API = "users" :> Header "Authorization" Text :> Get '[JSON] [User]

getUsers :: Maybe Text -> Handler [User]
```

**acolyte:**
```haskell
-- Required (400 if missing):
getUsers :: ReqHeader "Authorization" -> IO (Json [User])
getUsers (ReqHeader token) = ...

-- Optional:
getUsers :: Optional (ReqHeader "Authorization") -> IO (Json [User])
getUsers (Optional mHeader) = ...

-- All headers at once:
getUsers :: HeaderMap -> IO (Json [User])
getUsers (HeaderMap hdrs) = ...
```

For documentation, you can also declare headers at the type level with
`WithHeaders` (feeds into OpenAPI generation):
```haskell
WithHeaders '[HH "Authorization" Text] (Get UsersPath (Json [User]))
```

## Request body

**Servant:**
```haskell
type API = "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User

createUser :: CreateUser -> Handler User
```

**acolyte:**
```haskell
type API = '[ Post UsersPath (Json CreateUser) (Json User) ]

-- JSON body:
createUser :: JsonBody CreateUser -> IO (Json User)
createUser (JsonBody body) = ...

-- Form body:
submit :: Form LoginForm -> IO (Json Session)
submit (Form form) = ...

-- Raw bytes:
upload :: RawBody -> IO Text
upload (RawBody bytes) = ...

-- File uploads:
upload :: Multipart -> IO (Json UploadResult)
upload (Multipart parts) = ...
```

## Error handling

**Servant:**
```haskell
getUser :: Int -> Handler User
getUser uid = do
  mUser <- liftIO $ lookupUser uid
  case mUser of
    Nothing -> throwError err404 { errBody = "not found" }
    Just u  -> pure u
```

**acolyte:**
```haskell
-- Option 1: Either in the return type
getUser :: PathCapture Int -> IO (Either ServerError (Json User))
getUser (PathCapture uid) = do
  mUser <- lookupUser uid
  pure $ case mUser of
    Nothing -> Left (mkError status404 "not found")
    Just u  -> Right (Json u)

-- Option 2: Return a structured JSON error
getUser :: PathCapture Int -> IO (Either JsonError (Json User))
getUser (PathCapture uid) = do
  mUser <- lookupUser uid
  pure $ case mUser of
    Nothing -> Left (jsonError status404 "not found")
    Just u  -> Right (Json u)

-- Option 3: Return Response directly for full control
getUser :: PathCapture Int -> IO (Response ByteString)
getUser (PathCapture uid) = do
  mUser <- lookupUser uid
  pure $ case mUser of
    Nothing -> Response status404 [("Content-Type","application/json")] "{}"
    Just u  -> intoResponse (Json u)
```

No `ExceptT`. No `throwError`. No `catchError`. The return type is the
error handling mechanism.

## State and dependencies

**Servant:**
```haskell
type AppM = ReaderT Config Handler

server :: ServerT API AppM
server = getUsers :<|> getUser

app :: Config -> Application
app cfg = serve api $ hoistServer api (nt cfg) server
  where nt cfg m = runReaderT m cfg
```

**acolyte:**
```haskell
-- State via AppState extractor
getUsers :: AppState Config -> IO (Json [User])
getUsers (AppState cfg) = do
  users <- readIORef (cfgUsersRef cfg)
  pure (Json users)

-- Inject state at server construction
main = do
  cfg <- loadConfig
  let svc = serveWithState cfg router
  runServerBS 3000 svc
```

No `ReaderT`. No `hoistServer`. State is an extractor: it comes as a
handler argument, not a monad layer.

## Middleware

**Servant:**
```haskell
-- WAI middleware (untyped, no compile-time guarantees)
app :: Application
app = cors defaultCors
    $ logRequests
    $ serve api server
```

**acolyte:**
```haskell
-- spire middleware (typed, composable with |>)
main = do
  ridLayer <- requestIdLayer
  let svc = server
        |> ridLayer
        |> traceLayer traceFn
        |> corsLayer permissiveCors
        |> secureHeadersLayer defaultSecureHeaders
  runServerBS 3000 svc
```

What changed:

- `|>` instead of function composition
- Layers compose inside-out (first `|>` is closest to the handler)
- Each middleware is a `Layer`: a function `Service -> Service`
- `before`, `after`, `around` combinators for writing middleware
- WAI middleware still works via `fromWaiMiddleware` bridge in `spire-wai`

## Typed middleware effects

This has no Servant equivalent.

**acolyte:**
```haskell
type API =
  '[ Requires Auth (Get UserPath (Json User))     -- needs auth
   , Requires Auth (Post UsersPath (Json CreateUser) (Json User))
   , Get HealthPath Text                            -- public
   ]

app = run
    $ provide @Auth authMiddleware
    $ effectfulApi @API (getUser, createUser, health)
```

Delete the `provide @Auth` line and you get:
```
Missing middleware effect: Auth
This effect is required by an endpoint but was not provided.
Add .provide @Auth to the server builder.
```

Servant has no way to enforce at compile time that auth middleware is
present for endpoints that need it.

## Testing

**Servant:**
```haskell
-- Requires warp, a port, and network
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec = with app $ do
  it "GET /health returns 200" $
    get "/health" `shouldRespondWith` 200
```

**acolyte:**
```haskell
-- Direct dispatch (no network, no port, no warp)
import Acolyte.Test

tests :: IO ()
tests = do
  let svc = mkServer @API handlers
  resp <- get svc "/health"
  resp `shouldHaveStatus` 200
  resp `shouldHaveBody` "ok"

  resp2 <- post svc "/users" (CreateUser "alice")
  resp2 `shouldHaveStatus` 200
  resp2 `shouldHaveJsonBody` expectedUser
```

What changed:

- No test framework dependency required (works with any)
- Requests dispatched directly through the spire Service
- No network, no port, no warp. Fast and deterministic
- Same code path as production, minus TCP

## Running the server

**Servant:**
```haskell
main :: IO ()
main = Warp.run 3000 app
```

**acolyte:**
```haskell
-- Zero WAI (spire-server):
main = runServerBS 3000 svc

-- On warp (spire-wai):
main = runWarp 3000 svc
```

The server produces a `Service`. The backend consumes it. Swap
`runServerBS` for `runWarp`, nothing else changes.

## Sub-API composition

**Servant:**
```haskell
type API = UsersAPI :<|> ArticlesAPI :<|> TagsAPI
```

**acolyte:**
```haskell
type FullAPI = UsersAPI ++ ArticlesAPI ++ TagsAPI

service = runCombined @FullAPI
  $ provideEffect @Auth authMw
  $ combinedFromRouter @FullAPI
  $ subRouter @TagsAPI tagHandlers
  $ subRouter @ArticlesAPI articleHandlers
  $ subRouter @UsersAPI userHandlers
  $ emptyRouter
```

Each sub-API compiles independently in constant time. Effects are
checked across the combined type.

## Compile times

| Endpoints | Servant  | acolyte |
|-----------|----------|--------------------|
| 1         | ~0.5s    | ~1.3s              |
| 4         | ~1.5s    | ~1.3s              |
| 8         | ~4s      | ~1.3s              |
| 16        | ~15s     | ~1.3s              |
| 32        | ~60s+    | ~1.3s              |

Servant compile time grows exponentially with endpoint count due to
recursive `:<|>` constraint solving. acolyte stays constant
because APIs are flat lists with closed type families and direct tuple
indexing.

Run `bash bench/compile-time/run-bench.sh` to verify on your machine.

## Cheat sheet

| Servant | acolyte |
|---------|--------------------|
| `:<|>` | `'[endpoint1, endpoint2, ...]` |
| `"path" :>` | `At "path"` (or `'Lit "path"`) |
| `Capture "id" Int :>` | `Param "path" Int` (or `'Capture Int`) |
| `ReqBody '[JSON] a :>` | `JsonBody a` (handler argument) |
| `QueryParam "p" Int :>` | `QueryParam "p" Int` (handler argument) |
| `Header "h" Text :>` | `ReqHeader "h"` (handler argument) |
| `Get '[JSON] a` | `Get path (Json a)` |
| `Post '[JSON] a b` | `Post path (Json a) (Json b)` |
| `Handler a` | `IO a` |
| `liftIO` | (not needed) |
| `throwError err404` | `pure (Left (mkError status404 "..."))` |
| `ReaderT Config Handler` | `AppState Config` (handler argument) |
| `serve (Proxy @API) server` | `mkApi @API (h1, h2, ...)` |
| `Warp.run 3000 app` | `runServerBS 3000 svc` or `runWarp 3000 svc` |
| WAI middleware | `spire` layers with `\|>` |
| (no equivalent) | `Requires Auth` + `provide @Auth` |
| `hspec-wai` | `Acolyte.Test` (no network) |
| `Summary "..."` | `Describe "..."` (endpoint description) |
| `QueryParam "p" Int :>` (in type) | `WithParams '[QP "p" Int]` (type-level annotation) |
| `Header "h" Text :>` (in type) | `WithHeaders '[HH "h" Text]` (type-level annotation) |
| `PostCreated '[JSON] a` | `PostCreated path req resp` (201 status) |
| `DeleteNoContent` | `DeleteNoContent path` (204 status) |
| `StreamBody ...` | `ServerStream`, `ClientStream`, `BidiStream` |
| (no equivalent) | `Versioned V1 (Get ...)` (routes to `/v1/...`) |
| (no equivalent) | `callNamed @"name" @API client args` (named client calls) |
| `server = h1 :<\|> h2` | `mkRecordApi @API record` (named routes) |
