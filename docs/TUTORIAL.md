# Building a web API with acolyte

This walkthrough builds a small bookmarks API from scratch. By the end
you'll have typed endpoints, JSON handlers, middleware, effect tracking,
and tests, all without WAI, without monad transformers, and with
compile-time guarantees that everything fits together.

## Setup

Make sure you have GHC 9.10.3 and cabal-install. If you're adding this
to an existing project, depend on the `acolyte` facade
package (it re-exports everything). For this tutorial we'll use the
individual packages to see where each piece comes from.

```cabal
build-depends:
    acolyte-core
  , acolyte-server
  , tower
  , tower-http
  , tower-server
  , http-core
  , aeson
  , bytestring
  , text
  , http-types
```

Enable these extensions (or put them in `default-extensions`):

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
```

## Step 1: Define the API type

The API is a type-level list of endpoints. Each endpoint specifies an
HTTP method, a path (as a type-level list of segments), and request/response
types.

```haskell
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Network.HTTP.Types (status400, status404, status500)

import Acolyte.Core
import Acolyte.Server
import Tower
import Tower.Service (Service (..))

-- Path types: use At and Param helpers for concise definitions
type HealthPath    = At "health"
type BookmarksPath = At "bookmarks"
type BookmarkPath  = Param "bookmarks" Int

-- The API: a promoted list of endpoints
type BookmarkAPI =
  '[ Get  HealthPath    Text              -- GET /health
   , Get  BookmarksPath (Json [Text])     -- GET /bookmarks
   , Get  BookmarkPath  (Json Text)       -- GET /bookmarks/:id
   , Post BookmarksPath (Json Text) (Json Text) -- POST /bookmarks
   ]
```

A few things to notice:

- Paths are type-level lists of `PathSegment`. `At "health"` expands to
  `'[ 'Lit "health" ]`. `Param "bookmarks" Int` expands to
  `'[ 'Lit "bookmarks", 'Capture Int ]`. You can also write paths
  manually if you need more control.
- `Json a` means the endpoint accepts/returns JSON-encoded `a`.
- `Post path reqBody respBody` has both a request body type and
  a response body type.
- There is no `:<|>` operator. The list is flat. This is what keeps
  compile times constant as endpoints grow.
- Paths compose with `++`: `At "api" ++ Param "users" Int` gives
  `'[ 'Lit "api", 'Lit "users", 'Capture Int ]`.

## Step 2: Write handlers

Handlers are plain `HandlerFn` functions:

```haskell
type HandlerFn = RequestParts -> ByteString -> IO (Response ByteString)
```

They receive the non-body parts of the request (method, path, headers,
extensions) and the raw body. They return an `IO (Response ByteString)`.

No `ExceptT`. No `liftIO`. Just IO.

```haskell
healthHandler :: HandlerFn
healthHandler _parts _body = pure $ intoResponse ("ok" :: Text)

listBookmarksHandler :: HandlerFn
listBookmarksHandler _parts _body =
  pure $ intoResponse (Json
    [ "https://haskell.org"
    , "https://hackage.haskell.org"
    ] :: Json [Text])

getBookmarkHandler :: HandlerFn
getBookmarkHandler parts _body = do
  mCaps <- lookupExtension @CaptureList (rpExtensions parts)
  case mCaps of
    Just (CaptureList (idText : _)) ->
      case parseCapture @Int idText of
        Just n  -> pure $ intoResponse (Json (T.pack ("bookmark-" ++ show n)))
        Nothing -> pure $ intoResponse (mkError status400 "invalid ID")
    _ -> pure $ intoResponse (mkError status500 "missing capture")

createBookmarkHandler :: HandlerFn
createBookmarkHandler _parts body =
  -- In a real app, you'd parse the body and persist it
  pure $ intoResponse (Json ("created" :: Text))
```

`intoResponse` converts any type with an `IntoResponse` instance into
an HTTP response. `Text` becomes `200 text/plain`. `Json a` becomes
`200 application/json`. `ServerError` becomes a JSON error with the
right status code. `Either e a` dispatches to the error or success
response.

## Step 3: Wire handlers to the API

`mkApi` connects your handlers to the API type. Pass handler functions
positionally. No `wrapHandler`, no `toHandler`, no type annotations.
The compiler checks at compile time that:

1. You have the right number of handlers (one per endpoint)
2. Each handler's type matches its endpoint

```haskell
import Http.Core

server :: Service IO (Request ByteString) (Response ByteString)
server = mkApi @BookmarkAPI
  ( healthHandler
  , listBookmarksHandler
  , getBookmarkHandler
  , createBookmarkHandler
  )
```

If you add an endpoint to the API but forget a handler, you get a type
error, not a runtime 404.

For cases where you need explicit control, the lower-level
`mkServer` + `wrapHandler @EndpointType (toHandler fn)` pattern is
still available.

## Step 4: Add middleware

Middleware uses the tower `|>` operator. Layers compose inside-out:
the first `|>` is closest to the handler.

```haskell
import Tower.Http

app :: IO (Service IO (Request ByteString) (Response ByteString))
app = do
  ridLayer <- requestIdLayer
  let traceFn entry = putStrLn $
        show (traceMethod entry) ++ " " ++
        show (tracePath entry) ++ " -> " ++
        show (traceStatus entry)

  pure $ server
    |> ridLayer                                 -- assign X-Request-Id
    |> traceLayer traceFn                       -- log method + path + status
    |> corsLayer permissiveCors                 -- CORS headers
    |> secureHeadersLayer defaultSecureHeaders  -- OWASP security headers
```

Each middleware is a `Layer`: a function that wraps a `Service`.
You can write your own:

```haskell
import Tower (before)

myLogging :: Middleware IO (Request ByteString) (Response ByteString)
myLogging = before $ \req ->
  putStrLn $ "Incoming: " ++ show (requestMethod req)
```

## Step 5: Run the server

Pick a backend. `tower-server` is zero-WAI:

```haskell
import Tower.Server (runServerBS)

main :: IO ()
main = do
  svc <- app
  putStrLn "Listening on http://localhost:3000"
  runServerBS 3000 svc
```

Or use warp via `tower-wai`:

```haskell
import Tower.Wai (runWarp)

main :: IO ()
main = do
  svc <- app
  putStrLn "Listening on http://localhost:3000"
  runWarp 3000 svc
```

The server code doesn't change. Only the import and the run function.
That's the pluggable backend at work.

## Step 6: Add typed effect tracking

Say some endpoints need authentication. Instead of hoping you remembered
to add auth middleware, declare it in the type:

```haskell
type SecureAPI =
  '[ Get  HealthPath    Text                             -- public
   , Requires Auth (Get BookmarksPath (Json [Text]))     -- needs auth
   , Requires Auth (Post BookmarksPath (Json Text) (Json Text)) -- needs auth
   , Get  BookmarkPath  (Json Text)                      -- public
   ]
```

Now build the server with `effectfulApi` instead of `mkApi`:

```haskell
apiService :: Service IO (Request ByteString) (Response ByteString)
apiService = run
  $ provide @Auth authMiddleware
  $ effectfulApi @SecureAPI
      ( healthHandler
      , listBookmarksHandler
      , createBookmarkHandler
      , getBookmarkHandler
      )

authMiddleware :: Middleware IO (Request ByteString) (Response ByteString)
authMiddleware = before $ \req -> do
  -- In production: verify JWT, check session, etc.
  pure ()
```

The flow is:

1. `effectfulServer` creates a builder with `provided = '[]`
2. Each `provide @Effect mw` adds the effect to the provided list and
   applies the middleware
3. `run` finalizes, but only compiles if every `Requires` in the API
   has a matching `provide`

Delete the `provide @Auth` line and you'll see:

```
Missing middleware effect: Auth
This effect is required by an endpoint but was not provided.
```

## Step 7: Test without a network

`acolyte-test` lets you dispatch requests directly through
the service. No ports, no sockets, no flaky network tests.

```haskell
import Acolyte.Test

tests :: IO ()
tests = do
  svc <- app

  -- GET /health -> 200, "ok"
  resp <- get svc "/health"
  resp `shouldHaveStatus` 200
  resp `shouldHaveBody` "ok"

  -- GET /bookmarks -> 200, JSON
  resp2 <- get svc "/bookmarks"
  resp2 `shouldHaveStatus` 200

  -- GET /bookmarks/42 -> 200
  resp3 <- get svc "/bookmarks/42"
  resp3 `shouldHaveStatus` 200

  -- GET /nope -> 404
  resp4 <- get svc "/nope"
  resp4 `shouldHaveStatus` 404

  -- POST /bookmarks
  resp5 <- post svc "/bookmarks" ("https://example.com" :: Text)
  resp5 `shouldHaveStatus` 200

  putStrLn "All tests passed."
```

The `request` function builds a full `Request` and calls `runService`
on the tower `Service` directly. Same code path as production, minus
the TCP.

## Step 8: Annotating endpoints for documentation

Endpoint wrappers add metadata without changing routing or handler
signatures. They feed into OpenAPI generation, client generation, and
`.proto` output.

```haskell
type API =
  '[ -- Describe adds a human-readable summary (appears in OpenAPI)
     Describe "List all bookmarks, with pagination"
       -- WithParams declares query parameters at the type level
       (WithParams '[QP "page" Int, QP "limit" Int]
         (Get BookmarksPath (Json [Text])))

     -- WithHeaders documents required headers
   , WithHeaders '[HH "Authorization" Text]
       (Get BookmarkPath (Json Text))

     -- PostCreated = RespondsWith 201 (Post ...). Use for creation endpoints.
   , PostCreated BookmarksPath (Json Text) (Json Text)

     -- DeleteNoContent = RespondsWith 204 (Delete ... NoBody). Use for deletes.
   , DeleteNoContent BookmarkPath
   ]
```

These wrappers are transparent to routing -- the router unwraps them
and dispatches as usual. Your handlers don't change. The metadata is
consumed by:

- **OpenAPI generation:** `Describe` sets `opSummary` in the spec,
  `WithParams` populates query parameter schemas, `WithHeaders`
  populates header parameter schemas, `RespondsWith` sets the response
  status code, and request/response body types are turned into real
  JSON schemas via `ToSchema` constraints. Custom record types get
  automatic `ToSchema` instances through `Generic`-based derivation --
  just add `deriving Generic` to your data types and `genericToSchema`
  walks the `GHC.Generics` `Rep` to extract field names and types.
- **Client generation:** `WithParams` and `WithHeaders` produce typed
  helper arguments.
- **`.proto` generation:** streaming markers control the RPC shape.

### Versioned endpoints

Use `Versioned` to prefix routes with a version segment. The version
is part of the type, so the server, client, and OpenAPI spec all agree
on the URL shape:

```haskell
type API =
  '[ Versioned V1 (Get UsersPath (Json [User]))     -- GET /v1/users
   , Versioned V2 (Get UsersPath (Json [UserV2]))   -- GET /v2/users
   ]
```

`Versioned` is transparent to handlers -- the router prepends the
version prefix automatically. The OpenAPI spec includes the versioned
path.

### Generic ToSchema derivation

If your response or request types are records with `deriving Generic`,
you get `ToSchema` instances for free:

```haskell
data Bookmark = Bookmark
  { bookmarkId  :: Int
  , bookmarkUrl :: Text
  , bookmarkTag :: Maybe Text
  } deriving (Generic)

-- genericToSchema walks GHC.Generics Rep to produce a JSON Schema
-- with properties "bookmarkId", "bookmarkUrl", "bookmarkTag".
instance ToSchema Bookmark where
  toSchema = genericToSchema
```

This populates request and response body schemas in the generated
OpenAPI spec.

For gRPC streaming, mark endpoints with `ServerStream`, `ClientStream`,
or `BidiStream`:

```haskell
type StreamAPI =
  '[ ServerStream (Get (At "events") (Json Event))       -- server pushes events
   , ClientStream (Post (At "upload") (Json Chunk) (Json Result))  -- client streams
   , BidiStream (Post (At "chat") (Json Msg) (Json Msg))          -- both sides stream
   ]
```

### Named endpoints and record-based handlers

For larger APIs, positional tuple matching becomes fragile: swap two
handlers and you get confusing type errors instead of a clear mismatch.
Wrap endpoints with `Named` and use `mkRecordApi` to match handlers by
field name instead of position:

```haskell
type API =
  '[ Named "listBookmarks" (Get BookmarksPath (Json [Text]))
   , Named "getBookmark"   (Get BookmarkPath  (Json Text))
   , Named "createBookmark" (Post BookmarksPath (Json Text) (Json Text))
   ]

-- Field order doesn't matter (names are matched automatically)
data Handlers = Handlers
  { createBookmark :: JsonBody Text -> IO (Json Text)
  , listBookmarks  :: IO (Json [Text])
  , getBookmark    :: PathCapture Int -> IO (Json Text)
  }

server = mkRecordApi @API Handlers { ... }
```

`Named` is transparent: it works exactly like `Describe` for routing,
so you can still use positional tuples if you prefer:

```haskell
-- Tuples still work with Named endpoints (names are ignored):
server = mkApi @API (listHandler, getHandler, createHandler)
```

`Named` also feeds into OpenAPI generation as `operationId`, giving
your generated specs stable, human-readable operation identifiers.

For a manual approach without `Named` wrappers, the older `NamedApi`
class + `mkNamedApi` pattern is still available. See
[`examples/crud`](../examples/crud/) for an example.

On the client side, `mkClientRecord` constructs a typed client record
from `Named` APIs via `Generic`. Define a record type whose fields
match the endpoint names, and `mkClientRecord` fills in the typed
client functions automatically:

```haskell
import Acolyte.Client (mkClientRecord, ClientConfig)

data MyClient = MyClient
  { listBookmarks  :: IO (Json [Text])
  , getBookmark    :: Int -> IO (Json Text)
  , createBookmark :: Json Text -> IO (Json Text)
  }

client :: ClientConfig -> MyClient
client = mkClientRecord @API
```

## Request validation with ValidatedBody

For endpoints that accept user input, you often need validation beyond
what JSON parsing provides. `ValidatedBody v a` deserializes the JSON
body and then runs a `Validate v a` check before the handler sees the
data. Validation failures return 422 with a structured error message.

```haskell
-- 1. Define a validator tag type
data CreateUserValidator

-- 2. Implement Validate
instance Validate CreateUserValidator CreateUser where
  validate u
    | T.null (userName u) = Left "name is required"
    | userAge u < 0       = Left "age must be non-negative"
    | otherwise           = Right u

-- 3. Use ValidatedBody in the handler signature
createUser :: ValidatedBody CreateUserValidator CreateUser -> IO (Json User)
createUser (ValidatedBody user) = do
  -- 'user' is guaranteed to have passed validation
  saved <- saveUser user
  pure (Json saved)
```

The handler never sees invalid data: the framework rejects it before
dispatch. This keeps validation logic separate from business logic and
ensures it is applied consistently.

See [`examples/crud`](../examples/crud/) for a complete example using
`ValidatedBody` with named routes.

## What's happening under the hood

Here's the full picture of how a request flows:

```
HTTP bytes on the wire
        |
   tower-server (or tower-wai)    -- parse HTTP, build Request
        |
   tower middleware stack          -- security headers, CORS, tracing, etc.
        |
   acolyte-server       -- match route, extract captures, dispatch
        |
   your HandlerFn                  -- pure IO, returns a response type
        |
   IntoResponse                    -- convert to Response ByteString
        |
   tower middleware stack          -- post-processing (compression, headers)
        |
   tower-server (or tower-wai)    -- render HTTP, send bytes
```

Every boundary is a tower `Service`. The server produces one, the
backend consumes one, and middleware wraps one. That's the entire
composition model.

## Scaling up: sub-API composition

When your API grows past 25 endpoints, split it into sub-APIs and
combine them. Each sub-API compiles independently in constant time:

```haskell
type UsersAPI = '[ ... ]
type PostsAPI = '[ ... ]
type AdminAPI = '[ ... ]

type FullAPI = UsersAPI ++ PostsAPI ++ AdminAPI

service = runCombined @FullAPI
  $ provideEffect @Auth authMw
  $ combinedFromRouter @FullAPI
  $ subRouter @AdminAPI adminHandlers
  $ subRouter @PostsAPI postsHandlers
  $ subRouter @UsersAPI usersHandlers
  $ emptyRouter
```

See [`examples/realworld-combined`](../examples/realworld-combined/)
for a full 15-endpoint example using this pattern.

## Where to go from here

- **OpenAPI generation:** `acolyte-openapi` generates
  OpenAPI 3.1 or Swagger 2.0 specs from your API type. Same type,
  zero drift.
- **Type-safe client:** `acolyte-client` derives an HTTP
  client from the same API type your server uses.
- **Code generation:** `acolyte-codegen` generates API
  types from existing OpenAPI/Swagger specs.
- **The architecture:** [`ARCHITECTURE.md`](../ARCHITECTURE.md) covers
  the full package dependency graph, the two-layer middleware model,
  and design decisions.

## Further examples

The `examples/` directory contains 12 complete applications:

- `minimal/` -- simplest possible server (1 endpoint)
- `hello-world/` -- 3 endpoints, effect tracking, middleware stack
- `crud/` -- full CRUD with named routes, validated bodies, and structured errors
- `auth/` -- custom authentication extractors
- `custom-extractors/` -- writing your own request extractors
- `grpc-demo/` -- gRPC server with .proto generation
- `chat/` -- session-typed WebSocket chat
- `negotiate/` -- content negotiation (JSON, XML, plain text)
- `versioned-api/` -- API versioning with typed version headers
- `realworld/` -- RealWorld spec API types split into 6 sub-APIs
- `realworld-combined/` -- full RealWorld backend with 15 endpoints, in-memory store, combined effect tracking
- `streaming/` -- Server-Sent Events with async streaming
