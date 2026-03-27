# servant-reimagined

[![CI](https://github.com/joshburgess/servant-reimagined/actions/workflows/ci.yml/badge.svg)](https://github.com/joshburgess/servant-reimagined/actions/workflows/ci.yml)

A composable, type-safe web framework for Haskell. Your API is a type,
your middleware is tracked at compile time, and your backend is pluggable.

Built on GHC 9.10.3. Informed by Rust's tower/axum/typeway ecosystem.

## Prerequisites

- [GHC 9.10.3](https://www.haskell.org/ghcup/) (install via ghcup)
- [cabal-install](https://www.haskell.org/cabal/) >= 3.10

```sh
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3
```

## Quick start

Clone and build:

```sh
git clone <repo-url> servant-reimagined
cd servant-reimagined
cabal build all        # ~7s clean build
cabal test all         # 40 test suites, 612+ assertions, 43 hedgehog properties
```

Run the hello-world example:

```sh
cabal run hello-world
```

In another terminal:

```sh
curl http://localhost:3000/health        # -> ok
curl http://localhost:3000/users         # -> ["alice","bob","charlie"]
curl http://localhost:3000/users/42      # -> "user-42"
```

## Your first API in 4 steps

### 1. Define the API as a type

Endpoints are a promoted list. Paths use type-level strings. Captures
are typed.

```haskell
{-# LANGUAGE DataKinds #-}

import Servant.Reimagined.Core
import Servant.Reimagined.Server

type HealthPath = At "health"           -- expands to '[ 'Lit "health" ]
type UsersPath  = At "users"            -- expands to '[ 'Lit "users" ]

type API =
  '[ Get HealthPath   Text              -- GET /health -> Text
   , Get UsersPath    (Json [Text])     -- GET /users  -> JSON array
   ]
```

### 2. Write handlers

Handlers are plain IO functions. The type signature *is* the extraction
— arguments are automatically pulled from the request.

```haskell
healthHandler :: IO Text
healthHandler = pure "ok"

listUsersHandler :: IO (Json [Text])
listUsersHandler = pure (Json ["alice", "bob"])

-- Path captures are typed and extracted automatically:
getUser :: PathCapture Int -> IO (Json Text)
getUser (PathCapture uid) = pure (Json (T.pack ("user-" ++ show uid)))
```

No monad transformers, no `liftIO`, no manual extraction boilerplate.

### 3. Wire handlers to the API

`mkApi` checks at compile time that you have the right number of
handlers and that each one matches its endpoint type. No `wrapHandler`,
no `toHandler` — just pass your functions positionally.

```haskell
import Tower (Service, (|>))
import Tower.Http (secureHeadersLayer, defaultSecureHeaders)

server :: Service IO (Request ByteString) (Response ByteString)
server = mkApi @API (healthHandler, listUsersHandler)
```

For larger APIs, wrap endpoints with `Named` and use `mkRecordApi` to
match handlers by field name instead of position:

```haskell
type NamedAPI =
  '[ Named "health"    (Get HealthPath   Text)
   , Named "listUsers" (Get UsersPath    (Json [Text]))
   ]

data Handlers = Handlers
  { listUsers :: IO (Json [Text])  -- order doesn't matter
  , health    :: IO Text
  }

server = mkRecordApi @NamedAPI Handlers { ... }
```

### 4. Add middleware and run

Middleware composes with `|>`. Pick a backend — `tower-server` (zero
WAI) or `tower-wai` (warp).

```haskell
import Tower.Server (runServerBS)

main :: IO ()
main = do
  let app = server |> secureHeadersLayer defaultSecureHeaders
  runServerBS 3000 app
```

That's it. You have a running server with OWASP security headers,
compile-time route checking, and no WAI dependency.

## Typed middleware effects

Endpoints can declare what middleware they require. The compiler
enforces that you provide it.

```haskell
type EffectAPI =
  '[ Requires Auth (Get UserPath (Json User))  -- needs auth
   , Get HealthPath Text                        -- no requirements
   ]

app = run
    $ provide @Auth authMiddleware
    $ effectfulServer @EffectAPI (authHandler, healthHandler)
```

Forget `provide @Auth`? You get a compile error:

```
Missing middleware effect: Auth
This effect is required by an endpoint but was not provided.
```

## Testing without a network

`servant-reimagined-test` dispatches requests directly through the
tower Service — no ports, no sockets, deterministic.

```haskell
import Servant.Reimagined.Test

test :: IO ()
test = do
  let svc = mkServer @API (health, users)
  resp <- get svc "/health"
  resp `shouldHaveStatus` 200
  resp `shouldHaveBody` "ok"
```

## WebSocket session types

WebSocket protocols are enforced at compile time via phantom-typed
session handles. Each operation (send, recv, offer, select) transitions
the type-level state, so the compiler rejects out-of-order messages.

```haskell
import Tower.WebSocket
import Servant.Reimagined.Core.Session (SessionType (..))

type EchoProtocol = 'Send Text ('Recv Text 'End)

echoHandler :: Session EchoProtocol -> IO ()
echoHandler session = do
  session'         <- send ("hello" :: Text) session
  (msg, session'') <- recv session'
  close session''
```

The protocol type drives correctness: calling `recv` when the protocol
expects `send` is a type error. Branching protocols use `Offer` / `Select`,
and recursive protocols use `Rec` / `Var` with `recurse` / `loop`.

## gRPC from the same API type

The same API type drives REST and gRPC:

```haskell
-- REST server:
restSvc = mkServer @API restHandlers

-- gRPC server (same API type!):
grpcSvc = grpcServer (mkGrpcServiceMap @API "pkg" "Svc" grpcHandlers)

-- Multiplex REST + gRPC on a single port:
combined = multiplexServices restSvc grpcSvc

-- .proto file with full message definitions:
proto = generateProto @API "pkg" "Svc"

-- Or go the other way — .proto → API types:
-- cabal run proto-codegen -- service.proto
```

Run on HTTP/2 via tower-server (zero WAI):

```haskell
main = runServerH2 (defaultH2Config 50051) grpcSvc
```

Features: REST+gRPC multiplexing (`Tower.Grpc.Multiplex`), content
negotiation (`Servant.Reimagined.Server.Negotiate`), server reflection
(`Tower.Grpc.Reflection`), bidirectional `.proto` codegen, client
streaming and bidirectional streaming handlers, gRPC health check
service (`Tower.Grpc.Health`), and gzip compression
(`Tower.Grpc.Compression`).

See the [gRPC guide](docs/GRPC.md) for the full walkthrough.

## Architecture

```
servant-reimagined-server    API types -> REST handlers -> tower Service
servant-reimagined-grpc      API types -> gRPC handlers -> tower Service
         |                            |
   tower / tower-http / tower-grpc   Service/Layer/Middleware/gRPC framing
         |
      http-core              Backend-agnostic Request/Response
         |
  tower-wai | tower-server   Pick your backend (HTTP/1.1 + HTTP/2)
         |           |
       warp      raw sockets

  tower-websocket            WebSocket session types (protocol-enforced)
```

Each layer is independent. `tower` knows nothing about HTTP. `http-core`
knows nothing about WAI. `tower-grpc` knows nothing about API types.
The server produces a `Service` — it doesn't know or care what runs it.

## Packages

| Package | What it does |
|---------|-------------|
| [`servant-reimagined-core`](servant-reimagined-core/) | Type-level API: endpoints, paths, effects, sessions, versioning. Depends on `base` only. |
| [`tower`](tower/) | Service/Layer/Middleware composition. Depends on `base` only. Standalone — use it anywhere. |
| [`http-core`](http-core/) | Backend-agnostic Request, Response, Extensions (typed heterogeneous map). |
| [`tower-http`](tower-http/) | HTTP middleware: security headers, request ID, tracing, CORS, gzip, timeouts. |
| [`tower-wai`](tower-wai/) | WAI/warp backend adapter. The only package that imports WAI. |
| [`tower-server`](tower-server/) | Tower-native HTTP/1.1 + HTTP/2 server with TLS. Zero WAI dependency. |
| [`tower-grpc`](tower-grpc/) | gRPC wire protocol: framing, status codes, service dispatch. No protobuf dependency. |
| [`servant-reimagined-server`](servant-reimagined-server/) | Handler wiring, routing, extractors, effect tracking. |
| [`servant-reimagined-client`](servant-reimagined-client/) | Type-safe HTTP client derived from the same API type. |
| [`servant-reimagined-openapi`](servant-reimagined-openapi/) | OpenAPI 3.1 + Swagger 2.0 spec generation from API types. Annotations (`Describe`, `WithParams`, `WithHeaders`, `RespondsWith`) populate real operation summaries, parameter schemas, status codes, and request/response body schemas. Custom types get schemas automatically via `Generic`-based `ToSchema` derivation. |
| [`servant-reimagined-codegen`](servant-reimagined-codegen/) | Generate API types from OpenAPI/Swagger specs. |
| [`servant-reimagined-grpc`](servant-reimagined-grpc/) | gRPC interpretation: `GrpcCodec`, `GrpcReady`, `.proto` generation, `mkGrpcServiceMap`. |
| [`servant-reimagined-test`](servant-reimagined-test/) | Direct-dispatch testing: no network, no ports. |
| [`tower-websocket`](tower-websocket/) | WebSocket session types: phantom-typed `Session` handle enforces send/recv protocol at compile time. |
| [`servant-reimagined`](servant-reimagined/) | Facade — re-exports everything for convenience. |

## Examples

The `examples/` directory contains 12 complete applications:

- [`examples/minimal`](examples/minimal/) — simplest possible server (1 endpoint)
- [`examples/hello-world`](examples/hello-world/) — 3 endpoints, effect tracking, middleware stack
- [`examples/crud`](examples/crud/) — full CRUD with named routes, structured errors, and ValidatedBody
- [`examples/auth`](examples/auth/) — custom authentication extractors
- [`examples/custom-extractors`](examples/custom-extractors/) — writing your own request extractors
- [`examples/grpc-demo`](examples/grpc-demo/) — gRPC server with .proto generation
- [`examples/chat`](examples/chat/) — session-typed WebSocket chat
- [`examples/negotiate`](examples/negotiate/) — content negotiation (JSON, XML, plain text)
- [`examples/versioned-api`](examples/versioned-api/) — API versioning with typed version headers
- [`examples/streaming`](examples/streaming/) — Server-Sent Events with async streaming
- [`examples/realworld`](examples/realworld/) — RealWorld spec API types split into 6 sub-APIs
- [`examples/realworld-combined`](examples/realworld-combined/) — full RealWorld backend: 15 endpoints across 6 sub-APIs with handlers, in-memory store, and combined effect tracking

## Key design decisions

- **Promoted lists, not trees.** APIs are `'[endpoint1, endpoint2, ...]`.
  Compile time scales linearly, not exponentially.
- **IO handlers.** No `ExceptT`, no `ReaderT`. State via extractors
  (`AppState`), errors via `Either`.
- **tower Service as the boundary.** The server produces it, the backend
  consumes it. Middleware composes with `|>`.
- **WAI is confined.** Only `tower-wai` imports WAI. Swap it for
  `tower-server`, a Lambda adapter, or anything else.
- **Compile times are first-class.** 1 to 32 endpoints compile in
  constant time (~1.3s including GHC startup).
- **Runtime is fast.** 142 ns dispatch, 14 ns gRPC decode, middleware
  adds zero overhead. See the [**Performance Guide**](docs/PERFORMANCE.md)
  for full benchmarks and analysis.
- **Optional type-level annotations.** Endpoints can be wrapped with
  `Describe`, `WithParams`, `WithHeaders`, `RespondsWith` / `PostCreated` /
  `DeleteNoContent`, `Versioned`, and streaming markers (`ServerStream`,
  `ClientStream`, `BidiStream`) for richer OpenAPI specs and client
  generation -- without changing routing or handler signatures.
  `Versioned V1 (Get ...)` routes to `/v1/...` and is supported by the
  server, client, and OpenAPI interpretations. See the
  [tutorial](docs/TUTORIAL.md#step-8-annotating-endpoints-for-documentation)
  for details.
- **Named endpoints.** Wrap endpoints with `Named "fieldName"` to enable
  record-based handler binding via `mkRecordApi` — field order doesn't
  matter, the compiler matches by name. `Named` also sets `operationId`
  in OpenAPI specs. Fully opt-in: unnamed endpoints and positional tuples
  continue to work unchanged. On the client side, `mkClientRecord`
  constructs a typed client record from `Named` APIs via `Generic`.
- **Request validation.** `ValidatedBody v a` deserializes JSON and runs
  a `Validate v a` check before the handler sees the data. Validation
  failures return 422 with a structured error. Define a validator type,
  write a `Validate` instance, and use `ValidatedBody` in the handler
  signature — no manual error-checking boilerplate.
- **Async SSE streaming.** `sseResponse` runs the handler in a forked
  thread and delivers Server-Sent Events incrementally via chunked
  transfer encoding. For simple cases, `sseResponseSync` collects all
  events first.

## Next steps

- Read the [**Tutorial**](docs/TUTORIAL.md) — a step-by-step walkthrough
  building a complete API
- Read the [**Design Philosophy**](docs/DESIGN-PHILOSOPHY.md) — why every
  decision was made, compile-time performance analysis, Servant comparison
- Read the [**Data Flow**](docs/DATA-FLOW.md) — request-to-response flow
  diagrams for HTTP/1.1, HTTP/2, gRPC, and the compile-time type checking flow
- Coming from Servant? Read the [**Migration Guide**](docs/MIGRATING-FROM-SERVANT.md)
  — side-by-side comparison of every concept
- Read the [**Error Handling Guide**](docs/ERROR-HANDLING.md) for patterns
  on structured errors, early return, and fallible extractors
- Read the [**Streaming Guide**](docs/STREAMING.md) for large request/response
  bodies
- Read the [**gRPC Guide**](docs/GRPC.md) for serving gRPC from the same API
  type
- Read the [**Performance Guide**](docs/PERFORMANCE.md) — compile-time and
  runtime benchmarks with analysis
- Browse the [examples](examples/) for patterns to copy
- See [`ARCHITECTURE.md`](ARCHITECTURE.md) for the full design
- See [`VISION.md`](VISION.md) for project goals
