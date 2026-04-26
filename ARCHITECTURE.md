# Architecture

This document describes how `acolyte` is organized: the package
layout, the layered design, and the rationale behind the major boundaries.
For a quick start use the [README](README.md). For request-to-response flow
diagrams use [`docs/DATA-FLOW.md`](docs/DATA-FLOW.md). For benchmarks see
[`docs/PERFORMANCE.md`](docs/PERFORMANCE.md).

Target compiler: **GHC 9.10.3**.

---

## Design principles

1. **The type-level core has no runtime dependencies.** The package that
   defines endpoints, paths, methods, effects, session types, versioning, and
   content negotiation depends only on `base` and `text`. It is pure
   type-level machinery. Every interpretation (server, client, OpenAPI, gRPC)
   lives in a separate package that reads the same core types.

2. **The Service/Layer abstraction is a standalone library.** `tower` is not
   tied to the web framework, the same way Rust's `tower` crate is not tied
   to `axum`. It depends only on `base` and is usable from any Haskell
   project that wants composable middleware: web servers, gRPC, message
   queues, anything.

3. **Interpretations are independent of each other.** The server package
   does not depend on the client package. The OpenAPI package does not
   depend on either. Each interpretation reads the same core API types and
   produces its own output.

4. **Compile time is a first-class concern.** Closed type families are
   preferred over open type class instances. Flat tuple indexing is preferred
   over recursive constraint chains. Compile times are benchmarked
   (`bench/compile-time/`) and stay constant from 1 to 32 endpoints.

5. **WAI is confined to a single adapter.** Only `tower-wai` imports
   `Network.Wai`. Every other package operates on backend-agnostic types
   from `http-core`. Replacing the backend means replacing one adapter
   package, not rewriting the framework.

---

## Package layout

```
                       ┌──────────────────────────┐
                       │   acolyte     │
                       │       (facade)           │
                       └──────────────┬───────────┘
                                      │
   ┌──────────────┬──────────────┬────┼────┬──────────────┬──────────────┐
   ▼              ▼              ▼    ▼    ▼              ▼              ▼
 server        client        openapi grpc test          codegen      websocket
   │              │              │    │    │
   └──────────────┴──────────────┴────┼────┘
                                      ▼
                       ┌────────────────────────┐
                       │ acolyte-core │
                       └────────────────────────┘

   ┌────────────┐   ┌──────────────┐   ┌──────────────┐   ┌──────────────┐
   │   tower    │◄──│  tower-http  │   │  tower-grpc  │   │tower-protobuf│
   └────────────┘   └──────────────┘   └──────────────┘   └──────────────┘
        ▲                                     ▲                  ▲
        │                                     │                  │
   ┌────┴────────┐  ┌────────────┐            └──────────────────┘
   │ tower-server │  │  tower-wai │                used by
   │ (HTTP/1.1+H2)│  │ (warp/WAI) │            acolyte-grpc
   │   no WAI     │  └────────────┘
   └─────────────┘

   ┌────────────┐
   │ http-core  │   backend-agnostic Request/Response, Extensions map
   └────────────┘   used by every package above except tower itself
```

Sixteen packages total, organized in three families:

- **`tower-*`**: protocol and transport. Service/Layer composition,
  HTTP middleware, HTTP/1.1+HTTP/2 server, WAI adapter, gRPC wire protocol,
  protobuf, WebSocket. None of these know what a `acolyte`
  endpoint is.
- **`acolyte-*`**: type-level API specification and the
  interpretations that read it: server, client, OpenAPI, gRPC, codegen,
  test utilities. Plus the facade.
- **`http-core`**: the shared HTTP vocabulary. Backend-agnostic
  `Request`/`Response`/`Extensions`. Used everywhere except `tower` itself.

---

## The two-layer middleware model

There are two fundamentally different kinds of middleware in this codebase,
operating at different levels. They compose orthogonally and are easy to
confuse.

### Layer 1: generic middleware (tower Layers)

CORS, compression, tracing, timeouts, secure headers, request IDs, rate
limiting. These wrap the entire server as
`Service IO Request Response -> Service IO Request Response` and transform
requests and responses without any knowledge of individual endpoint types.
They compose via tower's `Layer` abstraction.

**Where they live:** `tower`, `tower-http`.

**What they see:** request method, path, headers, body bytes, response
status. Nothing about the API type, endpoint types, handler arguments, or
type-level effects.

**Compile-time enforcement:** the server tracks which generic middleware
has been provided as a phantom type parameter. `provide @Auth` moves a
phantom and does nothing at runtime; the actual middleware is applied
separately via `|>`. At `run` time, a type family checks that every
`Requires e _` in the API has its `e` in the provided list. Missing
`provide @Auth`? Compile error.

### Layer 2: per-endpoint typed wrappers

`Protected auth endpoint`, `ValidatedBody validator endpoint`,
`Versioned version endpoint`, `Requires effect endpoint`, `Named name endpoint`.
These are type-level wrappers on individual endpoints in the API type. They
are **not** tower Layers. They modify how individual handlers are bound and
dispatched by the router.

**Where they live:** types in `acolyte-core`, dispatch logic in
`acolyte-server`.

**What they see:** the full endpoint type, including method, path, request
body type, response type, auth type, validator type, version prefix.

**How they work:**
- `Protected auth` enforces (via the `FirstArg` type family) that the
  handler's first argument is the auth credential type.
- `ValidatedBody v a` deserializes JSON and runs a `Validate v a` check
  before the handler sees the data. Failures return 422.
- `Versioned V1 (Get UsersPath ...)` matches `/v1/users` instead of
  `/users`. The version becomes a path prefix inside the router.
- `Requires effect` is a phantom marker. It delegates all dispatch to the
  inner endpoint and exists only to be checked by the effect verifier at
  `run` time.
- `Named "fieldName"` associates a type-level `Symbol` name with an
  endpoint, enabling record-based handler binding via `mkRecordApi` (which
  uses `GHC.Records.HasField`). Also sets `operationId` in OpenAPI output.

### How the layers compose

```
  ┌──────────────────────────────────────────────────────────────┐
  │  acolyte-core                                      │
  │  Type-level: Endpoint, Requires, Protected, ValidatedBody,    │
  │  Versioned, Named, session types, change tracking.            │
  │  Pure types. No runtime. No IO.                               │
  └──────────────────────────────┬───────────────────────────────┘
                                 │ read by
                                 ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  acolyte-server                                    │
  │  Per-endpoint dispatch: handler binding, auth enforcement,    │
  │  validation, versioned routing, content negotiation.          │
  │                                                               │
  │  Produces: Service IO (Request ByteString) (Response ByteString) │
  └──────────────────────────────┬───────────────────────────────┘
                                 │ wrapped by
                                 ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  tower / tower-http                                           │
  │  Generic middleware: CORS, tracing, compression, timeouts.    │
  │  Operates on http-core Request/Response. No endpoint types.   │
  └──────────────────────────────┬───────────────────────────────┘
                                 │ run by
                ┌────────────────┼────────────────┐
                ▼                                 ▼
       ┌─────────────────┐               ┌─────────────────┐
       │   tower-wai     │               │  tower-server   │
       │ converts to WAI │               │ HTTP/1.1+H2+TLS │
       └────────┬────────┘               │   no WAI        │
                ▼                        └─────────────────┘
       ┌─────────────────┐
       │      warp       │
       │  TCP, TLS, H2   │
       └─────────────────┘
```

The bottom row is swappable. Everything above the adapter boundary uses
`http-core` types and is backend-agnostic. Most users never drop down to
WAI directly.

A consequence worth highlighting: an endpoint can be both `Protected`
(layer 2) and `Requires Auth` (layer 1). `Protected` ensures the handler
receives auth credentials at dispatch time. `Requires Auth` ensures the
auth middleware tower Layer is present in the stack. Two different concerns,
both compile-checked, neither aware of the other.

---

## Backend agnosticism: the `http-core` types

For the framework to be pluggable, the request and response types that flow
through the system cannot come from WAI. In Rust this is solved by the
standalone `http` crate. Haskell historically lacks this layer: WAI's types
play double duty as both the shared vocabulary and the warp interface, which
couples every middleware to WAI.

`http-core` is the answer: a small package defining backend-agnostic
`Request body`, `Response body`, and a typed `Extensions` map (replacing
WAI's untyped `Vault`). Both `Request` and `Response` are parameterized by
body type, so the same types support strict (`ByteString`) and streaming
bodies.

```haskell
data Request body = Request
  { requestMethod     :: !Method
  , requestPathRaw    :: !ByteString
  , requestPath       :: ![Text]
  , requestQuery      :: !Query
  , requestHeaders    :: !RequestHeaders
  , requestBody       :: !body
  , requestExtensions :: !(IORef Extensions)
  }

data Response body = Response
  { responseStatus  :: !Status
  , responseHeaders :: !ResponseHeaders
  , responseBody    :: !body
  }
```

Each backend provides a thin adapter that converts between these types and
its own. `tower-wai` converts to/from `Wai.Application` (and offers
`fromWaiMiddleware` for using existing WAI middleware). `tower-server`
parses and renders HTTP/1.1 and HTTP/2 directly from raw sockets and never
imports WAI at all.

The adapter conversion cost is minimal: copying a few pointers to headers
and path segments. The body is already strict bytes by the time the adapter
hands the request off.

---

## Handler model

Handlers are plain `IO` functions. No `ExceptT`, no `ReaderT`, no `liftIO`.

```haskell
getUser :: PathCapture Int -> AppState DbPool -> IO (Either AppError (Json User))
getUser (PathCapture uid) (AppState pool) = do
  result <- Pool.withResource pool $ \conn -> DB.getUser conn uid
  case result of
    Nothing   -> pure (Left (notFound "user not found"))
    Just user -> pure (Right (Json user))
```

Three rules govern the handler shape:

1. **State arrives via extractors, not a reader monad.** Anything a handler
   needs (DB pool, config, current user, request body, query params, headers)
   shows up as a function argument with a type that has a `FromRequest` or
   `FromRequestParts` instance. The handler signature *is* the extraction
   contract, the framework wires it up.

2. **Errors are return values, not throws.** Handlers that can fail return
   `IO (Either AppError a)`. Anything implementing `IntoResponse` works as
   a response, including `Either` and tuple types. Genuine exceptions (bugs)
   propagate to a recovery layer that returns 500.

3. **The declared response type is the happy-path type.** An endpoint
   declared as `Get "users" :> Capture Int :> Json User` says the spec
   produces a `User`. Handlers can return richer types as long as the
   happy path produces the declared response. This is what lets
   `IO (Either AppError User)` satisfy a `Json User` declaration without
   any explicit compatibility class.

If a user wants `ReaderT`-style handlers, they can wrap their own:
the framework is opt-in to monad transformers, not opt-out.

---

## Package reference

Each entry covers purpose, what it depends on, and what it is depended on by.
Module-level structure is intentionally not enumerated here (it changes
faster than this doc can be updated). Use `ls <package>/src/` to see the
current layout.

### Foundation (no internal dependencies)

#### `tower`

Service and Layer composition. The Haskell counterpart to Rust's `tower`
crate. Depends only on `base`. Defines:

- `Service m req resp`: a function with a name. The fundamental building
  block.
- `Layer m reqI respI reqO respO`: a service transformer. Carries
  configuration, produces a transformed service when applied.
- `Middleware m req resp`: a layer that doesn't change request/response
  types (the common case).
- `(|>)`: apply a layer to a service.

Newtype-based, not type-class-based: simpler in Haskell than the Rust
equivalent, and avoids orphan instance problems. No `poll_ready`
backpressure: Haskell's runtime handles backpressure through laziness
and green-thread scheduling.

#### `http-core`

Backend-agnostic HTTP types. Depends on `base`, `bytestring`, `text`,
`http-types`, `containers`. Defines `Request body`, `Response body`,
`Extensions` (typed heterogeneous map keyed by `TypeRep`), strict and
streaming body types.

#### `acolyte-core`

Pure type-level API specification. Depends only on `base` and `text`.
Defines the vocabulary that every interpretation reads:

- `Endpoint method path req resp`, with `Get`/`Post`/`Put`/`Delete`/`Patch`
  aliases.
- `PathSegment` (`Lit`, `Capture`, `CaptureRest`, `ParamNamed`).
- The `Method` and `Effect` promoted data kinds.
- `Json a` newtype (used by both server and OpenAPI for body schemas).
- Endpoint wrappers: `Requires`, `Protected`, `Named`, `Describe`,
  `Description`, `WithParams`, `WithHeaders`, `RespondsWith`, streaming
  markers.
- `Versioned` (URL-prefix routing) plus the typed-delta machinery
  (`Change`, `Added`, `Removed`, `Replaced`, `Deprecated`, `ApplyChanges`,
  `BackwardCompatible` constraint).
- `SessionType` for WebSocket protocols, with the `Dual` type family.

No IO, no runtime, no allocations. This is the source of truth that every
interpretation reads.

### Middleware

#### `tower-http`

HTTP-specific tower Layers. Depends on `tower`, `http-core`, `zlib`,
`directory`, `filepath`. Provides CORS, secure headers, request ID,
tracing, timeouts, gzip/deflate compression, and static file serving.

### Backend adapters (pick one or more)

#### `tower-wai`

WAI/warp adapter. Depends on `tower`, `http-core`, `wai`, `warp`. The
*only* package in the codebase that imports `Network.Wai`. Provides
`runWarp`, `toWaiApp`, `fromWaiApp`, `fromWaiMiddleware`,
`toWaiMiddleware`. Use this when you want existing WAI middleware (gzip,
logger, anything from `wai-extra`) or when you want warp's mature
HTTP/2/TLS implementation.

#### `tower-server`

Tower-native HTTP/1.1 + HTTP/2 server with TLS. Depends on `tower`,
`http-core`, `network`, `attoparsec`, `tls`, `http2`, `http-semantics`,
`network-run`, `async`. Zero WAI dependency. Use this when you want the
smallest possible dependency footprint, or when serving gRPC (which needs
HTTP/2).

### Wire protocols

#### `tower-grpc`

gRPC wire protocol. Depends on `tower`, `http-core`, `zlib`. Handles
framing, status codes, service dispatch, REST+gRPC multiplexing, server
reflection, health checks, and gzip compression. Knows nothing about
protobuf or about API types; pairs with `tower-protobuf` for codecs and
with `acolyte-grpc` for type-driven dispatch.

#### `tower-protobuf`

Standalone Protocol Buffers encoder/decoder. Depends only on `base`,
`bytestring`, `text`, `containers`. Independent of the rest of the
codebase: usable in any Haskell project that needs proto3 wire format,
including the packed-repeated and proto3-map field encodings. Has its own
benchmarks and protoc cross-validation tests.

#### `tower-websocket`

WebSocket session types. Depends on `acolyte-core` (for the
`SessionType` algebra), `tower`, `http-core`, `aeson`. Provides a
phantom-typed `Session s` handle whose type parameter tracks the current
protocol state. Operations (`send`, `recv`, `offer`, `select1`, `select2`,
`close`, `recurse`, `loop`) consume the old session and produce one at the
next state, so the compiler rejects out-of-order messages. Transport-agnostic
via a `WebSocketConn` abstraction.

### API interpretations

#### `acolyte-server`

HTTP server interpretation. Depends on `acolyte-core`, `tower`,
`http-core`, `aeson`. Produces a `Service IO (Request ByteString) (Response ByteString)`
from an API type and a tuple (or record) of handlers. Provides:

- `mkApi @API (handler1, handler2, ...)` for positional binding.
- `mkRecordApi @API HandlersRecord` for `Named`-based binding.
- `effectfulApi`/`provide`/`run` for compile-time effect tracking.
- 25+ request extractors (`PathCapture`, `JsonBody`, `ValidatedBody`,
  `QueryParam`, `QueryParams`, `HeaderMap`, `AppState`, `Form`,
  `Multipart`, etc.).
- Sub-API composition (`combineServer2` through `combineServer8`) for
  APIs larger than 25 endpoints.
- Content negotiation, validation, async SSE streaming.

Backend-agnostic: produces a tower Service, doesn't know or care what runs
it.

#### `acolyte-client`

Type-safe HTTP client. Depends on `acolyte-core`, `http-core`,
`http-client`, `http-client-tls`. Each endpoint becomes a callable
function with the right argument and return types. `callNamed @"getUser"`
looks up endpoints by name. `mkClientRecord` builds a typed client record
from a `Named` API via `Generic`.

#### `acolyte-openapi`

OpenAPI 3.1 + Swagger 2.0 generation. Depends on
`acolyte-core`, `aeson`, plus standard text/bytes/containers.
Produces a real spec from the API type: `Describe` becomes `summary`,
`WithParams` becomes parameter schemas, `RespondsWith 204` becomes the
response status code, request/response body types become JSON schemas via
the `ToSchema` class. Records with `deriving Generic` get automatic
`ToSchema` instances. Sums and `Either` produce `oneOf`. `Maybe` emits
`nullable: true`.

#### `acolyte-codegen`

Bidirectional code generation. Standalone executable plus library, no
internal dependencies. Generates Haskell API types from OpenAPI/Swagger
specs (`acolyte-codegen <spec>.json`) or from `.proto` files
(`proto-codegen <service>.proto`), and emits handler stubs ready to fill
in.

#### `acolyte-grpc`

gRPC interpretation of the same API type. Depends on
`acolyte-core`, `acolyte-server`, `tower`,
`tower-grpc`, `tower-protobuf`. Provides `GrpcCodec`, `GrpcReady`
constraint, `mkGrpcServiceMap`, `.proto` generation. Pair with
`Tower.Grpc.Multiplex` to serve REST and gRPC on the same port from the
same handlers.

#### `acolyte-test`

Direct-dispatch testing. Depends on `acolyte-core`,
`acolyte-server`, `tower`, `tower-grpc`, `http-core`,
`QuickCheck`. Sends requests directly through a tower Service: no
ports, no sockets, deterministic. Helpers like `get`, `post`,
`shouldHaveStatus`, `shouldHaveBody` for both REST and gRPC.

### Facade

#### `acolyte`

Re-exports the most common types and functions from across the stack.
Depends on every other `acolyte-*` package plus `tower`,
`tower-http`, `tower-server`, `tower-grpc`, `tower-websocket`, `http-core`.
Most users import `Acolyte.Prelude` and get everything they
need.

The facade does **not** depend on `acolyte-codegen` (it's a
build-time tool, not a runtime library) or on `tower-wai` (so apps that
prefer `tower-server` don't pull in WAI). To use either, depend on it
explicitly.

---

## Compile-time strategy

The single largest design lever is preferring closed type families and
flat tuple instances over open recursive type-class chains.

**Servant's compile-time problem** is structural: the API is a right-nested
binary tree of `:<|>` combinators, and `HasServer` recurses into both
branches at every node. GHC's constraint solver does not cache intermediate
results across branches, so the same sub-constraints are re-solved
repeatedly. This is O(2^n).

**The fix** has three pieces:

- **APIs are promoted lists, not trees.** `'[endpoint1, endpoint2, ...]`
  rather than `endpoint1 :<|> endpoint2 :<|> ...`. GHC indexes into a
  list with a closed type family in O(n) with a small constant factor.

- **Closed type families for path parsing and effect membership.** Closed
  type families evaluate top-to-bottom by pattern matching, with no
  backtracking. This is fundamentally cheaper than open type-class
  resolution.

- **Recursive `BuildApi` with a `SplitTuple` helper for handler binding.**
  Two recursive `BuildApi` instances (one for plain endpoints, one for
  `Named` endpoints) walk the API list. The 24 `SplitTuple` instances
  (arity 2 through 25) decompose handler tuples into head and tail. There
  are no per-arity `BuildApi` instances, so adding endpoints doesn't grow
  the instance count GHC has to consider.

The result, measured in `bench/compile-time/`: 1, 4, 8, 16, and 32
endpoints all compile in roughly the same time (~1.3s including GHC
startup). See [`docs/PERFORMANCE.md`](docs/PERFORMANCE.md) for the full
data and analysis.

---

## Key design decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| API encoding | Promoted lists `'[a, b, c]` | Constant compile time vs. exponential `:<|>` |
| Path literals | `Symbol` (`GHC.TypeLits`) | Native; no marker types or TH needed |
| Type-level computation | Closed type families, flat indexing | No backtracking, no recursive instance chains |
| Service abstraction | `tower` newtype, not type class | Simpler, no orphan instances, no coherence issues |
| HTTP types | `http-core`, not WAI | Backend-agnostic; WAI confined to one adapter |
| Handler monad | `IO` directly | Eliminates `liftIO`; no transformer stack forced on users |
| State passing | Extractors (`AppState`), not `ReaderT` | Explicit, composable, scoped per handler |
| Error handling | `Either AppError a` return values | No `throwError`/`catchError` machinery |
| Backend | Pluggable via adapter packages | `tower-wai` (warp) or `tower-server` (zero-WAI HTTP/1.1+H2) |
| WebSocket sessions | Phantom-typed `Session s` handle | Same guarantees as Linear Haskell, simpler ergonomics |
| Streaming | Async SSE + chunked transfer | Forks the producer, delivers events as they're ready |
| Testing | Dedicated package, direct dispatch | No ports, no sockets, deterministic |
| Target compiler | GHC 9.10.3 | |
