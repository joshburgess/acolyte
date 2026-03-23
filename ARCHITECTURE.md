# Architecture: Package Structure and Dependency Graph

This document defines the package structure for the Haskell reimagining of
Servant, informed by typeway's crate organization. Each package has a clear
responsibility boundary and minimal dependency surface.

Target compiler: **GHC 9.10.3**.

---

## Design Principles

1. **The type-level API core has no runtime dependencies.** The package that
   defines endpoints, paths, methods, effects, session types, versioning, and
   content negotiation depends only on `base`. It is pure type-level machinery.
   All interpretation (server, client, OpenAPI, gRPC) lives in separate
   packages that depend on the core.

2. **The Service/Layer abstraction is a standalone library.** It is not tied
   to the web framework, just as Tower in Rust is not tied to Axum. Any
   Haskell project can use it for composable middleware — web servers, gRPC,
   message queues, whatever. This is a new library for the Haskell ecosystem.

3. **Interpretation packages are independent of each other.** The server
   package does not depend on the client package. The OpenAPI package does not
   depend on the server. Each interpretation reads the same core API types and
   produces its own output.

4. **Feature scope matches typeway.** Every capability typeway provides —
   effects, session types, content negotiation, versioning, multi-protocol
   serving, auth typing, validation — has a place in this architecture.

5. **Compile time is a first-class concern.** We prefer closed type families
   over open type class instances. We prefer flat tuple indexing over recursive
   constraint chains. We benchmark compile times and maintain budgets.

---

## Package Overview

```
┌──────────────────────────────────────────────────────────────────────┐
│                         servant-reimagined                            │
│                    (facade / re-export package)                       │
└──┬────────┬──────────┬──────────┬──────────┬──────────┬─────────────┘
   │        │          │          │          │          │
   ▼        ▼          ▼          ▼          ▼          ▼
 server   client    openapi     grpc     codegen      test

   │        │          │          │          │
   └────────┴──────────┴──────┬───┴──────────┘
                              ▼
              ┌───────────────────────────────┐
              │    servant-reimagined-core     │
              │  (type-level API specification)│
              └───────────────────────────────┘

  ┌──────────┐   ┌──────────────┐   ┌──────────────┐
  │  tower    │   │  tower-http  │   │  tower-grpc  │
  │ (service  │◄──│  (HTTP       │   │  (gRPC wire  │
  │  +layer)  │◄──│  middleware) │   │  protocol,   │
  └──────────┘   └──────────────┘   │  dispatch,   │
       ▲                             │  multiplex,  │
       │◄────────────────────────────│  reflection) │
       │                             └──────────────┘
  ┌────┴──────────┐
  │  tower-server  │  HTTP/1.1 + HTTP/2 (no WAI)
  └───────────────┘
  ┌───────────────┐
  │   tower-wai   │  WAI/warp adapter
  └───────────────┘
  ┌─────────────────┐
  │ tower-websocket  │  WebSocket session types
  └─────────────────┘
```

---

## The Two-Layer Middleware Model

This is a critical architectural distinction that affects every package in
the system. There are two fundamentally different kinds of "middleware" and
they operate at different levels. Conflating them leads to confusion about
where typed guarantees live and what role WAI/tower play.

### Layer 1: Generic middleware (tower Layers)

CORS, compression, tracing, timeouts, secure headers, request IDs, rate
limiting. These wrap the entire server as `Service IO Request Response ->
Service IO Request Response`. They transform requests and responses without
any knowledge of individual endpoint types. They are composed via tower's
`Layer` abstraction.

**Where they live:** `tower`, `tower-http`

**What they know:** Request method, path, headers, body bytes, response
status. Nothing about the API type, endpoint types, handler arguments, or
type-level effects.

**How the effect system interacts:** The effect system tracks which generic
middleware has been provided (`provide @Auth`, `provide @Cors`) as a
phantom type parameter on the server builder. But the tracking is purely
type-level — `provide` changes a phantom and does nothing at runtime. The
actual middleware is applied separately via `.layer()`. At `.serve()` time,
a type family checks that every `Requires e _` in the API has its `e` in
the provided list. If not, compile error. The middleware itself is
completely unaware of the effect system.

This is how typeway works too. From `typeway-server/src/effects.rs`:

```rust
// provide() only moves a phantom type — zero runtime effect
pub fn provide<E: Effect>(self) -> EffectfulServer<A, ECons<E, P>> {
    EffectfulServer {
        router: self.router,      // unchanged
        _api: PhantomData,
        _provided: PhantomData,   // only this changes (phantom)
    }
}
```

### Layer 2: Per-endpoint typed wrappers (handler dispatch)

`Protected auth endpoint`, `Validated validator endpoint`,
`Versioned version endpoint`, `Requires effect endpoint`. These are
**type-level wrappers on individual endpoints in the API type**. They are
NOT tower Layers. They modify how individual handlers are bound and
dispatched by the router.

**Where they live:** `servant-reimagined-core` (type definitions),
`servant-reimagined-server` (dispatch logic)

**What they know:** The full endpoint type — method, path, request body
type, response type, auth type, validator type, version prefix. They have
access to all type-level information because they operate inside the
framework's handler dispatch, not at the generic service level.

**How they work:**

- `Protected auth` — changes which handler binding function is allowed.
  In typeway, `Protected` deliberately does not implement `BindableEndpoint`,
  so `bind!()` produces a type error. Only `bind_auth!()` works, which
  enforces that the auth type is the handler's first argument via a separate
  `AuthHandler` trait. In Haskell, we achieve the same through different
  type class instances for `Serves`.

- `Validated validator` — inserts validation logic between deserialization
  and handler dispatch. The framework deserializes the request body, runs
  the validator, and only calls the handler if validation passes. Returns
  422 on failure. The validator type is known at compile time from the API
  type.

- `Versioned version` — overrides the route pattern and matching function
  to prepend a version prefix. A `Versioned V1 (Get UsersPath ...)` matches
  `/v1/users` instead of `/users`. This happens inside the router, not as a
  Layer.

- `Requires effect` — purely a phantom marker. Delegates all dispatch
  behavior to the inner endpoint unchanged. Its only purpose is to be
  checked by the type-level effect verification at `.serve()` time.

### How the layers compose

```
  ┌──────────────────────────────────────────────────────────────┐
  │  servant-reimagined-core                                      │
  │  Type-level: Endpoint, Requires, Protected, Validated, ...    │
  │  Pure types. No runtime. No IO.                               │
  └──────────────────────────────┬───────────────────────────────┘
                                 │ read by
                                 ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  servant-reimagined-server                                    │
  │  Per-endpoint dispatch: handler binding, auth enforcement,    │
  │  validation, versioned routing, content negotiation.          │
  │  Operates on individual endpoints with full type info.        │
  │  Uses http-core types (NOT WAI types).                        │
  │                                                               │
  │  Produces: Service IO (Request StrictBody) (Response ...)     │
  └──────────────────────────────┬───────────────────────────────┘
                                 │ wrapped by
                                 ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  tower / tower-http / tower-grpc                              │
  │  Generic middleware: CORS, tracing, compression, timeouts.    │
  │  gRPC wire protocol: framing, status, dispatch, multiplex.    │
  │  Operates on http-core Request/Response. No endpoint types.   │
  │  Effect system tracks which middleware is present (phantom).   │
  └──────────────────────────────┬───────────────────────────────┘
                                 │
       ┌─────────────┬──────────┼──────────────────┐
       ▼             ▼          ▼                   ▼
  ┌────────────┐ ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
  │tower-server│ │    tower-wai    │ │   tower-snap    │ │  tower-lambda   │
  │ HTTP/1.1 + │ │ converts to WAI │ │ converts to Snap│ │ converts to λ   │
  │ HTTP/2     │ └────────┬────────┘ └────────┬────────┘ └────────┬────────┘
  │ (no WAI)   │          ▼                   ▼                    ▼
  └────────────┘ ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
                 │      warp       │ │      snap       │ │   AWS Lambda    │
                 │  TCP, TLS, H2   │ │                 │ │                 │
                 └─────────────────┘ └─────────────────┘ └─────────────────┘
```

The bottom two rows are **swappable**. Everything above the adapter
boundary uses `http-core` types and is completely backend-agnostic.

**Users interact with the top two layers.** They define API types (core),
write handlers and apply typed wrappers (server), and stack generic
middleware (tower/tower-http). The bottom two layers (tower-wai and warp)
are infrastructure that users never think about. Nobody drops down to raw
WAI — there is no reason to, because all typed middleware lives above
tower, and all generic middleware lives in tower itself.

### Why this matters for the architecture

This two-layer model clarifies several things:

1. **tower-wai is a thin adapter, not an escape hatch.** It converts tower's
   `Service IO Request Response` into WAI's `Application` so warp can consume
   it. It does not limit what the upper layers can do. The typed effect system,
   auth enforcement, validation, content negotiation, and session types all
   happen above this boundary with full access to API type information.

2. **WAI types flow through the system but are not the composition interface.**
   `Request` and `Response` from WAI are the concrete types that tower
   Services operate on. But composition happens through tower's `Layer`
   abstraction, not through WAI's `Middleware` type. Users who want existing
   WAI middleware can use `fromWaiMiddleware` in `tower-wai` as an opt-in
   interop bridge, not as the primary programming model.

3. **Generic middleware and typed middleware compose orthogonally.** You can
   have an endpoint that is both `Protected` (layer 2) and `Requires Auth`
   (layer 1). The `Protected` wrapper ensures the handler receives auth
   credentials at the dispatch level. The `Requires Auth` marker ensures the
   auth middleware tower Layer exists in the stack. These are separate
   concerns that compose without interference.

4. **The effect system is a phantom-type discipline, not a runtime mechanism.**
   Adding `Requires Cors` to an endpoint does not add CORS handling. It
   declares that CORS handling is required. The user must separately call
   `.provide @Cors` (which moves a phantom) and `.layer corsLayer` (which
   applies actual middleware). The compiler checks the phantom list against
   the API type at `.serve()` time and refuses to compile if they don't match.

---

## Backend Agnosticism: The HTTP Types Question

For the framework to be truly pluggable — swapping warp for a different
server, swapping WAI for a different interface, or running on a custom
transport — the types that flow through the system cannot come from WAI.

In Rust, this is solved by the standalone `http` crate: a tiny package
defining `Request<B>` and `Response<B>` that everyone depends on. Hyper
uses them. Tower uses them. Axum uses them. Swapping hyper for something
else is possible because the types above don't come from hyper.

In Haskell today, WAI's `Request` and `Response` types play double duty:
they are both the shared vocabulary AND the interface to warp. This couples
every middleware and framework to WAI.

**Our answer: we define our own Request and Response types.** They use
`http-types` primitives (Method, Status, Header) but are not WAI types.
Everything above the adapter layer uses our types. Adapters at the edge
convert to/from the backend.

### The `http-core` package

A tiny, dependency-light package defining the shared HTTP vocabulary types.
This is the Haskell equivalent of Rust's `http` crate.

**Depends on:** `base`, `bytestring`, `text`, `http-types` (for Method,
Status, Header), `case-insensitive`

**Depended on by:** `tower-http`, `servant-reimagined-server`,
`servant-reimagined-client`, `tower-wai`, and any future backend adapter.

```haskell
-- | An HTTP request, parameterized by body type.
--
-- The body type parameter enables both strict (ByteString) and
-- streaming (ConduitT/IO chunks) request bodies.
data Request body = Request
  { requestMethod     :: !Method
  , requestPathRaw    :: !ByteString        -- original path
  , requestPath       :: ![Text]            -- pre-split segments
  , requestQuery      :: !Query
  , requestHeaders    :: !RequestHeaders
  , requestBody       :: !body
  , requestExtensions :: !(IORef Extensions)
  }

-- | An HTTP response, parameterized by body type.
data Response body = Response
  { responseStatus  :: !Status
  , responseHeaders :: !ResponseHeaders
  , responseBody    :: !body
  }

-- | Heterogeneous map for passing typed data between middleware and handlers.
-- Replaces WAI's Vault with a typed, IORef-based store.
data Extensions  -- opaque, with insert/lookup by TypeRep

insertExtension :: Typeable a => a -> Extensions -> IO ()
lookupExtension :: Typeable a => Extensions -> IO (Maybe a)

-- | Common body types
data StrictBody   = StrictBody !ByteString
data StreamBody m = StreamBody (ConduitT () ByteString m ())
data EmptyBody    = EmptyBody
```

**Why a separate package?** Because these types are the shared vocabulary.
`tower` is protocol-agnostic (it shouldn't know about HTTP). `tower-http`
provides middleware. `servant-reimagined-core` is pure type-level. None of
these is the right home for concrete HTTP request/response types. A small,
stable, rarely-changing package is the right granularity — just like
Rust's `http` crate.

### How adapters work

Each backend provides a thin adapter package that converts between our
types and its own:

```haskell
-- tower-wai (adapter for warp/WAI backend)
toWaiApp     :: Service IO (Request StrictBody) (Response StrictBody) -> Wai.Application
fromWaiReq   :: Wai.Request -> IO (Request StrictBody)
toWaiResp    :: Response StrictBody -> Wai.Response

-- hypothetical tower-snap (adapter for Snap framework)
toSnapHandler :: Service IO (Request StrictBody) (Response StrictBody) -> Snap.Handler ()

-- hypothetical tower-direct (raw sockets, no WAI, no warp)
runDirect :: Settings -> Service IO (Request StrictBody) (Response StrictBody) -> IO ()
```

The conversion cost is minimal — it's copying a few pointers to headers
and path segments. The request body is already strict bytes at this point
(the backend collects the body before handing it to the adapter).

### What this means for each package

| Package | Uses our types? | Knows about WAI? |
|---------|-----------------|-------------------|
| `tower` | Generic (any req/resp) | No |
| `http-core` | Defines them | No |
| `tower-http` | Yes | No |
| `tower-grpc` | Yes | No |
| `tower-server` | Yes | No |
| `servant-reimagined-core` | No types (pure type-level) | No |
| `servant-reimagined-server` | Yes | No |
| `servant-reimagined-client` | Yes | No |
| `servant-reimagined-grpc` | Yes | No |
| `servant-reimagined-openapi` | No (generates specs) | No |
| `servant-reimagined-codegen` | No (generates code) | No |
| `servant-reimagined-test` | Yes | No |
| `tower-wai` | Yes (converts to/from WAI) | **Yes** (only package that does) |
| `warp` | No (external) | Yes (external) |

WAI is confined to a single adapter package. Swap it out and everything
above is unchanged.

---

## Package Details

### 1. `tower` — Service and Layer Abstraction

**Purpose:** A standalone, framework-agnostic library providing composable
service and middleware abstractions for Haskell. The Haskell equivalent of
Rust's `tower` crate. This is where Layer 1 (generic middleware) operates
— see "The Two-Layer Middleware Model" above.

**This library does not exist in the Haskell ecosystem today.** WAI provides
a raw `Application` type (CPS-style request handler), but no composable
`Service`/`Layer` interface. Every middleware author invents their own
conventions. This package fixes that.

**Depends on:** `base`

**Depended on by:** `tower-http`, `servant-reimagined-server`,
`servant-reimagined-grpc`

**Key types:**

```haskell
-- | A service transforms requests into responses.
--
-- This is the fundamental building block. Everything — HTTP servers,
-- gRPC handlers, message queue consumers — can be expressed as a Service.
newtype Service m req resp = Service
  { runService :: req -> m resp
  }

-- | A layer wraps an inner service to produce an outer service.
--
-- Layers are middleware factories. They carry configuration and produce
-- a transformed service when applied.
newtype Layer m reqI respI reqO respO = Layer
  { applyLayer :: Service m reqI respI -> Service m reqO respO
  }

-- | A middleware does not change the request/response types.
-- Most HTTP middleware falls into this category.
type Middleware m req resp = Layer m req resp req resp

-- | Compose two layers. The outer layer wraps the result of the inner.
-- This is the primary composition operator.
composeLayer
  :: Layer m req2 resp2 req3 resp3
  -> Layer m req1 resp1 req2 resp2
  -> Layer m req1 resp1 req3 resp3

-- | Apply a layer to a service.
(|>)
  :: Service m reqI respI
  -> Layer m reqI respI reqO respO
  -> Service m reqO respO
```

**Key combinators (ServiceExt equivalent):**

```haskell
-- | Transform the request before it reaches the inner service.
mapRequest :: (req' -> req) -> Middleware m req resp
-- (actually: Layer m req resp req' resp)

-- | Transform the response after the inner service produces it.
mapResponse :: (resp -> resp') -> Layer m req resp req resp'

-- | Transform errors in the response.
mapErr :: (e -> e') -> Layer m req (Either e a) req (Either e' a)

-- | Run a callback before the service processes the request.
before :: (req -> m ()) -> Middleware m req resp

-- | Run a callback after the service produces the response.
after :: (req -> resp -> m ()) -> Middleware m req resp

-- | Apply a timeout to the service.
timeout :: Duration -> Middleware IO req (Either Timeout resp)

-- | Retry the service according to a policy.
retry :: RetryPolicy -> Middleware m req resp
```

**Module structure:**

```
Tower
├── Tower                        -- Service, Layer, Middleware, (|>)
├── Tower.Layer                  -- Layer type, composeLayer, identity
├── Tower.Layer.Timeout          -- Timeout middleware
├── Tower.Layer.Retry            -- Retry with backoff policies
├── Tower.Layer.RateLimit        -- Token bucket / sliding window
├── Tower.Service                -- Service type, ServiceExt combinators
└── Tower.Util                   -- mapRequest, mapResponse, before, after
```

**Design decision — newtype, not typeclass:** In Rust, `Service` is a trait
because Rust uses traits for polymorphism. In Haskell, a newtype over a
function is simpler, composes better (no orphan instances, no coherence
issues), and is more idiomatic. You can still get type-class-like dispatch
via the handler system in servant-reimagined-server.

**Design decision — no backpressure (poll_ready):** Tower's `poll_ready` exists
because Rust's async model requires explicit readiness signaling. Haskell's
runtime handles backpressure through laziness and green thread scheduling.
If explicit backpressure is needed (rate limiting, connection pooling), it
belongs in specific `Layer` implementations, not the core `Service` type.

---

### 2. `tower-http` — HTTP Middleware Layers

**Purpose:** A collection of HTTP-specific middleware layers built on `tower`.
The Haskell equivalent of Rust's `tower-http` crate.

**Depends on:** `tower`, `http-types` (or `wai`), `bytestring`, `text`,
`aeson` (for JSON error responses), `stm` (for rate limit state)

**Depended on by:** `servant-reimagined-server` (optional, for built-in
middleware), user applications

**Provided layers:**

| Layer | Description |
|-------|-------------|
| `CorsLayer` | CORS preflight and header injection |
| `TraceLayer` | Structured request/response logging |
| `TimeoutLayer` | Request timeout with configurable duration |
| `CompressionLayer` | Response compression (gzip, deflate, brotli) |
| `SecureHeadersLayer` | OWASP security headers (CSP, HSTS, X-Frame-Options, etc.) |
| `RequestIdLayer` | Generate/propagate X-Request-Id headers |
| `AuthLayer` | Authentication middleware (Bearer, Basic, custom) |
| `RateLimitLayer` | Per-client rate limiting (token bucket) |
| `BodyLimitLayer` | Maximum request body size enforcement |

**Module structure:**

```
Tower.Http
├── Tower.Http.Cors              -- CORS handling
├── Tower.Http.Trace             -- Request/response tracing
├── Tower.Http.Timeout           -- HTTP-specific timeout
├── Tower.Http.Compression       -- Response compression
├── Tower.Http.SecureHeaders     -- Security headers
├── Tower.Http.RequestId         -- Request ID generation/propagation
├── Tower.Http.Auth              -- Authentication layers
├── Tower.Http.RateLimit         -- Rate limiting
└── Tower.Http.BodyLimit         -- Body size limits
```

**Design decision — separate from `tower`:** HTTP middleware should not
pollute the generic `tower` package. A gRPC-only service should not need
CORS or compression dependencies. This matches Rust's tower/tower-http split.

---

### 3. `servant-reimagined-core` — Type-Level API Specification

**Purpose:** The pure type-level core. Defines the vocabulary for describing
HTTP APIs as types: endpoints, paths, methods, effects, session types,
versioning, and content negotiation. **No IO, no runtime, no dependencies
beyond `base`.**

This is the single source of truth. Every interpretation package (server,
client, OpenAPI, gRPC) reads these types. The type definitions here drive
both Layer 1 (effect markers like `Requires`) and Layer 2 (endpoint wrappers
like `Protected`, `Validated`, `Versioned`) — see "The Two-Layer Middleware
Model" above. The core only defines the types; runtime behavior lives in
the server package.

**Depends on:** `base`

**Depended on by:** Every other `servant-reimagined-*` package.

**Key types:**

```haskell
-- Endpoints
data Endpoint (method :: Method) (path :: Path) (req :: Type) (resp :: Type)

type Get    path resp       = Endpoint 'GET    path NoBody resp
type Post   path req resp   = Endpoint 'POST   path req    resp
type Put    path req resp   = Endpoint 'PUT    path req    resp
type Delete path resp       = Endpoint 'DELETE path NoBody resp
type Patch  path req resp   = Endpoint 'PATCH  path req    resp

-- Paths (using GHC.TypeLits — no TH needed!)
--
-- In Rust, typeway needs proc macros to generate marker types for string
-- literals because const generic &'static str is unstable. In Haskell,
-- we have Symbol from GHC.TypeLits natively.
type UsersPath     = '["users"]
type UserByIdPath  = '["users", Capture Int]

-- Path segments
data PathSegment = Lit Symbol | Capture Type | CaptureRest

-- Methods (promoted data kind)
data Method = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS

-- API as a type-level list of endpoints
type UsersAPI =
  '[ Get    UsersPath     (Json [User])
   , Get    UserByIdPath  (Json User)
   , Post   UsersPath     (Json CreateUser) (Json User)
   , Delete UserByIdPath  StatusCode
   ]

-- Effects
data Effect = Auth | Cors | RateLimit | Tracing

-- Require an effect on an endpoint
data Requires (e :: Effect) (endpoint :: Type)

-- Protected endpoint (auth required, handler receives auth value)
data Protected (auth :: Type) (endpoint :: Type)

-- Validation
data Validated (validator :: Type) (endpoint :: Type)

-- Content negotiation
data Negotiate (formats :: [Type]) (a :: Type)

-- Session types (for WebSockets)
data SessionType
  = Send Type SessionType      -- send a message, continue
  | Recv Type SessionType      -- receive a message, continue
  | Offer SessionType SessionType    -- offer peer a choice
  | Select SessionType SessionType   -- select from offered choices
  | Rec SessionType            -- recursive protocol
  | Var                        -- recursion variable
  | End                        -- session complete

-- Compute the dual protocol (client sees the mirror)
type family Dual (s :: SessionType) :: SessionType where
  Dual (Send a s)     = Recv a (Dual s)
  Dual (Recv a s)     = Send a (Dual s)
  Dual (Offer s1 s2)  = Select (Dual s1) (Dual s2)
  Dual (Select s1 s2) = Offer (Dual s1) (Dual s2)
  Dual (Rec s)        = Rec (Dual s)
  Dual Var            = Var
  Dual End            = End

-- API versioning
data Change
  = Added Type
  | Removed Type
  | Replaced Type Type
  | Deprecated Type

type family ApplyChanges (base :: [Type]) (changes :: [Change]) :: [Type]

type family IsBackwardCompatible (changes :: [Change]) :: Bool where
  IsBackwardCompatible '[]               = 'True
  IsBackwardCompatible (Removed _ ': _)  = 'False
  IsBackwardCompatible (_ ': rest)       = IsBackwardCompatible rest

data VersionedApi (base :: [Type]) (changes :: [Change]) (resolved :: [Type])
```

**Type families for path analysis:**

```haskell
-- Extract capture types from a path (closed type family — no recursion explosion)
type family Captures (path :: [PathSegment]) :: [Type] where
  Captures '[]                   = '[]
  Captures (Lit _   ': rest)     = Captures rest
  Captures (Capture t ': rest)   = t ': Captures rest
  Captures (CaptureRest ': _)    = '[ [Text] ]

-- Convert captures list to a tuple (for handler arguments)
type family CapturesTuple (cs :: [Type]) :: Type where
  CapturesTuple '[]        = ()
  CapturesTuple '[a]       = a              -- no wrapping for single capture
  CapturesTuple '[a, b]    = (a, b)
  CapturesTuple '[a, b, c] = (a, b, c)
  -- ... up to 8

-- Effect membership (closed, O(n) in effect list length)
type family HasEffect (e :: Effect) (es :: [Effect]) :: Bool where
  HasEffect e '[]       = 'False
  HasEffect e (e ': _)  = 'True
  HasEffect e (_ ': es) = HasEffect e es

-- All effects in an API are provided
type family AllEffectsProvided (api :: [Type]) (provided :: [Effect]) :: Constraint where
  AllEffectsProvided '[] _                    = ()
  AllEffectsProvided (Requires e ep ': rest) provided =
    (HasEffect e provided ~ 'True, AllEffectsProvided rest provided)
  AllEffectsProvided (_ ': rest) provided     = AllEffectsProvided rest provided
```

**Module structure:**

```
Servant.Reimagined.Core
├── Servant.Reimagined.Core.API          -- ApiSpec type family, api-level combinators
├── Servant.Reimagined.Core.Endpoint     -- Endpoint, Get, Post, Put, Delete, Patch
├── Servant.Reimagined.Core.Path         -- PathSegment, Lit, Capture, CaptureRest
├── Servant.Reimagined.Core.Path.Parse   -- Captures type family, CapturesTuple
├── Servant.Reimagined.Core.Method       -- Method kind (GET, POST, etc.)
├── Servant.Reimagined.Core.Effect       -- Effect kind, Requires, HasEffect, AllEffectsProvided
├── Servant.Reimagined.Core.Session      -- SessionType, Dual, Send, Recv, Offer, Select, Rec, Var, End
├── Servant.Reimagined.Core.Negotiate    -- Negotiate, ContentFormat class
├── Servant.Reimagined.Core.Versioning   -- Change, VersionedApi, ApplyChanges, IsBackwardCompatible
├── Servant.Reimagined.Core.Auth         -- Protected, auth wrappers
├── Servant.Reimagined.Core.Validate     -- Validated, Validate class
└── Servant.Reimagined.Core.Error        -- Custom type errors for missing handlers, effects, etc.
```

**GHC extensions required:**

```
DataKinds, TypeFamilies, GADTs, PolyKinds, TypeOperators,
UndecidableInstances, ConstraintKinds, StandaloneKindSignatures
```

**Design decision — type-level lists, not `:<|>`:** Servant uses `:<|>` as a
binary combinator, building a right-nested tree. This is the root cause of
exponential compile times. We use promoted lists (`'[a, b, c]`) instead.
GHC can index into a type-level list in O(n) with a closed type family,
and the constant factor is much smaller than recursive instance resolution
because closed type families are evaluated by simple pattern matching with
no backtracking.

**Design decision — type-level strings, not marker types:** Haskell has
`Symbol` in `GHC.TypeLits`, so path literals are just `Lit "users"` — no
proc macros, no generated marker types, no hidden modules. This eliminates
typeway's biggest workaround.

---

### 4. `servant-reimagined-server` — HTTP Server

**Purpose:** Interprets the API type into a running HTTP server. This is
where Layer 2 (per-endpoint typed dispatch) lives. It reads the core API
types, builds a router with typed handler binding (auth, validation,
versioning, content negotiation), and produces a `tower Service` that
Layer 1 (generic middleware) can wrap. The effect system builder also
lives here, enforcing at compile time that required generic middleware
is present before the server can start.

**Depends on:** `servant-reimagined-core`, `tower`, `tower-wai`, `tower-http`
(optional), `warp`, `wai`, `aeson`, `bytestring`, `text`, `http-types`, `stm`

**Depended on by:** `servant-reimagined` (facade)

**Key types:**

```haskell
-- | A server for API type @api@ with effects @provided@.
data Server (api :: [Type]) (provided :: [Effect]) where
  Server :: (AllEffectsProvided api provided) => Router -> Server api provided

-- | Build a server from an API type and handler tuple.
-- The constraint Serves api handlers verifies completeness at compile time.
serve
  :: forall api handlers provided
   . (Serves api handlers, AllEffectsProvided api provided)
  => handlers
  -> Server api provided

-- | Request extractors (typeclass-based, like typeway/axum)
class FromRequest a where
  fromRequest :: Request -> IO (Either ServerError a)

class FromRequestParts a where
  fromRequestParts :: RequestParts -> Either ServerError a

-- Built-in extractors
newtype PathCapture path = PathCapture (CapturesTuple (Captures path))
newtype JsonBody a       = JsonBody a
newtype QueryParams a    = QueryParams a
newtype AppState a       = AppState a
newtype ReqHeader (name :: Symbol) a = ReqHeader a

-- | Response encoding
class IntoResponse a where
  intoResponse :: a -> Response

-- | Handler type — async functions with extractors as arguments
class Handler args result where
  handle :: (args -> IO result) -> BoundHandler

-- | The API completeness check
class Serves (api :: [Type]) handlers where
  boundHandlers :: handlers -> [BoundHandler]

-- | Effect builder
data EffectfulServer (api :: [Type]) (provided :: [Effect])

provide
  :: forall e api provided
   . EffectfulServer api provided
  -> Middleware IO Request Response
  -> EffectfulServer api (e ': provided)

ready
  :: (AllEffectsProvided api provided)
  => EffectfulServer api provided
  -> Server api provided
```

**Module structure:**

```
Servant.Reimagined.Server
├── Servant.Reimagined.Server              -- Server, serve, run
├── Servant.Reimagined.Server.Handler      -- Handler class, BoundHandler
├── Servant.Reimagined.Server.Extract      -- FromRequest, FromRequestParts
├── Servant.Reimagined.Server.Extract.Path -- PathCapture extractor
├── Servant.Reimagined.Server.Extract.Json -- JsonBody extractor
├── Servant.Reimagined.Server.Extract.Query-- QueryParams extractor
├── Servant.Reimagined.Server.Extract.State-- AppState extractor
├── Servant.Reimagined.Server.Extract.Header -- ReqHeader extractor
├── Servant.Reimagined.Server.Response     -- IntoResponse class, Json, StatusCode
├── Servant.Reimagined.Server.Router       -- Routing dispatch
├── Servant.Reimagined.Server.Serves       -- Serves class (compile-time completeness)
├── Servant.Reimagined.Server.Effects      -- EffectfulServer, provide, ready
├── Servant.Reimagined.Server.Auth         -- AuthHandler, Protected endpoint handling
├── Servant.Reimagined.Server.Negotiate    -- Content negotiation dispatch
├── Servant.Reimagined.Server.Validate     -- Validation middleware
├── Servant.Reimagined.Server.WebSocket    -- WebSocket upgrade + session types
└── Servant.Reimagined.Server.Error        -- ServerError, JsonError, error responses
```

---

### 5. `servant-reimagined-client` — HTTP Client

**Purpose:** Derives a type-safe HTTP client from the API type. Each endpoint
becomes a callable function with the correct argument and return types.

**Depends on:** `servant-reimagined-core`, `http-client`, `aeson`,
`bytestring`, `text`

**Depended on by:** `servant-reimagined` (facade)

**Key types:**

```haskell
-- | A client targeting API type @api@.
data Client (api :: [Type]) = Client
  { clientBaseUrl :: BaseUrl
  , clientManager :: Manager
  }

-- | Call a specific endpoint. The endpoint type determines:
-- - URL path (with captures substituted)
-- - HTTP method
-- - Request body serialization
-- - Response deserialization
class CallEndpoint endpoint where
  type CallArgs endpoint
  type CallResult endpoint
  call :: Client api -> CallArgs endpoint -> IO (Either ClientError (CallResult endpoint))

-- | Interceptor for request/response transformation (auth headers, logging, etc.)
data Interceptor = Interceptor
  { interceptRequest  :: Request -> IO Request
  , interceptResponse :: Response -> IO Response
  }
```

**Module structure:**

```
Servant.Reimagined.Client
├── Servant.Reimagined.Client              -- Client, mkClient
├── Servant.Reimagined.Client.Call         -- CallEndpoint class
├── Servant.Reimagined.Client.Error        -- ClientError
├── Servant.Reimagined.Client.Interceptor  -- Request/response interceptors
├── Servant.Reimagined.Client.Streaming    -- Streaming response support
└── Servant.Reimagined.Client.Retry        -- Retry policies
```

---

### 6. `servant-reimagined-openapi` — OpenAPI Specification

**Purpose:** Generates an OpenAPI 3.1 specification from the API type. Paths,
methods, request/response schemas, security requirements, deprecation markers,
and content types are all derived from the types.

**Depends on:** `servant-reimagined-core`, `aeson`, `text`,
`unordered-containers`

**Depended on by:** `servant-reimagined-server` (optional, for embedded docs),
`servant-reimagined` (facade)

**Key types:**

```haskell
-- | Generate an OpenAPI spec from an API type.
class ApiToSpec (api :: [Type]) where
  toSpec :: Proxy api -> OpenApiSpec

-- | Generate an operation from a single endpoint.
class EndpointToOperation endpoint where
  toOperation :: Proxy endpoint -> Operation

-- | JSON Schema derivation (via GHC.Generics or aeson-schemas)
class ToSchema a where
  toSchema :: Proxy a -> Schema
```

**Module structure:**

```
Servant.Reimagined.OpenApi
├── Servant.Reimagined.OpenApi             -- ApiToSpec, toSpec
├── Servant.Reimagined.OpenApi.Schema      -- ToSchema class, Generic deriving
├── Servant.Reimagined.OpenApi.Operation   -- EndpointToOperation
├── Servant.Reimagined.OpenApi.Types       -- OpenApiSpec, Operation, Schema, etc.
└── Servant.Reimagined.OpenApi.Render      -- JSON rendering, Swagger UI embedding
```

---

### 7. `servant-reimagined-grpc` — gRPC Support

**Purpose:** Unified REST + gRPC serving from the same API type and handlers.
Generates `.proto` files, provides gRPC server dispatch, type-safe gRPC
client, server reflection, and health checks.

**Depends on:** `servant-reimagined-core`, `tower`, `proto-lens` or
`protobuf`, `bytestring`, `http2`, `text`

**Depended on by:** `servant-reimagined` (facade)

**Key types:**

```haskell
-- | Verify all types in the API can be serialized as protobuf.
class GrpcReady (api :: [Type])

-- | Generate a .proto file from the API type.
class ApiToProto (api :: [Type]) where
  toProto :: Proxy api -> ProtoFile

-- | Map a Haskell type to a protobuf message.
class ToProtoType a where
  protoMessageName :: Proxy a -> Text
  protoFields      :: Proxy a -> [ProtoField]

-- | gRPC server wrapping an existing server.
data GrpcServer (api :: [Type])

-- | Type-safe gRPC client.
data GrpcClient (api :: [Type])
```

**Module structure:**

```
Servant.Reimagined.Grpc
├── Servant.Reimagined.Grpc                -- GrpcServer, withGrpc
├── Servant.Reimagined.Grpc.Proto          -- ApiToProto, ToProtoType, .proto generation
├── Servant.Reimagined.Grpc.Client         -- GrpcClient, type-safe calls
├── Servant.Reimagined.Grpc.Dispatch       -- gRPC request dispatch (shared handlers)
├── Servant.Reimagined.Grpc.Codec          -- Protobuf encode/decode
├── Servant.Reimagined.Grpc.Streaming      -- Server/client/bidirectional streaming
├── Servant.Reimagined.Grpc.Reflection     -- Server reflection service
├── Servant.Reimagined.Grpc.Health         -- Health check service
└── Servant.Reimagined.Grpc.Web            -- gRPC-Web support for browser clients
```

---

### 8. `servant-reimagined` — Facade Package

**Purpose:** Re-exports the most common types and functions from all packages.
Users can depend on this single package for typical use cases.

**Depends on:** All of the above.

**Provides:** A `Prelude` module with everything you need for common usage:

```haskell
module Servant.Reimagined.Prelude
  ( -- * Core API types
    module Servant.Reimagined.Core
    -- * Server
  , module Servant.Reimagined.Server
    -- * Client
  , module Servant.Reimagined.Client
    -- * OpenAPI
  , module Servant.Reimagined.OpenApi
  ) where
```

---

## Dependency Graph

```
                    ┌───────────────────────────────┐
                    │      servant-reimagined        │
                    │          (facade)              │
                    └─┬───┬────┬────┬────┬────┬────┘
                      │   │    │    │    │    │
        ┌─────────────┘   │    │    │    │    └────────────┐
        ▼        ▼        ▼    ▼    ▼    ▼                 ▼
   ┌─────────┐ ┌────────┐ ┌────────┐ ┌──────┐ ┌─────────┐ ┌──────┐
   │ server  │ │ client │ │openapi │ │ grpc │ │ codegen │ │ test │
   └────┬────┘ └───┬────┘ └───┬────┘ └──┬───┘ └────┬────┘ └──┬───┘
        │          │          │         │          │          │
        └──────────┴──────────┴────┬────┴──────────┘          │
                                   ▼                          │
                     ┌────────────────────────┐               │
                     │ servant-reimagined-core │◄──────────────┘
                     │                        │
                     │   depends on: base     │
                     └────────────────────────┘


   ┌────────────┐    ┌──────────────────┐    ┌──────────────────┐
   │   tower    │◄───│   tower-http     │    │   tower-grpc     │
   │            │    │                  │    │                  │
   │ base       │    │ tower            │    │ tower            │
   │            │◄───│ http-types       │    │ http-core        │
   └────────────┘    │ bytestring       │    │ bytestring       │
        ▲            │ aeson            │    │ http2            │
        │            └──────────────────┘    └──────────────────┘
        │
   ┌────┴───────────┐
   │  tower-server   │  HTTP/1.1 + HTTP/2 (no WAI)
   │  tower, http2   │
   └────────────────┘
   ┌────────────────┐
   │   tower-wai    │  WAI/warp adapter
   │  tower, wai    │
   └────────────────┘

   Used by: server, grpc, tower-server, tower-wai
```

**Key properties of this graph:**

- `servant-reimagined-core` is a **leaf** — it depends only on `base`. This
  means it compiles fast and never breaks due to upstream dependency changes.

- `tower` is **independent** of `servant-reimagined-core`. They are in
  separate dependency trees. A project could use `tower` without any
  servant-reimagined packages.

- `server`, `client`, `openapi`, `grpc`, `codegen`, and `test` are
  **siblings** — none depends on another. You can use the client without the
  server, or OpenAPI without either.

- `tower-http` and `tower-grpc` depend on `tower` but not on
  `servant-reimagined-core`. Their layers work with any `tower` service.

- `tower-server` provides a WAI-free HTTP/1.1 + HTTP/2 backend. It depends
  on `tower` and `http2`, and is the native backend for gRPC serving.

- REST and gRPC can be **multiplexed** on the same port via `tower-grpc`'s
  `multiplex` combinator, which routes by content-type.

---

## Mapping to Typeway Crates

| Typeway Crate | Haskell Package | Notes |
|---------------|-----------------|-------|
| `typeway-core` | `servant-reimagined-core` | Direct equivalent. Haskell version is simpler — TypeLits eliminates marker type workarounds. |
| `typeway-server` | `servant-reimagined-server` | Direct equivalent. Uses `tower` instead of importing tower crate directly. |
| `typeway-client` | `servant-reimagined-client` | Direct equivalent. |
| `typeway-openapi` | `servant-reimagined-openapi` | Direct equivalent. Also supports Swagger 2.0. |
| `typeway-grpc` | `servant-reimagined-grpc` | Direct equivalent. GrpcCodec, .proto generation, mkGrpcServiceMap. |
| `typeway-protobuf` | (folded into `servant-reimagined-grpc`) | Haskell has `proto-lens`; less need for a custom codec package. |
| `typeway-macros` | (not needed) | Haskell has TypeLits for string literals and GHC.Generics for deriving. No proc macros needed for core functionality. TH may be used sparingly for convenience but is not architecturally required. |
| `typeway` (facade) | `servant-reimagined` | Direct equivalent. |
| `typeway-migrate` | (future work) | Migration tool from `servant` to `servant-reimagined`. |
| (no equivalent) | `tower` | **New library.** Rust has tower; Haskell does not have an equivalent. |
| (no equivalent) | `http-core` | **New library.** Rust has the `http` crate; Haskell has no backend-agnostic Request/Response types. |
| (no equivalent) | `tower-http` | **New library.** HTTP middleware built on `tower` + `http-core`. |
| (no equivalent) | `tower-grpc` | **New library.** gRPC wire protocol: framing, status, dispatch, multiplexing, reflection. |
| (no equivalent) | `tower-server` | **New library.** Tower-native HTTP/1.1 + HTTP/2 server. No WAI dependency. |
| (no equivalent) | `servant-reimagined-codegen` | Code generation from OpenAPI/Swagger/.proto specs to Haskell API types. |
| (no equivalent) | `servant-reimagined-test` | Testing utilities for REST and gRPC services. |

---

## What Haskell Gives Us for Free

Several things that typeway had to work around or build from scratch come
naturally in Haskell:

### Type-Level Strings

Typeway's biggest workaround — proc macros generating marker types for path
literals — is completely unnecessary. Haskell has `Symbol` from `GHC.TypeLits`:

```haskell
-- Typeway (Rust): needs proc macro
-- typeway_path!(type UsersPath = "users" / u32);

-- Haskell: native
type UsersPath = '[ Lit "users", Capture Int ]
```

### Promoted Data Constructors

Effects, methods, changes, session types — all naturally expressed as promoted
data kinds:

```haskell
data Effect = Auth | Cors | RateLimit | Tracing
-- GHC promotes these to types: 'Auth :: Effect, 'Cors :: Effect, etc.
```

Typeway has to define these as zero-sized structs with trait impls.

### Closed Type Families

The core compile-time computation engine. Closed type families evaluate by
top-to-bottom pattern matching with no backtracking — fundamentally more
efficient than Rust's trait resolution or Servant's open type class instances.

### Constraint Kinds

Effects-as-constraints is natural:

```haskell
type RequiresAuth api = HasEffect 'Auth (RequiredEffects api) ~ 'True
```

In Rust, this requires the `AllProvided` trait with index witnesses (`EHere`,
`EThere`).

### Higher-Kinded Types

The `Service` abstraction benefits from HKTs:

```haskell
newtype Service m req resp = Service { runService :: req -> m resp }
```

The `m` parameter lets the same `Service` type work with `IO`, `Handler`,
`ReaderT Env IO`, or any monad. Rust's Tower hardcodes `Future` as the
effect type.

### Linear Types (GHC 9.0+)

Session-typed WebSockets can use linear types for protocol enforcement:

```haskell
send :: a -> Session (Send a s) %1 -> IO (Session s)
recv :: Session (Recv a s) %1 -> IO (a, Session s)
```

The `%1` annotation means the `Session` value must be used exactly once.
Typeway relies on Rust's move semantics for this, which is equivalent but
less explicit.

---

## Compile-Time Budget

Following typeway's practice, we set explicit compile-time targets:

| Scenario | Target (clean build, -O0) |
|----------|--------------------------|
| `servant-reimagined-core` alone | < 3s |
| `tower` + `tower-http` | < 5s |
| `servant-reimagined-server` + core + tower | < 10s |
| Example app with 5 routes, no openapi | < 15s |
| Example app with 5 routes, all features | < 25s |
| Example app with 20 routes, all features | < 45s |

If any target is exceeded, investigate before adding features. The primary
risk is type family evaluation — profile with `-ddump-tc-trace` if compile
times grow unexpectedly.

---

## Build Order (Implementation Phases)

The dependency graph dictates the natural build order:

1. **`tower`** — standalone, no dependencies on the rest. Can be developed
   and released independently. This is useful to the Haskell ecosystem
   regardless of whether the rest of the project succeeds.

2. **`servant-reimagined-core`** — depends only on `base`. Pure type-level
   work. Get the API vocabulary right before building interpreters.

3. **`http-core`** — backend-agnostic Request/Response types. Depends on
   `base`, `bytestring`, `text`, `http-types`.

4. **`tower-http`** — depends on `tower` + `http-core`. Can be developed in
   parallel with the core once `tower` is stable.

5. **`tower-server`** — tower-native HTTP/1.1 + HTTP/2 server. Depends on
   `tower`, `http-core`, `http2`. No WAI.

6. **`tower-wai`** — WAI/warp adapter. Depends on `tower`, `http-core`, `wai`.

7. **`tower-grpc`** — gRPC wire protocol. Depends on `tower`, `http-core`.
   Provides framing, status, dispatch, multiplexing, and reflection.

8. **`servant-reimagined-server`** — depends on core + tower. The first
   "it works" milestone: define an API type, write handlers, run a server.

9. **`servant-reimagined-client`** — depends on core. Can be developed in
   parallel with the server.

10. **`servant-reimagined-openapi`** — depends on core. Can be developed in
    parallel with server and client. Supports OpenAPI 3.1 + Swagger 2.0.

11. **`servant-reimagined-grpc`** — depends on core + tower + tower-grpc.
    gRPC interpretation, GrpcCodec, .proto generation, mkGrpcServiceMap.

12. **`servant-reimagined-codegen`** — code generation from OpenAPI/Swagger/
    .proto specs to Haskell API types.

13. **`servant-reimagined-test`** — testing utilities for REST and gRPC.

14. **`servant-reimagined`** — facade. Trivial once the others exist.

---

## Gaps, Open Questions, and Missing Pieces

The architecture above covers the package decomposition but leaves several
critical design questions unresolved. This section addresses them.

### Gap 1: Where Does Tower Sit Relative to WAI and Warp?

This is the most important architectural question we hadn't fully answered.

**The Rust stack** has three distinct layers:

| Layer | Rust | Responsibility |
|-------|------|----------------|
| HTTP engine | hyper | TCP/TLS listening, HTTP parsing, connection lifecycle, HTTP/1.1 + HTTP/2 |
| Composition | tower | Abstract Service/Layer middleware composition |
| Framework | axum / typeway | Routing, extractors, handler dispatch, API types |

**The Haskell stack today** collapses the first two:

| Layer | Haskell | Responsibility |
|-------|---------|----------------|
| HTTP engine + interface | warp + WAI | TCP/TLS, HTTP parsing, connections, AND the application interface |
| Framework | servant | Routing, handlers, API types |

WAI's `Application` type (`Request -> (Response -> IO ResponseReceived) ->
IO ResponseReceived`) serves double duty as both the HTTP engine's callback
interface and the middleware composition layer. There is no separate
composition abstraction.

**Our answer: tower replaces WAI as the composition layer. Warp stays as the
HTTP engine. A new adapter package bridges them.**

```
┌──────────────────────────────────────┐
│   servant-reimagined-server          │
│   (routing, extractors, handlers)    │
└──────────────┬───────────────────────┘
               │ produces a
               ▼
┌──────────────────────────────────────┐
│   tower Service IO Request Response  │
│   (middleware composition happens    │
│    here via Layers)                  │
└──────────────┬───────────────────────┘
               │ adapted by
               ▼
┌──────────────────────────────────────┐
│   tower-wai (adapter)               │
│   toWaiApp / fromWaiMiddleware      │
└──────────────┬───────────────────────┘
               │ consumed by
               ▼
┌──────────────────────────────────────┐
│   warp (HTTP engine)                │
│   TCP, TLS, HTTP parsing, H2       │
└──────────────────────────────────────┘
```

This means we need a new package:

### New Package: `tower-wai` — WAI/Warp Backend Adapter

**Purpose:** One of potentially many backend adapters. This one bridges
our `http-core` types and tower Services to WAI/warp. It is the **only
package that knows about WAI.** Swapping warp for a different backend
means replacing this package with a different adapter — nothing above
changes.

Also provides a migration bridge for existing WAI middleware, allowing
it to be used as tower Layers during incremental adoption.

**Depends on:** `tower`, `http-core`, `wai`, `warp`, `bytestring`

**Depended on by:** User applications that choose warp as their backend.
Note that `servant-reimagined-server` does NOT depend on this — it produces
a `tower Service` and is backend-agnostic.

**Key functions:**

```haskell
-- | Run a tower Service on warp.
-- This is the primary entry point for warp-based applications.
runWarp
  :: Warp.Settings
  -> Service IO (Request StrictBody) (Response StrictBody)
  -> IO ()

-- | Run with graceful shutdown.
runWarpWithShutdown
  :: Warp.Settings
  -> Service IO (Request StrictBody) (Response StrictBody)
  -> IO ()  -- shutdown signal
  -> IO ()

-- | Convert to a WAI Application (for embedding in existing WAI apps).
toWaiApp
  :: Service IO (Request StrictBody) (Response StrictBody)
  -> Wai.Application

-- | Convert a WAI Application into a tower Service
-- (for embedding existing WAI apps in a tower stack).
fromWaiApp
  :: Wai.Application
  -> Service IO (Request StrictBody) (Response StrictBody)

-- | Wrap existing WAI Middleware as a tower Layer.
-- Migration bridge: use wai-extra, wai-cors, etc. without rewriting.
fromWaiMiddleware
  :: Wai.Middleware
  -> Middleware IO (Request StrictBody) (Response StrictBody)

-- | Convert a tower Middleware to WAI Middleware
-- (for contributing tower Layers to WAI-based apps).
toWaiMiddleware
  :: Middleware IO (Request StrictBody) (Response StrictBody)
  -> Wai.Middleware

-- | Internal: convert between our types and WAI types.
fromWaiRequest :: Wai.Request -> IO ByteString -> IO (Request StrictBody)
toWaiResponse  :: Response StrictBody -> Wai.Response
```

**Why not just use WAI directly?** Because WAI's `Middleware` type
(`Application -> Application`) is untyped — it's just function composition
with no structure. Tower's `Layer` carries type information, composes with
an explicit operator, and can be extended with typed effects. The WAI adapter
exists for interop, not as the primary interface. See "The Two-Layer
Middleware Model" for the full picture of how tower-wai fits at the bottom
of the stack, well below where any typed middleware operates.

**Why not replace warp entirely?** Warp is battle-tested, handles HTTP/2,
TLS, connection draining, and thousands of edge cases. Rewriting it would
be years of work for no architectural benefit. The right boundary is: warp
handles bytes-on-the-wire, tower handles generic middleware composition,
our framework handles typed per-endpoint dispatch and effect verification.

**Could you write a different adapter?** Yes. That's the point. A
`tower-snap` for the Snap framework, a `tower-direct` that uses raw
sockets with `http2` and `network`, or a `tower-lambda` for AWS Lambda —
each would be a small adapter package that converts our types and plugs
into a different runtime. Nothing above the adapter changes.

---

### Gap 2: The Handler Monad

What monad do handlers run in? This is one of Servant's most painful design
decisions — `Handler` is `ExceptT ServerError IO`, which forces `liftIO`
everywhere and makes error handling awkward.

**Typeway's answer:** Handlers are plain `async fn` — no monad choice because
Rust only has one async model. Shared state goes through extractors
(`State<T>`), not through a reader monad.

**Our answer: Handlers run in IO. No monad transformer stack.**

```haskell
-- Handlers are just IO functions. No ExceptT, no ReaderT, no lift.
getUser :: PathCapture UserByIdPath -> AppState DbPool -> IO (Either AppError (Json User))
getUser (PathCapture userId) (AppState pool) = do
  result <- Pool.withResource pool $ \conn ->
    DB.getUser conn userId
  case result of
    Nothing   -> pure (Left (notFound "user not found"))
    Just user -> pure (Right (Json user))
```

**Rationale:**

- **No `liftIO`.** Handlers are `IO` — every IO action works directly. This
  is the #1 ergonomic complaint about Servant's `Handler` monad.

- **State via extractors, not ReaderT.** Shared state (DB pools, config,
  caches) is injected via `AppState` extractors, not via a reader environment.
  This is the pattern proven by typeway and axum. It composes better because
  each handler declares exactly what state it needs.

- **Errors via return types, not `throwError`.** Handlers return
  `IO (Either AppError a)` or `IO a` (for infallible handlers). The
  `IntoResponse` class handles both. No `throwError`/`catchError` machinery.

- **Exceptions for truly exceptional cases.** If a handler throws a runtime
  exception, the framework catches it and returns 500. This is the panic
  recovery pattern from typeway's `production.rs`. Handlers should use
  `Either` for expected errors and let exceptions propagate for bugs.

- **The `m` parameter on `Service` still exists.** The tower `Service` type
  is parameterized by `m`, so middleware authors can write polymorphic layers.
  But the HTTP handler layer specifically uses `IO`. Users who want `ReaderT`
  can wrap their handlers manually — the framework does not force a transformer
  stack on everyone.

**What about users who want ReaderT?** They can write a thin wrapper:

```haskell
type App = ReaderT AppEnv IO

runApp :: AppEnv -> App a -> IO a
runApp = flip runReaderT

-- Handler using ReaderT — just use runApp at the boundary
getUser :: PathCapture UserByIdPath -> IO (Json User)
getUser (PathCapture userId) = runApp myEnv $ do
  pool <- asks dbPool
  -- ...
```

This is opt-in, not forced. Most handlers don't need it.

---

### Gap 3: Request and Response Types

Do we define our own `Request`/`Response` types or use WAI's?

**Revised answer: Define our own types in `http-core`.** The original
proposal was to use WAI's types for ecosystem interop. But that pins the
entire stack to WAI — swapping the backend means rewriting every middleware
and extractor. See "Backend Agnosticism: The HTTP Types Question" above.

Our types use `http-types` primitives (Method, Status, Header) but are not
WAI types. The only package that imports `Network.Wai` is `tower-wai`.

For the extractor protocol, the full request splits into Parts + Body,
mirroring typeway's design (borrowed from axum):

```haskell
-- RequestParts: the non-body parts of the request, used by extractors.
-- FromRequestParts extracts from Parts (can be called multiple times).
-- FromRequest extracts from the body (consumed once).
data RequestParts = RequestParts
  { rpMethod     :: !Method
  , rpPath       :: ![Text]
  , rpQuery      :: !Query
  , rpHeaders    :: !RequestHeaders
  , rpExtensions :: !(IORef Extensions)
  }

splitRequest :: Request StrictBody -> IO (RequestParts, ByteString)
```

WAI interop is handled by `tower-wai`'s conversion functions
(`fromWaiRequest`, `toWaiResponse`), not by sharing types.

---

### Gap 4: Streaming Responses and SSE

The architecture mentions responses but not streaming. Typeway supports:
- `body_from_stream` — streaming response from any async stream
- `sse_body` — Server-Sent Events helper
- WebSocket streaming via session types

**Our answer: Streaming is a first-class response type.**

```haskell
-- Streaming response body (chunked transfer encoding)
newtype StreamBody = StreamBody (ConduitT () ByteString IO ())

instance IntoResponse StreamBody where
  intoResponse (StreamBody src) = responseSource status200 headers src

-- SSE (Server-Sent Events)
newtype EventStream = EventStream (ConduitT () ServerEvent IO ())

data ServerEvent = ServerEvent
  { eventName :: !(Maybe Text)
  , eventData :: !Text
  , eventId   :: !(Maybe Text)
  }

instance IntoResponse EventStream where
  intoResponse (EventStream src) =
    responseSource status200 [("Content-Type", "text/event-stream")] (src .| formatSSE)
```

This belongs in `servant-reimagined-server` under a `Streaming` module.
We use `conduit` for streaming since it composes well and handles resource
cleanup.

---

### Gap 5: Static File Serving and SPA Fallback

Typeway provides `with_static_files` and `with_spa_fallback` on the server
builder. We need the same for full-stack applications.

**Our answer: These are server builder methods.**

```haskell
-- Serve static files from a directory
withStaticFiles :: FilePath -> Text -> Server api provided -> Server api provided
-- withStaticFiles "./public" "/static"

-- SPA fallback: serve index.html for unmatched routes (client-side routing)
withSpaFallback :: FilePath -> Server api provided -> Server api provided
-- withSpaFallback "./public/index.html"
```

These could also be implemented as tower-http Layers, but builder methods
are more ergonomic for the common case.

---

### Gap 6: Graceful Shutdown

Production servers need graceful shutdown: stop accepting new connections,
let in-flight requests finish, then exit.

**Our answer: Part of the server runner, not the framework.**

```haskell
-- Run with graceful shutdown on SIGTERM/SIGINT
runServerWithShutdown
  :: Warp.Settings
  -> Server api provided
  -> IO ()  -- returns when shutdown signal received + in-flight requests drained
```

This delegates to warp's `setInstallShutdownHandler` and
`setGracefulShutdownTimeout`. The framework does not need to reimplement
connection lifecycle management.

---

### Gap 7: Exception Safety and Panic Recovery

Typeway catches handler panics and returns 500 without crashing the server.
We need the same.

**Our answer: A tower Layer for exception catching.**

```haskell
-- In tower-http or servant-reimagined-server:
exceptionRecoveryLayer :: Middleware IO Request Response
-- Catches SomeException from the inner service, logs it, returns 500.
-- Individual request failures are isolated from the server process.
```

This is naturally a tower `Middleware` — it wraps the service and catches
exceptions. It should be in the default middleware stack.

---

### Gap 8: Testing Utilities

Neither Servant nor typeway has a dedicated testing package. This is a gap
we should fill.

**New package: `servant-reimagined-test`**

**Purpose:** Testing utilities for APIs defined with servant-reimagined.
Spin up servers, make type-safe requests, assert on responses.

**Depends on:** `servant-reimagined-core`, `servant-reimagined-server`,
`servant-reimagined-client`, `hspec` or `tasty`, `warp` (for test server)

```haskell
-- Run a test against a server on a random port
withTestServer
  :: Server api provided
  -> (Port -> IO a)
  -> IO a

-- Direct testing without network (wai-test style)
testRequest
  :: Server api provided
  -> Wai.Request
  -> IO Wai.Response

-- Type-safe testing via the client
withTestClient
  :: Server api provided
  -> (Client api -> IO a)
  -> IO a

-- Response assertions
shouldHaveStatus :: Wai.Response -> Status -> Expectation
shouldHaveHeader :: Wai.Response -> HeaderName -> ByteString -> Expectation
shouldHaveJsonBody :: (FromJSON a, Eq a, Show a) => Wai.Response -> a -> Expectation
```

This package should also support property-based testing:

```haskell
-- Generate valid requests for any endpoint in the API
class ArbitraryRequest endpoint where
  arbitraryRequest :: Gen Wai.Request
```

---

### Gap 9: WAI Ecosystem Interop (Migration Bridge)

For adoption, we need to interop with the existing WAI ecosystem. People
have years of investment in WAI middleware (wai-cors, wai-extra, wai-logger,
etc.). They won't switch if they have to rewrite everything.

**Covered by `tower-wai`** (Gap 1 above), specifically `fromWaiMiddleware`.
This wraps any `Wai.Middleware` as a `tower Layer`, enabling incremental
migration:

```haskell
-- Use existing WAI middleware as tower Layers
import qualified Network.Wai.Middleware.Gzip as Wai

myServer :: Server API '[]
myServer = serve handlers
  & applyLayer (fromWaiMiddleware Wai.gzip)  -- existing WAI middleware
  & applyLayer corsLayer                      -- new tower-http Layer
```

---

### Gap 10: Servant Migration Path

Users coming from Servant need a migration path. This is not just tooling —
it's type-level interop.

**Future package: `servant-reimagined-servant-interop`**

```haskell
-- Convert a Servant API type to our API type
type family FromServantAPI (api :: Type) :: [Type]

-- Convert our API type to a Servant API type (for gradual migration)
type family ToServantAPI (api :: [Type]) :: Type

-- Run a servant-reimagined handler tuple as a Servant server
toServantServer :: Server api provided -> Servant.Server (ToServantAPI api)

-- Embed a Servant sub-API inside a servant-reimagined server
nestServant :: Servant.Server subApi -> ???
```

This is hard to get right and should come after the core is stable. But it
needs to be designed for from the start — decisions we make in the core
types affect whether this interop is possible.

---

### Gap 11: Error Handling Strategy

Errors happen at multiple layers. The strategy needs to be coherent across
all of them:

| Layer | Error Source | Handling |
|-------|-------------|----------|
| Path parsing | Capture parse failure | 404 Not Found |
| Extraction | Missing/malformed header, query, body | 400/422 with structured JSON error |
| Validation | `Validated` check fails | 422 Unprocessable Entity |
| Auth | Missing/invalid credentials | 401/403 |
| Handler | Business logic error | Handler returns `Either AppError a` |
| Handler | Unexpected exception | Caught by recovery layer → 500 |
| Middleware | Timeout, rate limit | 408/429 from the Layer |
| Router | No matching route | 404 |
| Router | Route matches but wrong method | 405 Method Not Allowed |

**Key design decision: structured error responses.**

```haskell
data ServerError = ServerError
  { seStatus  :: !Status
  , seMessage :: !Text
  , seDetails :: !(Maybe Value)  -- optional structured details
  }

instance IntoResponse ServerError where
  intoResponse se = responseLBS (seStatus se)
    [("Content-Type", "application/json")]
    (encode $ object
      [ "error" .= object
        [ "status"  .= statusCode (seStatus se)
        , "message" .= seMessage se
        , "details" .= seDetails se
        ]
      ])
```

Extractors, validators, auth, and handlers all produce `ServerError`. The
framework converts them uniformly. Users can define custom error types via
`IntoResponse`.

---

### Gap 12: TLS and HTTP/2

Typeway supports TLS via `tokio-rustls` and HTTP/2 auto-detection via hyper.

**Our answer: Delegate to warp.** Warp already supports TLS (via
`warp-tls`) and HTTP/2. Since we run on warp (via `tower-wai`), we get
these for free. The server builder should expose TLS configuration:

```haskell
runServerTLS :: Warp.TLSSettings -> Warp.Settings -> Server api provided -> IO ()
```

---

### Gap 13: Observability Beyond Tracing

`tower-http` has `TraceLayer` for request/response logging. But production
observability also needs:
- **Metrics** — request count, latency histograms, error rates
- **Distributed tracing** — propagate trace IDs across services

**Our answer: Metrics as a tower Layer.**

```haskell
-- In tower-http:
metricsLayer :: MetricsConfig -> Middleware IO Request Response
-- Records request count, latency, status code distribution.
-- Exposes a /metrics endpoint (Prometheus format) or pushes to a backend.

-- Distributed tracing:
tracePropagationLayer :: Middleware IO Request Response
-- Extracts trace-id / span-id from incoming headers (W3C Trace Context
-- or B3), makes them available via request extensions, propagates to
-- outgoing requests.
```

---

## Revised Package List

Including all gaps, the complete package set is:

| # | Package | Purpose | Status |
|---|---------|---------|--------|
| 1 | `tower` | Service/Layer composition (protocol-agnostic) | New library |
| 2 | `http-core` | Backend-agnostic Request/Response types | New library |
| 3 | `tower-http` | HTTP-specific middleware (uses http-core types) | New library |
| 4 | `tower-wai` | WAI/warp backend adapter (only WAI-aware package) | New library |
| 5 | `servant-reimagined-core` | Type-level API specification | Core |
| 6 | `servant-reimagined-server` | HTTP server interpretation (backend-agnostic) | Core |
| 7 | `servant-reimagined-client` | HTTP client interpretation | Core |
| 8 | `servant-reimagined-openapi` | OpenAPI spec generation | Core |
| 9 | `servant-reimagined-grpc` | gRPC support | Core |
| 10 | `servant-reimagined-test` | Testing utilities | Core |
| 11 | `servant-reimagined` | Facade / re-exports | Core |
| 12 | `servant-reimagined-servant-interop` | Servant migration bridge | Future |

## Revised Dependency Graph

```
                         ┌──────────────────────────┐
                         │   servant-reimagined     │
                         │       (facade)           │
                         └──┬───┬────┬────┬───┬────┘
                            │   │    │    │   │
            ┌───────────────┘   │    │    │   └───────────┐
            │      ┌────────────┘    │    └─────┐         │
            ▼      ▼                 ▼          ▼         ▼
     ┌────────┐ ┌────────┐ ┌──────────────┐ ┌──────┐ ┌──────┐
     │ server │ │ client │ │   openapi    │ │ grpc │ │ test │
     └───┬────┘ └───┬────┘ └──────┬───────┘ └──┬───┘ └──┬───┘
         │          │             │             │        │
         │          └─────┬───────┴─────────────┘        │
         │                │                              │
         │                ▼                              │
         │   ┌────────────────────────┐                  │
         ├──►│ servant-reimagined-core │◄─────────────────┘
         │   └────────────────────────┘
         │
         ├──────────────────┐
         ▼                  ▼
  ┌──────────────┐   ┌──────────────┐
  │  tower-http  │   │  http-core   │◄──────────────────────┐
  └──────┬───────┘   └──────┬───────┘                       │
         │                  │                                │
         ▼                  ▼                                │
  ┌────────────┐     ┌────────────┐                         │
  │   tower    │     │            │    Backend adapters      │
  └────────────┘     │ tower-wai  │───► warp (external)     │
                     │ tower-snap │───► snap (external)     │
                     │ tower-xyz  │───► ...                  │
                     └────────────┘                         │
                           │                                │
                           └────────────────────────────────┘
                                  (each adapter depends on
                                   http-core + tower + the backend)
```

**Key property: the dotted line.** Everything above `http-core` and `tower`
is backend-agnostic. The choice of warp, snap, raw sockets, or AWS Lambda
is made at the bottom of the stack by selecting which adapter package to
depend on. Nothing above the adapter changes.

**`servant-reimagined-server` does NOT depend on `tower-wai` or `warp`.**
It produces a `Service IO (Request StrictBody) (Response StrictBody)` and
is done. The user's application depends on a backend adapter to run that
service.

## Revised Build Order

```
Phase 1 (foundation, parallel):                              ✅ DONE
  tower                      — 3 modules, 15 tests
  http-core                  — 4 modules, 27 tests
  servant-reimagined-core    — 10 modules, 20+ compile-time assertions

Phase 2 (middleware + first adapter, parallel):              ✅ DONE
  tower-http                 — 4 modules, 17 tests
  tower-wai                  — 1 module, 11 tests

Phase 3 (first working server):                              ✅ DONE
  servant-reimagined-server  — 7 modules, 22 tests
  Includes: extractors, responses, routing, automatic wiring,
  EffectfulServer with phantom-type effect tracking.

Phase 4 (interpretations, parallel):                          ✅ DONE
  servant-reimagined-client  — 3 modules, 13 tests
  servant-reimagined-openapi — 3 modules, 22 tests

Phase 5 (native HTTP server + middleware):                    ✅ DONE
  tower-server               — 5 modules, 21 tests (HTTP/1.1, TLS, streaming)
  tower-http (expanded)      — +3 modules: CORS, compression, timeout

Phase 6 (polish + testing):                                   ✅ DONE
  servant-reimagined         — 2 modules (facade)
  servant-reimagined-test    — 1 module, 6 tests
  examples/hello-world       — end-to-end example app
  benchmarks/compile-time    — scaling verification

Remaining:
  servant-reimagined-grpc
  servant-reimagined-servant-interop

Phase N (community / future):
  tower-snap, tower-direct, tower-lambda, ...  — alternative backends

Totals: 12 packages, 48 modules, 200+ tests, 10 test suites, ~7s clean build.
```

## Summary of Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Handler monad | `IO` (no transformer stack) | Eliminates liftIO, simpler than Servant's ExceptT |
| State passing | Extractors (`AppState`), not ReaderT | Explicit, composable, proven by typeway/axum |
| Error handling | `Either AppError a` return types | Explicit, no throwError. Exceptions for bugs only. |
| Request/Response types | Own types in `http-core` | Backend-agnostic. WAI confined to adapter. Enables backend swapping. |
| Tower vs WAI | Tower replaces WAI for composition | Typed, composable. WAI adapter for interop. |
| Backend | Pluggable via adapter packages | Default: tower-wai (warp). Swappable without changing framework code. |
| Streaming | Conduit-based | Composes well, handles cleanup |
| Testing | Dedicated package | Neither Servant nor typeway has this |
| Servant interop | Future package, but design for it now | Adoption depends on migration path |
