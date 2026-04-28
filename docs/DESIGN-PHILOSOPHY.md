# Design Philosophy

## The core idea

One type drives everything. Define your API as a Haskell type once, and
the framework derives the REST server, gRPC server, OpenAPI spec, HTTP
client, .proto file, and test harness from it. No drift, no duplication,
no runtime reflection.

```
                    ┌─────────────────────┐
                    │   type API = '[ ... ]│
                    └──────────┬──────────┘
                               │
          ┌────────────────────┼────────────────────┐
          │                    │                     │
          ▼                    ▼                     ▼
    REST server          gRPC server           OpenAPI spec
    (mkApi @API)     (mkGrpcServiceMap @API)  (generateSpec @API)
          │                    │                     │
          ▼                    ▼                     ▼
    spire Service        spire Service          JSON document
          │                    │
          ▼                    ▼
    ┌──────────┐    ┌──────────────┐
    │spire-wai │    │ spire-server │
    │  (warp)  │    │ (HTTP/1.1+2) │
    └──────────┘    └──────────────┘
```

## Why not Servant?

Servant proved the idea works. But it has structural problems:

**Recursive instance resolution over `:<|>`.** Servant's `:<|>` tree is
a nested binary type. `HasServer (a :<|> b)` decomposes into
`HasServer a` and `HasServer b` and recurses, with constraint solving
walking the whole tree. Modern Servant (0.20.x) handles this fine for
trivial routing tables at the sizes we benchmark, but historically
this design has produced the long compile times reported in the
community for richer APIs (captures, query params, request bodies,
JSON-derived response types) and at larger endpoint counts. The
structural risk is an open recursive instance chain that GHC cannot
short-circuit.

**No shared middleware layer.** Every Haskell web framework has its own
middleware abstraction (or none). There's no equivalent of Rust's tower
crate (a composable, framework-agnostic Service/Layer pattern) that
works for web servers, gRPC, message queues, and anything else.

**No compile-time middleware enforcement.** Servant has no way to verify
at compile time that auth middleware is present for endpoints that need
it. You deploy, get a 500, and debug.

**Monolithic handler monad.** `Handler = ExceptT ServerError IO`. Every
handler carries this stack. Want state? Add `ReaderT`. Want logging?
Another transformer. Each layer adds `liftIO` noise and makes types
harder to read.

## How we fix each problem

### Problem: Open recursive instance resolution over `:<|>`
### Fix: Flat lists + closed type families + flat tuple indexing

APIs are promoted lists, not trees:

```haskell
-- Servant (right-nested tree, instance resolution recurses):
type API = A :<|> B :<|> C :<|> D :<|> E

-- acolyte (flat list, single instance match per arity):
type API = '[ A, B, C, D, E ]
```

Handler wiring uses flat tuple instances. `BuildApi` has one instance
per arity (1-25), each matching directly on the tuple and API list
structure. No recursion, no backtracking. GHC does O(1) work per
instantiation.

`Serves` is a constraint synonym:
```haskell
type Serves api handlers = CheckArity (Length api) (TupleArity handlers)
```

Both `Length` and `TupleArity` are closed type families that reduce in
a single pass. `CheckArity` either reduces to `()` or fires a
`TypeError`. No instance resolution at all.

**Measured result:** 1 to 32 endpoints compile in roughly the same
wall-clock time (~1.63s on an Apple Silicon laptop in the head-to-head
bench, against ~1.79s for Servant on the same hardware and the same
trivial API shape). Both libraries are flat at this scale; acolyte's
structural choices keep instance resolution at O(1) per endpoint, so
there is no recursive chain to slope as APIs grow richer.

### Problem: No shared middleware layer
### Fix: spire (a standalone Haskell library)

```haskell
newtype Service m req resp = Service { runService :: req -> m resp }
newtype Layer m reqI respI reqO respO = Layer
  { applyLayer :: Service m reqI respI -> Service m reqO respO }
type Middleware m req resp = Layer m req resp req resp
```

Three types. That's the entire abstraction. Middleware composes with
`|>`:

```haskell
server |> cors |> tracing |> secureHeaders
```

spire has no HTTP knowledge. http-core has no WAI knowledge. The server
produces a Service; the backend adapter consumes it. Swap spire-wai for
spire-server (or any other adapter that consumes the same boundary).
Nothing above the adapter changes.

### Problem: No compile-time middleware enforcement
### Fix: Phantom-type effect tracking

```haskell
data EffectfulServer (api :: [Type]) (provided :: [Type])

provide :: forall e. Middleware ... -> EffectfulServer api provided
                                    -> EffectfulServer api (e ': provided)

run :: AllEffectsProvided api provided
    => EffectfulServer api provided -> Service ...
```

`provided` is a type-level list that grows with each `provide` call.
`run` only compiles when every `Requires` in the API has been provided.
The check uses two type families:

1. `RequiredEffects` walks the API and collects all effect tags
2. `AllIn` checks each tag against the provided list

Both are closed type families: no instance resolution, no overlapping
pragmas, no ambiguity.

### Problem: Monolithic handler monad
### Fix: Plain IO + extractors

Handlers are plain functions:
```haskell
getUser :: PathCapture Int -> IO (Json User)
getUser (PathCapture uid) = ...
```

No `Handler` monad. No `liftIO`. State comes from `AppState` extractors.
Errors come from `Either` return types. Each extractor is a
`FromRequestParts` instance that pulls data from the request: path
captures, query params, headers, JSON body, form data, multipart uploads.

`ToHandler` converts any function whose arguments are
`FromRequestParts` extractors and whose return is `IntoResponse` into
the internal `HandlerFn`. `mkApi` calls `toHandler` internally, so
users never see it.

## The two-layer middleware model

There are two fundamentally different kinds of middleware:

```
Layer 1: Generic (spire)          Layer 2: Per-endpoint (type wrappers)
─────────────────────────         ────────────────────────────────────
CORS, compression, tracing,       Protected, Validated, Versioned,
timeouts, secure headers.         Requires, WithParams, Describe.

Wrap Service → Service.           Wrap endpoint types in the API list.
No knowledge of endpoints.        Full type-level information.
Composed with |>.                 Part of the API type definition.
Effect-tracked via phantoms.      Transparent to routing.
```

Layer 1 operates on HTTP requests and responses. It doesn't know what
endpoint matched or what the handler signature is.

Layer 2 operates at the type level. `Requires Auth` adds a compile-time
check. `WithParams` documents query parameters. `Describe` adds OpenAPI
descriptions. These wrappers are transparent to routing: `HasEndpointInfo`
delegates through them to the inner endpoint.

## Backend agnosticism

```
             ┌─────────────────────────────────┐
             │         Your application          │
             │    mkApi @API (h1, h2, h3)        │
             └──────────────┬──────────────────┘
                            │ Service IO (Request Body) (Response Body)
          ┌─────────────────┼───────────────────┐
          │                 │                    │
    ┌─────▼──────┐   ┌─────▼──────┐
    │  spire-wai  │   │spire-server│
    │   (warp)    │   │(HTTP/1.1+2)│
    └────────────┘   └────────────┘
```

The boundary is `Service IO (Request Body) (Response Body)`. Everything
above it is portable. Everything below it is a backend adapter. The
project ships two: `spire-wai` (warp) and `spire-server` (zero WAI,
HTTP/1.1+2 over raw sockets). New backends (Lambda, custom transports)
are written by implementing the same boundary. WAI types never leak
above spire-wai.

## Type-level annotation composability

All endpoint wrappers compose by nesting:

```haskell
type CreateUser =
  Describe "Create a new user"
    (WithParams '[QP "invite" Text]
      (WithHeaders '[HH "Authorization" Text]
        (Requires Auth
          (RespondsWith 201
            (Post (At "users") (Json CreateUserReq) (Json User))))))
```

Every wrapper delegates `HasEndpointInfo` to the inner endpoint. The
server ignores the documentation wrappers. OpenAPI generation reads
them. gRPC generation reads streaming markers. The effect system reads
`Requires`. Each consumer takes what it needs and ignores the rest.

## Performance budget

| Operation | Budget | Actual |
|-----------|--------|--------|
| Compile 1 endpoint (trivial) | < 2s | ~1.63s |
| Compile 32 endpoints (trivial) | < 2s | ~1.65s |
| Compile 32 endpoints (rich: capture + JSON body) | < 3s | ~2.01s |
| Compile 32 endpoints (Servant 0.20.3.0, trivial) | - | ~1.82s |
| Compile 32 endpoints (Servant 0.20.3.0, rich) | - | ~2.24s |
| Type family reduction (Serves) | O(1) | O(1) (constraint synonym) |
| Type family reduction (AllEffectsProvided) | O(n*m) | n=endpoints, m=effects |
| Instance resolution (BuildApi) | O(1) | Flat instances, no recursion |
| Handler dispatch (runtime) | O(n) | Linear scan of routes |
| Middleware application | O(k) | k = number of layers |

The design principle: **the compiler should not do work proportional to
the number of endpoints squared.** Every type-level operation is either
constant (flat instances, constraint synonyms) or linear (closed type
family traversal). Nothing has worse-than-linear scaling.
