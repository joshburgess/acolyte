# servant-reimagined

A composable, type-safe web framework for Haskell where the API is a type,
the middleware is tracked at compile time, and the backend is pluggable.

Built on GHC 9.10.3. Informed by Rust's tower/axum/typeway ecosystem.

## What this is

Six packages that together provide something the Haskell ecosystem has
never had: a layered, composable web framework architecture where each
layer is independent and swappable.

```
servant-reimagined-server    (framework: API types -> handlers -> tower Service)
         |
   tower / tower-http        (composition: Service/Layer/Middleware)
         |
      http-core              (shared types: Request/Response/Extensions)
         |
      tower-wai              (adapter: runs on warp — swappable)
         |
       warp                  (HTTP engine: TCP, TLS, HTTP/2)
```

## Quick example

```haskell
import Servant.Reimagined.Core
import Servant.Reimagined.Server
import Tower
import Tower.Http (secureHeadersLayer, defaultSecureHeaders)
import Tower.Wai (runWarp)

-- 1. Define the API as a type
type HealthPath = '[ 'Lit "health" ]
type UsersPath  = '[ 'Lit "users" ]

type API =
  '[ Get HealthPath   Text
   , Get UsersPath    (Json [Text])
   ]

-- 2. Wire handlers (compile-time checked: wrong count = type error)
server = mkServer @API
  ( wrapHandler @(Get HealthPath Text)
      (mkHandler0 (pure ("ok" :: Text)))
  , wrapHandler @(Get UsersPath (Json [Text]))
      (mkHandler0 (pure (Json ["alice", "bob"])))
  )

-- 3. Add middleware via tower's |> operator
app = server |> secureHeadersLayer defaultSecureHeaders

-- 4. Run on warp (or any other backend adapter)
main = runWarp 3000 app
```

## With typed middleware effects

```haskell
-- API declares what middleware each endpoint requires
type EffectAPI =
  '[ Requires Auth (Get UserPath (Json User))  -- needs auth
   , Get HealthPath Text                        -- no requirements
   ]

-- Server builder tracks effects as phantom types.
-- Missing 'provide' = compile error.
app = run
    $ provide @Auth authMiddleware
    $ effectfulServer @EffectAPI (authHandler, healthHandler)
```

Forget to provide auth? The compiler tells you:

```
Missing middleware effect: Auth
This effect is required by an endpoint but was not provided.
Add .provide @Auth to the server builder.
```

## Packages

| Package | Purpose | Depends on |
|---------|---------|------------|
| [`servant-reimagined-core`](servant-reimagined-core/) | Type-level API: endpoints, paths, effects, sessions, versioning | `base` only |
| [`tower`](tower/) | Service/Layer/Middleware composition | `base` only |
| [`http-core`](http-core/) | Backend-agnostic Request/Response/Extensions | `base`, `http-types` |
| [`tower-http`](tower-http/) | HTTP middleware: security headers, request ID, tracing | `tower`, `http-core` |
| [`tower-wai`](tower-wai/) | WAI/warp backend adapter (the only WAI-aware package) | `tower`, `http-core`, `wai`, `warp` |
| [`tower-server`](tower-server/) | Tower-native HTTP/1.1 server — zero WAI dependency | `tower`, `http-core`, `network` |
| [`servant-reimagined-server`](servant-reimagined-server/) | Handler dispatch, routing, extractors, EffectfulServer | `core`, `tower`, `http-core` |
| [`servant-reimagined-client`](servant-reimagined-client/) | Type-safe HTTP client derived from API types | `core`, `http-client` |
| [`servant-reimagined-openapi`](servant-reimagined-openapi/) | OpenAPI 3.1 spec generation from API types | `core`, `aeson` |
| [`servant-reimagined-test`](servant-reimagined-test/) | Testing utilities (direct dispatch, assertions) | `core`, `server`, `http-core` |
| [`servant-reimagined`](servant-reimagined/) | Facade — re-exports everything | all of the above |

## Design philosophy

**Composable over monolithic.** Every package has a clear boundary and
minimal dependencies. `tower` knows nothing about HTTP. `http-core` knows
nothing about WAI. The server produces a tower `Service` — it doesn't know
or care what runs it.

**Pluggable backends.** WAI and warp are confined to `tower-wai`. Swap it
for `tower-snap`, `tower-lambda`, or raw sockets — nothing above the
adapter changes.

**Compile-time correctness.** Missing handler? Type error. Missing
middleware? Type error. Wrong handler arity? Type error. Breaking API
change? Type error.

**Fast compilation.** Promoted lists instead of recursive `:<|>` trees.
Closed type families instead of open instances. Flat tuple indexing instead
of recursive constraint solving. Benchmarked: 1 to 16 endpoints compiles
in constant time (0.16–0.17s). No exponential blowup.

**IO handlers, not monadic stacks.** No `ExceptT`, no `liftIO`. Handlers
are plain `IO` functions. State via extractors, errors via `Either`.

## Two-layer middleware model

There are two fundamentally different kinds of middleware, operating at
different levels:

**Layer 1 — Generic middleware (tower Layers):** CORS, compression,
tracing, timeouts. These wrap the entire server as
`Service -> Service`. They know nothing about endpoint types. The effect
system tracks them via phantom types on the builder.

**Layer 2 — Per-endpoint typed wrappers:** `Protected`, `Validated`,
`Versioned`, `Requires`. These are type-level wrappers on individual
endpoints in the API type. They modify handler dispatch with full type
information — auth enforcement, body validation, version prefixing.

Users never drop to raw WAI. All typed middleware lives above the tower
boundary where endpoint type information is available.

## Current status

**Phases 1-6 complete.** The full framework is built and working:

- Type-level API definition with compile-time completeness checks
- Service/Layer/Middleware composition with `|>` piping
- Backend-agnostic Request/Response types with streaming body support
- HTTP middleware: security headers, request ID, tracing, CORS,
  compression (gzip), timeouts
- Two backend adapters: `tower-wai` (warp) and `tower-server` (zero WAI)
- TLS support in tower-server
- Automatic handler wiring from API types
- EffectfulServer with phantom-type effect tracking
- Type-safe HTTP client derived from API types
- OpenAPI 3.1 spec generation from API types
- Testing utilities (direct dispatch, no network needed)
- Compile-time benchmarks confirming linear scaling
- 12 packages, 48 modules, 200+ tests, 10 test suites passing

**Remaining:**

- `servant-reimagined-grpc` — unified REST + gRPC serving
- HTTP/2 support in tower-server (types defined, implementation pending)
- Session-typed WebSocket runtime enforcement (LinearTypes)

## Example

See [`examples/hello-world`](examples/hello-world/) for a complete
application that defines an API with effects, wires handlers, stacks
middleware (CORS, security headers, tracing, request ID), and runs on
tower-server.

```sh
cabal run hello-world
# Then: curl http://localhost:3000/health
#        curl http://localhost:3000/users
#        curl http://localhost:3000/users/42
```

## Building

Requires GHC 9.10.3.

```sh
cabal build all     # build everything (~7s clean)
cabal test all      # run all 10 test suites
```

## Project documents

- [`VISION.md`](VISION.md) — goals, philosophy, key decisions
- [`ARCHITECTURE.md`](ARCHITECTURE.md) — package structure, dependency
  graph, two-layer model, backend agnosticism, design decisions
- [`TYPEWAY-ANALYSIS.md`](TYPEWAY-ANALYSIS.md) — analysis of the Rust
  typeway framework and how its ideas translate to Haskell
