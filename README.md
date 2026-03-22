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
| [`servant-reimagined-server`](servant-reimagined-server/) | Handler dispatch, routing, extractors, EffectfulServer | `core`, `tower`, `http-core` |

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
of recursive constraint solving. All 6 packages build from clean in ~5
seconds.

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

**Phases 1-3 complete.** The foundation is built and working:

- Type-level API definition with compile-time checks
- Service/Layer/Middleware composition with `|>` piping
- Backend-agnostic Request/Response types
- HTTP middleware (security headers, request ID, tracing)
- WAI/warp adapter with existing WAI middleware interop
- Automatic handler wiring from API types
- EffectfulServer with phantom-type effect tracking
- 6 packages, 29 modules, 112+ tests, all passing

**Remaining (future phases):**

- `servant-reimagined-client` — type-safe HTTP client
- `servant-reimagined-openapi` — OpenAPI spec generation
- `servant-reimagined-grpc` — unified REST + gRPC
- `servant-reimagined-test` — testing utilities
- Session-typed WebSocket runtime enforcement (LinearTypes)
- Custom `TypeError` messages for all common mistakes

## Building

Requires GHC 9.10.3.

```sh
cabal build all     # build everything (~5s clean)
cabal test all      # run all test suites
```

## Project documents

- [`VISION.md`](VISION.md) — goals, philosophy, key decisions
- [`ARCHITECTURE.md`](ARCHITECTURE.md) — package structure, dependency
  graph, two-layer model, backend agnosticism, design decisions
- [`TYPEWAY-ANALYSIS.md`](TYPEWAY-ANALYSIS.md) — analysis of the Rust
  typeway framework and how its ideas translate to Haskell
