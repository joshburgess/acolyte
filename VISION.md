# Vision

This project aims to do three things that don't exist in the Haskell
ecosystem today:

1. **Bring the composable, pluggable web framework experience from Rust
   to Haskell.** The Rust ecosystem has a layered architecture — `http`
   (shared types), `tower` (Service/Layer composition), `hyper` (HTTP
   engine), `axum` (framework) — where each layer is independent and
   swappable. Haskell's web ecosystem is fragmented into monolithic islands
   (Servant, Yesod, Scotty, Spock) with no shared composition layer and
   no portable middleware. We fix this with `tower` (Haskell), `http-core`,
   and pluggable backend adapters.

2. **Apply the compile-time performance techniques from typeway to improve
   on Servant's design.** Servant's recursive `:<|>` tree and open type
   class instances cause exponential compile times. Typeway proved that
   flat tuple indexing and non-recursive dispatch fix this. We translate
   those techniques to Haskell using closed type families, promoted lists,
   and flat indexing — tools that are actually more powerful than what Rust
   has.

3. **Deliver type-safe, effect-tracked middleware for Haskell web
   projects.** No existing Haskell web framework has compile-time
   verification that required middleware is present. We provide this via
   a two-layer model: per-endpoint typed wrappers (auth, validation,
   versioning) enforced at handler dispatch, and phantom-type effect
   tracking that prevents the server from starting if required generic
   middleware (CORS, tracing, rate limiting) is missing.

## Source Material

This project is informed by **typeway** (~/code/typeway), a Rust web
framework built by the same author. Typeway demonstrated that these ideas
work in practice — with a complete implementation including REST server,
REST client, OpenAPI generation, gRPC integration, session-typed
WebSockets, content negotiation, API versioning, and a migration tool.

The key insight from typeway: Haskell's type system is strictly more
powerful than Rust's for this domain. Many of typeway's workarounds (marker
types for string literals, proc macros, phantom data) are unnecessary in
Haskell. We have type-level strings, promoted data kinds, closed type
families, constraint kinds, linear types, and higher-kinded types natively.
The goal is not to port typeway to Haskell — it's to take the ideas and
build something that could only exist in Haskell.

## Current Status

Phases 1-3 are complete. Six packages are built, tested, and working:

| Phase | Packages | Status |
|-------|----------|--------|
| 1 (foundation) | `servant-reimagined-core`, `tower`, `http-core` | Done |
| 2 (middleware + adapter) | `tower-http`, `tower-wai` | Done |
| 3 (first working server) | `servant-reimagined-server` | Done |
| 4 (interpretations) | `servant-reimagined-client`, `servant-reimagined-openapi`, `servant-reimagined-test` | Not started |
| 5 (advanced) | `servant-reimagined-grpc` | Not started |
| 6 (polish) | `servant-reimagined` (facade), servant interop | Not started |

29 modules, 112+ tests, 5.2 seconds clean build on GHC 9.10.3.

## Project Documents

- `README.md` — Quick start, examples, package overview, design philosophy.

- `TYPEWAY-ANALYSIS.md` — Detailed analysis of typeway's 12 improvements
  over Servant, with Haskell translation sketches for each.

- `ARCHITECTURE.md` — Complete package structure (12 packages), dependency
  graph, the two-layer middleware model, backend agnosticism design,
  handler monad decision, error handling strategy, and build phases.

## Key Design Decisions (Summary)

| Decision | Choice |
|----------|--------|
| API encoding | Promoted lists `'[a, b, c]`, not `:<|>` trees |
| Path literals | `Symbol` (GHC.TypeLits), not marker types |
| Compilation strategy | Closed type families, flat tuple indexing |
| Service abstraction | `tower` — new standalone Haskell library |
| HTTP types | `http-core` — backend-agnostic, not WAI |
| Backend | Pluggable adapters (tower-wai, tower-snap, ...) |
| Handler monad | `IO` directly, no transformer stack |
| State passing | Extractors, not ReaderT |
| Error handling | `Either` return types, not ExceptT |
| Middleware model | Two layers: typed per-endpoint + phantom-tracked generic |
| Session types | Linear Haskell for protocol enforcement |
| Target compiler | GHC 9.10.3 |
