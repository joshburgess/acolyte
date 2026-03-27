# Changelog

## Unreleased

### New Features
- **Versioned endpoint routing:** `Versioned V1 (Get ...)` routes to `/v1/...`. Server, client, and OpenAPI all support the `Versioned` wrapper.
- **`callNamed`:** Look up client endpoints by name with `callNamed @"getUser" @API client 42`, powered by a `LookupNamed` type family.
- **Generic-based `ToSchema` derivation:** Records with `deriving Generic` get automatic `ToSchema` instances via `genericToSchema`, which walks `GHC.Generics` `Rep` to extract field names and types.

### OpenAPI
- `Describe "text"` now sets `opSummary` in the generated spec.
- `RespondsWith 204` now sets the response status code.
- `WithParams '[QP "page" Int]` produces query parameter schemas.
- `WithHeaders '[HH "Authorization" Text]` produces header parameter schemas.
- Request and response body schemas are now populated via `ToSchema` constraints (previously skeleton-only).
- `ToSchema (Json a)` instance delegates to the inner type's schema.

### Refactoring
- **`Json` newtype moved to core:** `newtype Json a = Json { unJson :: a }` now lives in `Servant.Reimagined.Core.Endpoint`, accessible to both server and openapi packages.
- **`BuildApi` refactored:** 25 flat arity-specific instances replaced with a `SplitTuple` type class and 2 recursive `BuildApi` instances.
- **Crud example updated:** Now uses `Named` + `mkRecordApi` instead of the manual `NamedApi`/`mkNamedApi` pattern.

### Client
- Added `EndpointRequest` delegation instances for `Versioned`, `Describe`, `WithParams`, `WithHeaders`, and `RespondsWith` wrappers.

## 0.1.0.0 — Initial release

### Core Framework
- Type-level API definition with promoted lists (constant compile time)
- Service/Layer/Middleware composition (tower)
- Backend-agnostic HTTP types (http-core)
- HTTP middleware: security headers, CORS, tracing, request ID, compression, timeouts
- Two backend adapters: tower-wai (warp) and tower-server (zero WAI)
- Automatic handler wiring with compile-time completeness check
- EffectfulServer with phantom-type effect tracking
- 23 request extractors with ToHandler ergonomic conversion
- Custom type errors for handler count mismatch and missing effects

### Interpretations
- HTTP server (servant-reimagined-server)
- Type-safe HTTP client (servant-reimagined-client)
- OpenAPI 3.1 + Swagger 2.0 generation (servant-reimagined-openapi)
- Code generation from OpenAPI/Swagger specs (servant-reimagined-codegen)
- Direct-dispatch testing utilities (servant-reimagined-test)

### gRPC
- HTTP/2 transport via the http2 package (tower-server)
- gRPC wire protocol: framing, status codes, service dispatch (tower-grpc)
- gRPC API type interpretation: GrpcCodec, GrpcReady, .proto generation (servant-reimagined-grpc)
- REST+gRPC multiplexing on a single port (`Tower.Grpc.Multiplex`)
- Content negotiation runtime (`Servant.Reimagined.Server.Negotiate`)
- Complete .proto generation with full message definitions
- .proto to API type codegen (`Servant.Reimagined.Codegen.Proto`)
- gRPC server reflection (`Tower.Grpc.Reflection`)
- Real network integration tests for gRPC
- gRPC testing utilities (servant-reimagined-test)

### WebSocket Session Types
- Session-typed WebSocket runtime (tower-websocket)
- Phantom-typed `Session` handle enforces send/recv protocol at compile time
- Operations: `send`, `recv`, `offer`, `select1`, `select2`, `close`, `recurse`, `loop`
- Branching protocols via `Offer` / `Select`
- Recursive protocols via `Rec` / `Var` with `Unfold` type family
- Transport-agnostic `WebSocketConn` abstraction
- `withSession` runner for initializing a protocol session

### gRPC Streaming and Services
- Client streaming handler (`clientStreamHandler`): collect multiple client messages, return one response
- Bidirectional streaming handler (`bidiStreamHandler`): collect multiple client messages, return multiple responses
- gRPC health check service (`Tower.Grpc.Health`): implements `grpc.health.v1.Health/Check` protocol
- `withHealthCheck` helper to add health check to any service map
- gRPC gzip compression (`Tower.Grpc.Compression`): `compressMessage`, `decompressMessage`, `encodeMessageCompressed`, `decompressGrpcBody`
- `grpc-encoding: gzip` header support

### Examples
- 11 complete example applications: minimal, hello-world, crud, auth, custom-extractors, grpc-demo, chat, negotiate, versioned-api, realworld, realworld-combined

### API Ergonomics
- Named endpoints (`Named "name" endpoint`): wrap endpoints with type-level names for automatic record-based handler binding via `mkRecordApi` — field order doesn't matter, matched by `GHC.Records.HasField`
- Named endpoint names become `operationId` in OpenAPI generation
- `Named` is transparent to routing and tuples (like `Describe`)
- Compile-time validation: `AllNamed`, `NoDuplicateNames` with custom type errors
- Legacy named routes (`mkNamedApi`) for manual record-to-tuple conversion
- Type-level annotations: `WithParams`, `WithHeaders`, `RespondsWith`, streaming markers (`ServerStream`, `ClientStream`, `BidiStream`)
- Path helpers: `At`, `Param`, `mkApi`, `effectfulApi`

### Quality
- StrictData + funbox-strict-fields on all packages
- -O2 for production builds
- -fno-full-laziness on streaming modules
- 40 test suites, 43 hedgehog property tests across 8 suites, 612+ unit assertions
- Compile-time benchmarks (1-32 endpoints in constant time)
