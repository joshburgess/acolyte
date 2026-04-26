# Changelog

## 0.1.0.0 (initial release)

### Core framework
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
- HTTP server (acolyte-server)
- Type-safe HTTP client (acolyte-client)
- OpenAPI 3.1 + Swagger 2.0 generation (acolyte-openapi)
- Code generation from OpenAPI/Swagger specs (acolyte-codegen)
- Direct-dispatch testing utilities (acolyte-test)

### gRPC
- HTTP/2 transport via the http2 package (tower-server)
- gRPC wire protocol: framing, status codes, service dispatch (tower-grpc)
- gRPC API type interpretation: GrpcCodec, GrpcReady, .proto generation (acolyte-grpc)
- REST+gRPC multiplexing on a single port (`Tower.Grpc.Multiplex`)
- Content negotiation runtime (`Acolyte.Server.Negotiate`)
- Complete .proto generation with full message definitions
- .proto to API type codegen (`Acolyte.Codegen.Proto`)
- gRPC server reflection (`Tower.Grpc.Reflection`)
- Real network integration tests for gRPC
- gRPC testing utilities (acolyte-test)

### gRPC streaming and services
- Client streaming handler (`clientStreamHandler`): collect multiple client messages, return one response
- Bidirectional streaming handler (`bidiStreamHandler`): collect multiple client messages, return multiple responses
- gRPC health check service (`Tower.Grpc.Health`): implements `grpc.health.v1.Health/Check` protocol
- `withHealthCheck` helper to add health check to any service map
- gRPC gzip compression (`Tower.Grpc.Compression`): `compressMessage`, `decompressMessage`, `encodeMessageCompressed`, `decompressGrpcBody`
- `grpc-encoding: gzip` header support

### WebSocket session types
- Session-typed WebSocket runtime (tower-websocket)
- Phantom-typed `Session` handle enforces send/recv protocol at compile time
- Operations: `send`, `recv`, `offer`, `select1`, `select2`, `close`, `recurse`, `loop`
- Branching protocols via `Offer` / `Select`
- Recursive protocols via `Rec` / `Var` with `Unfold` type family
- Transport-agnostic `WebSocketConn` abstraction
- `withSession` runner for initializing a protocol session

### API ergonomics
- Named endpoints (`Named "name" endpoint`): wrap endpoints with type-level names for automatic record-based handler binding via `mkRecordApi`. Field order does not matter, matched by `GHC.Records.HasField`.
- Named endpoint names become `operationId` in OpenAPI generation
- `Named` is transparent to routing and tuples (like `Describe`)
- Compile-time validation: `AllNamed`, `NoDuplicateNames` with custom type errors
- Legacy named routes (`mkNamedApi`) for manual record-to-tuple conversion
- Type-level annotations: `WithParams`, `WithHeaders`, `RespondsWith`, streaming markers (`ServerStream`, `ClientStream`, `BidiStream`)
- Path helpers: `At`, `Param`, `mkApi`, `effectfulApi`
- `CaptureNamed`: explicit path parameter names (`ParamNamed "userId" "users" Int`) for cleaner OpenAPI output
- `Description` wrapper: long-form endpoint documentation (distinct from the short `Describe` summary)
- Versioned endpoint routing: `Versioned V1 (Get ...)` routes to `/v1/...`. Server, client, and OpenAPI all support the `Versioned` wrapper.
- `callNamed`: look up client endpoints by name with `callNamed @"getUser" @API client 42`, powered by a `LookupNamed` type family
- `mkClientRecord`: constructs a typed client record from `Named` APIs via `Generic`, matching fields by name
- `ValidatedBody` extractor: deserializes JSON and validates via a `Validate v a` instance before the handler runs. Failures return 422 with a structured error.
- `Protected` auth enforcement via `FirstArg`: ensures the handler's first argument is the auth credential type
- SSE streaming types and async `sseResponse`: `sseResponse` runs the handler in a forked thread and delivers events incrementally. `sseResponseSync` provides the simpler collect-then-serve approach.

### OpenAPI
- `Describe "text"` sets `opSummary` in the generated spec
- `RespondsWith 204` sets the response status code
- `WithParams '[QP "page" Int]` produces query parameter schemas
- `WithHeaders '[HH "Authorization" Text]` produces header parameter schemas
- Request and response body schemas populated via `ToSchema` constraints
- `ToSchema (Json a)` instance delegates to the inner type's schema
- Generic-based `ToSchema` derivation: records with `deriving Generic` get automatic `ToSchema` instances via `genericToSchema`, which walks `GHC.Generics` `Rep` to extract field names and types
- Sum type / enum OpenAPI schemas via `GToSchemaCon` (produces `oneOf` with tagged variants)
- `Either` `ToSchema` instance produces `oneOf`
- `Maybe` `ToSchema` instance emits `"nullable": true` (via `schemaNullable` field)
- Capture path parameter schemas use `ToSchema t` instead of hardcoded `"string"`

### Examples
- 12 complete example applications: minimal, hello-world, crud, auth, custom-extractors, grpc-demo, chat, negotiate, versioned-api, streaming, realworld, realworld-combined

### Quality
- StrictData + funbox-strict-fields on all packages
- -O2 for production builds
- -fno-full-laziness on streaming modules
- 27 test suites, 1000+ unit assertions, 60+ hedgehog property tests
- Compile-time benchmarks (1 to 32 endpoints in constant time)

### Implementation notes
- `Json` newtype lives in `Acolyte.Core.Endpoint`, accessible to both server and openapi packages
- `BuildApi` uses two recursive instances plus a `SplitTuple` type class with arity-2-through-25 instances, instead of 25 flat per-arity instances
- Crud example uses `Named` + `mkRecordApi` for record-based handler binding
