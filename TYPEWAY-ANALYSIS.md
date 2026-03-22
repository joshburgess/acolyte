# Typeway Analysis: What It Does Better Than Servant

This document captures the results of a thorough analysis of the typeway Rust web
framework (~/code/typeway) and what a ground-up Haskell reimagining of Servant
should learn from it.

Typeway is a type-level web framework for Rust where the entire API is described
as a single Rust type. Servers, clients, OpenAPI schemas, gRPC services, and
`.proto` files are all derived from that type. It was explicitly designed to
address pain points in Haskell's Servant while working within Rust's more
limited type system.

---

## 1. Compile-Time Performance

### Servant's Problem

Recursive type class resolution causes exponential compile times. Each route
adds another layer of recursive constraint solving through the `HasServer`
class and the `:<|>` combinator. Large APIs (30+ routes) can take minutes to
compile. This is the single most common complaint about Servant in production.

The root cause is structural: Servant encodes the API as a right-nested binary
tree of `:<|>` combinators, and `HasServer` recurses into both branches at
every node. GHC's constraint solver does not cache intermediate results across
branches, so the same sub-constraints get re-solved repeatedly.

### Typeway's Solution

Flat tuple `macro_rules!` impls for `Serves`, `ApiSpec`, and `Handler` —
O(1) per instantiation. HLists are used only for paths (which are short and
bounded at 8 captures), while route collections are flat tuples (up to arity
16, nestable for larger APIs).

```rust
// Flat tuple — no recursive resolution
type API = (
    GetEndpoint<UsersPath, Json<Vec<User>>>,
    GetEndpoint<UserByIdPath, Json<User>>,
    PostEndpoint<UsersPath, Json<CreateUser>, Json<User>>,
    DeleteEndpoint<UserByIdPath, StatusCode>,
);
```

Macro-generated impls mean the compiler sees a single, direct impl for each
tuple arity — no recursion, no backtracking, no exponential blowup.

### Haskell Translation

GHC's type families and closed type families can be more efficient than open
type class resolution. A redesigned Servant should:

- **Use closed type families** for path parsing instead of open instances. Closed
  type families are evaluated top-to-bottom with no backtracking — similar to
  pattern matching.

- **Use flat tuple-indexed type families** for handler matching instead of
  recursive `HasServer` chains. GHC can resolve a single type family application
  in roughly constant time, so a family that directly indexes into a tuple avoids
  the exponential blowup.

- **Use type-level Maps or sorted lists** indexed by route, avoiding the linear
  scan through `:<|>` alternatives that Servant does today.

- **Adopt the "HList for paths, tuples for APIs" rule.** Paths are inherently
  recursive (match one segment, recurse on the remainder — a catamorphism).
  Route collections are flat sets with no recursive structure. Using the right
  data structure for each avoids unnecessary constraint solving depth.

- **Benchmark compile times** as a first-class project metric with explicit
  budgets, as typeway does (e.g., 5 routes < 15s, 20 routes < 45s).

---

## 2. Type-Level Middleware Effects System

### Servant's Problem

Middleware in the Servant ecosystem means WAI middleware, which is completely
untyped. You can forget to add auth middleware to a protected endpoint and the
compiler will not notice — you get runtime 401s or worse, security holes. There
is no connection between what a route requires and what the middleware stack
provides.

This is arguably the most important missing feature. In practice, teams build
ad-hoc solutions: custom monadic contexts, manual checks in handlers, or
separate auth modules that are not connected to the API type.

### Typeway's Solution

Type-level effects encoded as marker types:

```rust
trait Effect: Send + Sync + 'static {}

struct AuthRequired;
impl Effect for AuthRequired {}

struct CorsRequired;
impl Effect for CorsRequired {}

struct RateLimitRequired;
impl Effect for RateLimitRequired {}
```

Endpoints declare their requirements via `Requires`:

```rust
type API = (
    Requires<AuthRequired, GetEndpoint<UserPath, User>>,
    Requires<CorsRequired, GetEndpoint<PublicPath, Data>>,
    GetEndpoint<HealthPath, String>,  // no requirements
);
```

The `EffectfulServer` builder discharges effects:

```rust
EffectfulServer::<API>::new(handlers)
    .provide::<AuthRequired>()
    .layer(auth_middleware)
    .provide::<CorsRequired>()
    .layer(CorsLayer::permissive())
    .serve(addr)  // only compiles if all effects are provided
    .await?
```

The compile-time verification works through:
- Type-level effect lists: `ENil`, `ECons<E, Tail>`
- Index witnesses: `EHere`, `EThere<Idx>` for O(n) lookup
- `HasEffect<E, Idx>` trait for membership checking
- `AllProvided<Provided, Idx>` trait that recursively walks the API type and
  verifies every `Requires<E, _>` has its `E` in the provided list

Missing `.provide()` call = compile error. Order of `.provide()` calls does not
matter (the index witness handles arbitrary positioning).

### Haskell Translation

This is where Haskell can truly shine. We have far more powerful tools:

- **Promoted data kinds** for effect types — no need for marker traits:
  ```haskell
  data Effect = Auth | Cors | RateLimit | Tracing
  ```

- **Type-level lists** with `'[Auth, Cors]` syntax for the provided set

- **Closed type families** for membership checking:
  ```haskell
  type family Elem (e :: Effect) (es :: [Effect]) :: Bool where
    Elem e '[]       = 'False
    Elem e (e ': es) = 'True
    Elem e (_ ': es) = Elem e es
  ```

- **Constraint kinds** to turn membership into a constraint:
  ```haskell
  type Requires (e :: Effect) (es :: [Effect]) = Elem e es ~ 'True
  ```

- **Indexed phantom state** in the builder to track which effects have been
  discharged — each `.provide @Auth` call transitions the builder's phantom
  type parameter from one list to another.

- **Custom type errors** (`TypeError`) for clear messages when an effect is
  missing.

Could also integrate with actual algebraic effect libraries (`effectful`,
`polysemy`, `cleff`) for runtime semantics, while the type-level tracking
provides the compile-time guarantee.

---

## 3. Tower-Like Service/Layer Abstraction

### Servant's Problem

WAI's middleware type is:

```haskell
type Middleware = Application -> Application
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
```

This is a raw CPS function with no composable abstraction. There is no `Service`
trait, no `Layer` concept, no typed composition. Middleware is ad-hoc: each
middleware package defines its own conventions for configuration, error handling,
and state passing.

Compare this to Rust's Tower ecosystem, where `Service` and `Layer` are
standardized traits that the entire ecosystem composes through. Tower middleware
from different authors composes seamlessly because they all implement the same
interface.

### Typeway's Solution

Typeway uses Tower directly. It does not define its own service abstraction —
it wraps its router as a `tower::Service` and accepts any `tower::Layer` for
middleware:

```rust
Server::<API>::new(handlers)
    .layer(SecureHeadersLayer::new())
    .layer(TraceLayer::new_for_http())
    .layer(CorsLayer::permissive())
    .serve(addr)
```

The key insight: the middleware abstraction (Tower) is separate from the
type-level API specification (typeway). They compose orthogonally.

### Haskell Translation

We should design a proper Tower equivalent for Haskell. The core abstractions:

```haskell
-- A service transforms requests into responses in some monad
class Service s where
  type Req s
  type Resp s
  type Err s
  serve :: s -> Req s -> IO (Either (Err s) (Resp s))

-- A layer wraps one service to produce another
class Layer l inner outer | l inner -> outer where
  layer :: l -> inner -> outer
```

Or, more practically using a concrete type rather than a class:

```haskell
-- Service as a value, not a class — enables easier composition
newtype Service req resp = Service { runService :: req -> IO resp }

-- Middleware transforms a service
type Middleware req resp req' resp' = Service req resp -> Service req' resp'

-- Layer is a middleware factory (can carry configuration)
newtype Layer req resp req' resp' = Layer { applyLayer :: Service req resp -> Service req' resp' }
```

The advantage of a standardized `Service`/`Layer` interface:
- All middleware composes through the same types
- Middleware ordering is explicit (layer application order)
- Each layer can carry typed configuration
- The effect system can verify that required layers are present
- Timeout, retry, rate limiting, auth, tracing, CORS can all be independent
  packages implementing the same `Layer` interface

This would be a **new Haskell library** — the Haskell equivalent of Tower. It
does not exist today. WAI would become just one possible adapter (turning a
`Service Request Response` into a WAI `Application`).

---

## 4. Session-Typed WebSockets

### Servant's Problem

No WebSocket support in core Servant. The `servant-websockets` package exists
but provides no protocol enforcement — you get a raw `Connection` and can send
any message at any time. Protocol violations are runtime errors.

### Typeway's Solution

Full session type encoding integrated into the API type:

```rust
type ChatProtocol = Recv<JoinMsg, Send<WelcomeMsg,
    Rec<Offer<
        Recv<ChatMsg, Send<BroadcastMsg, Var>>,
        Recv<LeaveMsg, End>
    >>>>;

type API = (
    GetEndpoint<ChatPath, WebSocket<ChatProtocol>>,
);
```

Session type primitives:
- `Send<T, S>` — send message of type T, continue with session S
- `Recv<T, S>` — receive message of type T, continue with session S
- `Offer<S1, S2>` — offer the peer a choice between two continuations
- `Select<S1, S2>` — select one of two offered continuations
- `Rec<S>` / `Var` — recursive protocol (looping)
- `End` — session termination
- `Dual` trait — computes the dual protocol (client sees the mirror)

Each state transition consumes the old channel and produces a new one at the
next session type. Rust's ownership/move semantics enforce the linear discipline:
you cannot reuse a consumed channel or skip a step.

### Haskell Translation

Haskell has prior art here (the `sessions` and `session-types` packages), but
they have not been integrated into web frameworks. We could:

- Define the same session type algebra as promoted data kinds:
  ```haskell
  data SessionType
    = Send Type SessionType
    | Recv Type SessionType
    | Offer SessionType SessionType
    | Select SessionType SessionType
    | Rec SessionType
    | Var
    | End
  ```

- Use **Linear Haskell** (`-XLinearTypes`) for enforcement. A linear
  `Session s` value must be used exactly once — consuming it yields the next
  session state. This gives us the same guarantees as Rust's ownership:
  ```haskell
  send :: a -> Session (Send a s) %1 -> IO (Session s)
  recv :: Session (Recv a s) %1 -> IO (a, Session s)
  ```

- Compute the `Dual` as a type family:
  ```haskell
  type family Dual (s :: SessionType) :: SessionType where
    Dual (Send a s) = Recv a (Dual s)
    Dual (Recv a s) = Send a (Dual s)
    Dual (Offer s1 s2) = Select (Dual s1) (Dual s2)
    Dual (Select s1 s2) = Offer (Dual s1) (Dual s2)
    Dual End = End
    Dual (Rec s) = Rec (Dual s)
    Dual Var = Var
  ```

- Integrate into the API type so the WebSocket protocol appears in OpenAPI
  documentation and client generation.

---

## 5. Content Negotiation as Type-Level Coproduct

### Servant's Problem

Content types are handled per-endpoint with `ReqBody '[JSON, PlainText] a` but
negotiation logic is limited and somewhat manual. The `Accept` header is checked
but the framework does not provide a clean abstraction for "return the same data
in multiple formats based on what the client asks for."

### Typeway's Solution

`NegotiatedResponse<T, Formats>` — a single handler returns the domain value,
and the framework automatically selects the serialization format based on the
`Accept` header:

```rust
async fn get_article(
    accept: AcceptHeader,
    path: Path<ArticlePath>,
) -> Result<NegotiatedResponse<Article, (JsonFormat, TextFormat, XmlFormat)>, Error> {
    let article = db.find(id).await?;
    Ok(NegotiatedResponse::new(article, &accept))
}
```

The format list is a type-level coproduct. Each format must have a way to
serialize the domain type. All formats appear in the OpenAPI spec. Runtime
negotiation uses quality-weighted Accept header parsing.

### Haskell Translation

Type-level list of content types with automatic negotiation:

```haskell
type GetArticle = Negotiate '[JSON, XML, PlainText] Article

-- Handler just returns the domain value
getArticle :: ArticleId -> Handler Article
getArticle id = findArticle id

-- Framework handles Accept parsing and format selection
```

A `RenderAs` class provides per-format serialization:

```haskell
class RenderAs (fmt :: ContentType) a where
  render :: a -> ByteString
  contentType :: Proxy fmt -> ByteString
```

This is cleaner than Servant's current approach because the handler does not
need to know about content types at all — it returns the domain value and the
framework does the rest.

---

## 6. API Versioning with Typed Deltas

### Servant's Problem

No versioning support whatsoever. Teams copy-paste API types for each version,
manually ensuring backward compatibility. Breaking changes are discovered at
runtime or during code review, not by the compiler.

### Typeway's Solution

Type-level version deltas with compile-time compatibility checking:

```rust
type V1 = (
    GetEndpoint<UsersPath, Vec<UserV1>>,
    GetEndpoint<UserByIdPath, UserV1>,
);

type V2Changes = (
    Added<GetEndpoint<UserProfilePath, Profile>>,
    Replaced<GetEndpoint<UserByIdPath, UserV1>, GetEndpoint<UserByIdPath, UserV2>>,
    Deprecated<GetEndpoint<UsersPath, Vec<UserV1>>>,
);

type V2 = VersionedApi<V1, V2Changes, V2Resolved>;

// Compile-time check:
assert_api_compatible!(V1, V2);
```

Change markers:
- `Added<E>` — new endpoint
- `Removed<E>` — endpoint deleted (breaking change)
- `Replaced<Old, New>` — endpoint changed
- `Deprecated<E>` — endpoint still works but marked for removal

`BackwardCompatible` trait verifies that no `Removed` changes appear (only
additions, replacements with compatible types, and deprecations are allowed).

### Haskell Translation

Type families can express this very elegantly:

```haskell
data Change
  = Added Endpoint
  | Removed Endpoint
  | Replaced Endpoint Endpoint
  | Deprecated Endpoint

type family ApplyChanges (base :: [Endpoint]) (changes :: [Change]) :: [Endpoint]

type family IsBackwardCompatible (changes :: [Change]) :: Bool where
  IsBackwardCompatible '[] = 'True
  IsBackwardCompatible (Removed _ ': _) = 'False
  IsBackwardCompatible (_ ': rest) = IsBackwardCompatible rest

-- Constraint alias
type BackwardCompatible changes = IsBackwardCompatible changes ~ 'True
```

Version deltas become first-class values in the type system. The compiler
enforces compatibility policies. Changelog generation comes for free from
the change list.

---

## 7. Endpoint Spec vs Handler Contract Separation

### Servant's Problem

`HasServer` conflates the API specification with the handler contract. The
handler must return exactly the declared type. If the API says `Get '[JSON]
User`, the handler must return `User` — not `Either AppError User`, not
`Handler (Either NotFound User)`. This leads to awkward workarounds like
`throwError` or custom servant error types.

### Typeway's Solution

Two-level split:

- `Endpoint<M, P, Req, Res>` is the **specification** — what appears in
  OpenAPI, what the client expects. `Res` is the happy-path type.

- Handlers return `impl IntoResponse` — they can return `Result<Json<User>,
  JsonError>`, `StatusCode`, tuples, or any response type.

- `CompatibleWith<Res>` bridges the gap: it verifies that the handler's
  return type can produce the declared response type on the happy path.

```rust
// Spec says: returns Json<User>
type GetUser = GetEndpoint<UserByIdPath, Json<User>>;

// Handler actually returns Result — the error case is not in the spec
async fn get_user(path: Path<UserByIdPath>) -> Result<Json<User>, JsonError> {
    db.find(id).await.ok_or(JsonError::not_found("user not found"))
}
```

### Haskell Translation

Separate the spec type from the handler return type:

```haskell
-- API spec declares the happy-path response
type GetUser = Get "users" :> Capture "id" UserId :> Returns User

-- Handler can return any type that is CompatibleWith User
class CompatibleWith declared actual
instance CompatibleWith a a
instance (CompatibleWith a ok, IntoResponse err) => CompatibleWith a (Either err ok)
-- etc.

-- Handler:
getUser :: UserId -> Handler (Either AppError User)
```

The OpenAPI spec and client see `User`. The handler sees
`Either AppError User`. The framework bridges the gap.

---

## 8. Unified Multi-Protocol Serving

### Servant's Problem

REST only. gRPC requires completely separate tooling (`grpc-haskell` or
`proto-lens` with manual service definitions). There is no connection between
the Servant API type and gRPC service definitions. If you serve both REST and
gRPC, you maintain two separate stacks with no shared types or handlers.

### Typeway's Solution

The same API type drives everything:

```rust
Server::<API>::new(handlers)
    .with_openapi("My API", "1.0")    // REST + OpenAPI docs
    .with_grpc("UserService", "pkg")  // same handlers serve gRPC too
    .serve(addr)
    .await?
```

From a single API type, typeway generates:
- REST server routing and handler dispatch
- REST client with type-safe calls
- OpenAPI 3.1 spec + Swagger UI
- gRPC server dispatch (same handlers, different wire format)
- gRPC client with type-safe calls
- `.proto` file generation
- gRPC service spec + HTML documentation
- Server reflection for tools like `grpcurl`

The `GrpcReady` compile-time check ensures every type in the API implements
`ToProtoType` before `.with_grpc()` compiles.

Additional gRPC features:
- `#[derive(ToProtoType)]` with `#[proto(tag = N)]` for stable field numbering
- Enum support (simple enums to proto enum, tagged enums to oneof)
- `ServerStream`, `ClientStream`, `BidirectionalStream` markers
- Server reflection and health check services
- gRPC-Web layer for browser clients
- Proto diff/validation CLI for breaking change detection
- High-performance `TypewayCodec` (3-8x faster than prost for some cases)

### Haskell Translation

This would require:
- A `ToProtoType` class with Template Haskell or GHC.Generics deriving
- A type family that validates all types in the API are proto-compatible
- Shared handler abstraction that can dispatch to both HTTP and gRPC
- `.proto` generation from the API type
- Integration with `proto-lens` or `grpc-haskell` for the actual gRPC runtime

---

## 9. Protected Endpoints and Auth Typing

### Servant's Problem

Authentication in Servant uses `AuthProtect` or custom combinators, but the
connection between "this endpoint requires auth" and "this handler receives
auth credentials" is loose. You can accidentally write an unprotected handler
for a protected endpoint.

### Typeway's Solution

`Protected<Auth, Endpoint>` wrapper that changes the binding rules:

```rust
type API = (
    Protected<AuthUser, GetEndpoint<UserPath, User>>,  // requires auth
    GetEndpoint<HealthPath, String>,                    // public
);

// bind_auth! required for Protected endpoints — bind! won't compile
Server::<API>::new((
    bind_auth!(get_user),   // AuthUser must be first argument
    bind!(health),          // no auth needed
))
```

`Protected` is NOT `BindableEndpoint`, so `bind!()` (the normal macro) produces
a type error. Only `bind_auth!()` works, which enforces that `AuthUser` is the
first handler argument via the `AuthHandler` trait.

### Haskell Translation

A `Protected auth` wrapper on the API type, with the handler type requiring
`auth` as the first argument:

```haskell
type API = Protected AuthUser (Get "users" :> Capture "id" UserId :> Returns User)
     :<|> Get "health" :> Returns String

-- Type class dispatch ensures Protected routes require auth argument
```

---

## 10. Typed Request Validation

### Servant's Problem

No built-in validation. Request body parsing (via `FromJSON`) either succeeds
or fails with a parse error. Business rule validation (e.g., "username must be
at least 3 characters") happens inside the handler, with no compile-time
connection to the endpoint spec.

### Typeway's Solution

`Validated<Validator, Endpoint>` wrapper:

```rust
struct CreateUserValidator;
impl Validate<CreateUser> for CreateUserValidator {
    fn validate(body: &CreateUser) -> Result<(), String> {
        if body.username.len() < 3 { return Err("username too short".into()); }
        Ok(())
    }
}

type API = (
    Validated<CreateUserValidator, PostEndpoint<UsersPath, Json<CreateUser>, Json<User>>>,
);
```

Validation runs after deserialization but before the handler. Returns 422 on
failure. The `bind_validated!()` macro is required for these endpoints.

### Haskell Translation

A `Validated validator` wrapper with a `Validate` class:

```haskell
class Validate v a where
  validate :: a -> Either Text a

type API = Validated CreateUserValidator (Post "users" :> ReqBody CreateUser :> Returns User)
```

---

## 11. Migration Tooling

### Servant's Problem

No migration path. Adopting Servant or moving away from it is a manual,
all-or-nothing effort.

### Typeway's Solution

`typeway-migrate` — a bidirectional CLI tool (106 tests, 14 roundtrip tests):
- Axum to Typeway and back
- Interactive mode for ambiguous conversions
- Partial migration (convert only selected endpoints)
- Automatic Cargo.toml dependency updates
- VSCode extension with 5 commands

### Haskell Translation

A migration tool that converts between Servant API types and the new framework
would significantly lower the adoption barrier. Template Haskell or a
standalone tool could parse Servant API types and generate equivalent new types.

---

## 12. Axum Interop (Incremental Adoption)

### Servant's Problem

You either use Servant for everything or you don't use it. There is no way to
embed a Servant app inside a non-Servant app or vice versa, except through
raw WAI.

### Typeway's Solution

Bidirectional Axum interop:
- `Server::into_axum_router()` — embed typeway inside an Axum app
- `Server::with_axum_fallback(axum_router)` — use Axum for unmatched routes
- Enables incremental adoption: migrate one route group at a time

### Haskell Translation

Interop with WAI and existing frameworks (Scotty, Yesod) via adapter functions.
The Tower-like `Service` abstraction would make this natural — any framework
that can produce or consume a `Service` can interoperate.

---

## Summary: Typeway's Key Advantages Over Servant

| Area | Servant | Typeway | Haskell Opportunity |
|------|---------|---------|---------------------|
| Compile time | Exponential (recursive constraints) | O(1) (flat tuple impls) | Closed type families, flat indexing |
| Middleware typing | Untyped (WAI) | Type-level effects | DataKinds + constraint kinds |
| Service abstraction | None (raw WAI) | Tower | New Haskell Tower library |
| Session-typed WS | None | Full session types | LinearTypes |
| Content negotiation | Limited | Type-level coproduct | Type-level lists |
| API versioning | None | Typed deltas | Type families |
| Spec/handler split | Conflated | Separated (CompatibleWith) | Type families |
| Multi-protocol | REST only | REST + gRPC + proto | New, requires design |
| Auth typing | Loose | Protected<Auth, E> | Wrapper + type class |
| Validation | None | Validated<V, E> | Wrapper + type class |
| Migration tooling | None | Bidirectional CLI | TH or standalone tool |
| Incremental adoption | All or nothing | Axum interop | WAI/framework adapters |

### Where Haskell Has the Advantage

Haskell's type system is strictly more powerful for this domain. Many of
typeway's workarounds are unnecessary:

- **Type-level strings** (GHC.TypeLits) — no marker types or proc macros needed
- **Type families** (open and closed) — more powerful than Rust trait impls
- **DataKinds** — promoted data constructors for effects, changes, etc.
- **GADTs** — richer type indexing than Rust's PhantomData patterns
- **LinearTypes** — native linear enforcement for session types
- **Higher-kinded types** — proper Service/Layer abstractions
- **Constraint kinds** — effects as constraints, not marker traits
- **Custom type errors** (TypeError) — good error messages without hacks

### Where We Must Be Careful

- **Compile times** — GHC can be slow with heavy type-level programming. We
  must actively benchmark and optimize, using closed type families and flat
  indexing instead of open recursive instances.

- **Error messages** — type-level Haskell errors are notoriously bad. We must
  use `TypeError` aggressively and design for error quality from the start.

- **Ecosystem fragmentation** — Haskell has many effect systems, many web
  frameworks, many serialization libraries. We should design for composition
  with the ecosystem, not replacement of it.

- **Simplicity** — typeway's design is pragmatic and focused. We should resist
  the temptation to over-abstract just because Haskell lets us. The right
  amount of type-level machinery is the minimum needed for the guarantees we
  want.
