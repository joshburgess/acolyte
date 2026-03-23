# Data Flow: Request to Response

## HTTP/1.1 request flow

```
Client (curl, browser, etc.)
    │
    │ TCP connection
    ▼
┌──────────────────────────────────────────────────────────────┐
│ tower-server                                                  │
│                                                               │
│  1. Accept TCP connection (Network.Socket)                    │
│  2. Read headers with timeout (slow loris protection)         │
│  3. Parse HTTP/1.1 request line + headers (Parse.hs)          │
│  4. Read body (Content-Length or chunked)                      │
│  5. Build http-core Request                                   │
│  6. Call runService on the tower Service ──────────────────┐  │
│  7. Receive http-core Response                             │  │
│  8. Render HTTP/1.1 response (Render.hs)                   │  │
│  9. Send over socket                                       │  │
│  10. Keep-alive? Loop to step 2                            │  │
└────────────────────────────────────────────────────────────┘  │
                                                                │
┌───────────────────────────────────────────────────────────────┘
│
▼
┌──────────────────────────────────────────────────────────────┐
│ tower middleware stack (composed with |>)                      │
│                                                               │
│  Request flows INWARD (first |> is innermost):                │
│                                                               │
│   secureHeaders ──► cors ──► tracing ──► requestId ──► ...   │
│        │                                        │             │
│        │              request path               │             │
│        │         ◄──────────────────────         │             │
│        │                                         │             │
│        ▼                                         ▼             │
│   ... ◄── requestId ◄── tracing ◄── cors ◄── secureHeaders  │
│                                                               │
│              response path (reverse order)                     │
└──────────────────────────────────┬───────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────┐
│ servant-reimagined-server (Router)                            │
│                                                               │
│  1. splitRequest → (RequestParts, body bytes)                 │
│  2. Linear scan of BoundHandlers:                             │
│     a. Match method (GET, POST, etc.)                         │
│     b. Match path pattern against request segments            │
│     c. Extract captures into CaptureList                      │
│  3. Store in Extensions:                                      │
│     - CaptureList (path captures)                             │
│     - BodyBytes (request body)                                │
│     - MatchedPath (route pattern)                             │
│     - OriginalUri (raw path)                                  │
│  4. Call HandlerFn(parts, body)                               │
│  5. No match? 404. Method match but path mismatch? 405.      │
└──────────────────────────────────┬───────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────┐
│ ToHandler (extractor chain)                                   │
│                                                               │
│  Handler: PathCapture Int -> QueryParam "q" Text -> IO (Json R)│
│                                                               │
│  1. fromRequestParts @(PathCapture Int) parts                 │
│     → Read CaptureList from Extensions, parseCapture          │
│     → Left err? Return 400 immediately. Right val? Continue.  │
│                                                               │
│  2. fromRequestParts @(QueryParam "q" Text) parts             │
│     → Read from rpQuery, parseCapture                         │
│     → Left err? Return 400. Right val? Continue.              │
│                                                               │
│  3. Call handler function with extracted values                │
│     → handler (PathCapture 42) (QueryParam "haskell")         │
│     → Returns IO (Json Result)                                │
│                                                               │
│  4. intoResponse converts to Response ByteString              │
│     → Json val → 200, application/json, aeson-encoded body    │
│     → Either e a → Left branch or Right branch                │
│     → Text → 200, text/plain                                  │
└──────────────────────────────────────────────────────────────┘
```

## HTTP/2 request flow

```
Client (grpcurl, h2c client, etc.)
    │
    │ TCP + HTTP/2 framing
    ▼
┌──────────────────────────────────────────────────────────────┐
│ tower-server H2 module                                        │
│                                                               │
│  Uses the http2 package (Kazu Yamamoto):                      │
│  1. allocSimpleConfig for socket                              │
│  2. H2.run dispatches each stream to toH2Server callback      │
│  3. fromH2Request converts http2 Request → http-core Request  │
│     - Extract :method, :path from pseudo-headers              │
│     - Token-based headers → [(CI ByteString, ByteString)]     │
│     - Body: pull-based BodyStream from getRequestBodyChunk    │
│  4. Call tower Service (same as HTTP/1.1 from here)           │
│  5. toH2Response converts http-core Response → http2 Response │
│     - BodyStrict → responseBuilder                            │
│     - BodyStream → responseStreaming                           │
│     - BodyFile → responseFile (zero-copy)                     │
│  6. respond callback sends to client                          │
└──────────────────────────────────────────────────────────────┘
```

## gRPC request flow

```
Client (grpcurl, gRPC client)
    │
    │ HTTP/2 POST /package.Service/Method
    │ content-type: application/grpc+proto
    │ body: [5-byte header][protobuf payload]
    ▼
┌──────────────────────────────────────────────────────────────┐
│ tower-grpc (grpcServer)                                       │
│                                                               │
│  1. Check content-type starts with "application/grpc"         │
│     → No? Return HTTP 415.                                    │
│  2. Collect body bytes (bodyToStrict)                          │
│  3. Parse gRPC path: /package.Service/Method                  │
│     → (serviceName, methodName)                               │
│  4. Lookup in GrpcServiceMap                                  │
│     → Not found? grpc-status: 12 (UNIMPLEMENTED)             │
│  5. Decode request message (strip 5-byte framing header)      │
│     → Compressed flag (1 byte) + length (4 bytes BE) + payload│
│  6. Build GrpcRequest { service, method, body, metadata }     │
│  7. Call GrpcHandler                                          │
│  8. Encode response:                                          │
│     → GrpcUnary: [5-byte header][payload] + trailers          │
│     → GrpcStream: multiple [5-byte header][payload] + trailers│
│     → GrpcError: trailers-only (no body)                      │
│  9. Always HTTP 200. Real status in grpc-status trailer.      │
└──────────────────────────────────────────────────────────────┘
```

## REST+gRPC multiplexing

```
Any client
    │
    ▼
┌──────────────────────────────────────────────────────────────┐
│ multiplex (Tower.Grpc.Multiplex)                              │
│                                                               │
│  content-type: application/grpc*  ──►  gRPC Service           │
│  anything else                    ──►  REST Service           │
│                                                               │
│  Both are tower Services. Both use the same http-core types.  │
│  The multiplexer is a 3-line function.                         │
└──────────────────────────────────────────────────────────────┘
```

## Compile-time flow

```
┌─────────────────────────────────────────────────────────────┐
│ User writes:                                                 │
│   type API = '[ Get (At "health") Text                       │
│               , Get (Param "users" Int) (Json User) ]        │
│   server = mkApi @API (health, getUser)                      │
└──────────────────────────┬──────────────────────────────────┘
                           │ GHC compiles
                           ▼
┌─────────────────────────────────────────────────────────────┐
│ Step 1: Serves (constraint synonym)                          │
│   CheckArity (Length API) (TupleArity (IO Text, ...))        │
│   → Length API = 2                                           │
│   → TupleArity (...) = 2                                     │
│   → CheckArity 2 2 = ()  ✓                                  │
│   (If mismatched: TypeError with clear message)              │
│                                                              │
│ Step 2: BuildApi instance selection                           │
│   GHC finds: instance BuildApi '[e1, e2] (h1, h2)           │
│   → Direct match, O(1) instance resolution                   │
│   → No recursive constraint solving                          │
│   → No overlapping instance ambiguity                        │
│                                                              │
│ Step 3: Per-handler constraint solving                        │
│   For each position:                                         │
│   → HasEndpointInfo (Get (At "health") Text)  ✓              │
│   → ToHandler (IO Text)  ✓ (base case)                       │
│   → HasEndpointInfo (Get (Param "users" Int) (Json User))  ✓ │
│   → ToHandler (PathCapture Int -> IO (Json User))  ✓         │
│     (peels PathCapture via FromRequestParts, then base case) │
│                                                              │
│ Step 4: Code generation                                      │
│   GHC produces a Router with two BoundHandlers.              │
│   Each BoundHandler has: method, pattern, matcher, handler.  │
│   The handler function calls toHandler at compile time.      │
│   No dictionaries in the hot path at runtime.                │
└─────────────────────────────────────────────────────────────┘
```

## Compile-time: Servant vs servant-reimagined

```
Servant (recursive :<|> tree):

  type API = A :<|> B :<|> C :<|> D

  GHC resolves HasServer:
    HasServer (A :<|> (B :<|> (C :<|> D)))
      → HasServer A, HasServer (B :<|> (C :<|> D))
        → HasServer B, HasServer (C :<|> D)
          → HasServer C, HasServer D

  At each level, GHC must unify the handler type with the
  :<|> decomposition. Constraint solving touches every node
  in the tree. For n endpoints: O(2^n) work.


servant-reimagined (flat list + flat instances):

  type API = '[ A, B, C, D ]

  GHC resolves BuildApi:
    BuildApi '[A, B, C, D] (h1, h2, h3, h4)
      → Single instance match (arity 4)
      → No decomposition, no recursion
      → O(1) instance resolution

  Then resolves per-handler constraints independently:
    HasEndpointInfo A  →  O(1) (direct instance)
    ToHandler h1       →  O(k) (k = number of arguments)
    HasEndpointInfo B  →  O(1)
    ToHandler h2       →  O(k)
    ...

  Total: O(n * k) where n = endpoints, k = avg handler args.
  In practice k ≤ 5, so this is effectively O(n).
```

## Extension map (typed heterogeneous storage)

```
┌─────────────────────────────────────────────────────────────┐
│ Extensions (IORef (Map TypeRep Any))                         │
│                                                              │
│ The router stores request-scoped data here before dispatch:  │
│                                                              │
│  ┌──────────────┬───────────────────────────────────┐       │
│  │ TypeRep key  │ Value                              │       │
│  ├──────────────┼───────────────────────────────────┤       │
│  │ CaptureList  │ CaptureList ["42", "hello"]        │       │
│  │ BodyBytes    │ BodyBytes "{\"name\":\"alice\"}"   │       │
│  │ MatchedPath  │ MatchedPath "/users/{capture}"     │       │
│  │ OriginalUri  │ OriginalUri "/users/42"            │       │
│  │ RequestId    │ RequestId "req-00042"              │       │
│  │ AppState     │ AppState myDbPool                  │       │
│  └──────────────┴───────────────────────────────────┘       │
│                                                              │
│ Extractors read from here via lookupExtension @Type.         │
│ Middleware writes here via insertExtension.                   │
│ Type-safe: wrong type → Nothing → 500 error.                │
└─────────────────────────────────────────────────────────────┘
```
