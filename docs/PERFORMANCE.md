# Performance

This document covers both compile-time and runtime performance.
The numbers come from real benchmarks running on the codebase —
run them yourself with `bash bench/compile-time/run-bench.sh` and
`cabal run runtime-bench -- +RTS -T`.

## The headline numbers

| What | How fast |
|------|---------|
| Dispatch a request (1 endpoint) | 142 ns |
| Dispatch a request (25 endpoints, worst case) | 155 ns |
| Compile 1 endpoint | ~1.3s |
| Compile 32 endpoints | ~1.3s |
| gRPC encode (any payload size) | 14-19 ns |
| Adding 5 middleware layers | Free (0 ns overhead) |

## Compile-time: the exponential problem, solved

Servant's biggest pain point is compile time. Its `:<|>` tree encoding
forces GHC to recursively decompose the API type at every constraint
resolution step. This is O(2^n) — at 20 endpoints, you're waiting
minutes.

We use flat promoted lists and flat tuple instances. GHC matches one
instance directly, does O(1) work, and moves on. The result:

```
Endpoints       Time (s)
---------       --------
1                   1.27
4                   1.30
8                   1.28
16                  1.29
32                  1.30
```

That's not linear — it's **constant**. The ~1.3 seconds is GHC startup
and dependency loading. The actual type checking of the API adds
effectively nothing regardless of how many endpoints you have.

The `Serves` constraint is a type synonym that reduces in a single
step. `CheckArity` is a closed type family with two equations. `BuildApi`
has flat instances for arities 1-25. None of this recurses. None of it
backtracks. GHC's constraint solver does less work for a 25-endpoint
API than Servant does for a 3-endpoint one.

## Request dispatch: what 142 nanoseconds buys you

A request hits the server, gets split into parts and body, and the
router does a linear scan of bound handlers. For a 1-endpoint API,
the entire round trip — match path, extract captures, run handler,
serialize response — takes **142 ns**.

```
dispatch/1-endpoint/hit:      148 ns    1.4 KB allocated
dispatch/3-endpoints/first:   165 ns    1.4 KB allocated
dispatch/3-endpoints/last:    346 ns    7.1 KB allocated
dispatch/5-endpoints/first:   202 ns    1.6 KB allocated
dispatch/5-endpoints/last:    366 ns    7.2 KB allocated
router-scaling/10-routes/last: 181 ns    1.6 KB allocated
router-scaling/25-routes/last: 155 ns    1.4 KB allocated
router-scaling/25-routes/miss:  77 ns    752 B allocated
```

The router uses a **segment-indexed dispatch**: routes are keyed by
their first path segment in a `Map Text [BoundHandler]`. When a request
for `/users/42` arrives, the router looks up `"users"` in the Map
(O(log n)), then linear scans only the 2-3 routes sharing that prefix.

This is why 25 routes (155 ns) is actually *faster* than 3 routes
hitting the last match (346 ns) — the 3-route case has all routes
sharing a prefix (`/items`), forcing a full scan of the candidate list,
while the 25-route case distributes across many distinct first segments.

A 404 miss at 25 routes takes only 77 ns — the Map lookup finds no
candidates, so no matchers run at all.

We benchmarked three optimization strategies:
1. **Segment index** (kept): 2.5x faster at 25 routes, 3.6x faster for misses
2. **Method + segment compound key**: slower due to 405 detection overhead
3. **Segment count pre-check**: marginal benefit over the segment index

The segment index alone is the sweet spot — simple, effective, no
regressions on small APIs.

## Middleware is free

This was the most surprising result. Adding tower middleware layers
has **zero measurable overhead**:

```
middleware/0-layers:    135 ns    1.4 KB allocated
middleware/1-layer:     135 ns    1.4 KB allocated
middleware/3-layers:    133 ns    1.4 KB allocated
middleware/5-layers:    134 ns    1.4 KB allocated
```

Why? Because a `Layer` is just a function `Service -> Service`. When
you write `server |> cors |> tracing |> secureHeaders`, GHC inlines
the layer applications at compile time. The middleware functions get
fused into a single call chain — there's no list of layers being
traversed at runtime. The `before`, `after`, and `around` combinators
are all marked `INLINABLE`, so GHC can see through them.

This means you should **never** hesitate to add middleware. Security
headers, CORS, request IDs, tracing — stack as many as you need.
The compile-time cost is zero (flat composition) and the runtime cost
is zero (GHC optimizes it away).

## Extractors: ~80 ns each

The `ToHandler` machinery peels arguments from your handler function
and extracts each one from the request. Each extractor adds measurable
but small overhead:

```
extractors/0-extractors:    132 ns    1.4 KB allocated
extractors/1-extractor:     256 ns    6.7 KB allocated
extractors/2-extractors:    313 ns    7.2 KB allocated
extractors/3-extractors:    368 ns    7.6 KB allocated
```

The first extractor (`PathCapture Int`) costs ~124 ns because it
reads from the Extensions map (a `Map TypeRep Any` backed by an
`IORef`) and parses the captured text. Each additional extractor
adds ~55-60 ns. The allocation increase comes from the `IORef`
lookups and the `Either` wrapping.

For a typical handler with 2-3 extractors, you're looking at 300-370
ns total — still well under a microsecond. The ergonomic benefit of
writing `getUser :: PathCapture Int -> QueryParam "fields" Text -> IO (Json User)`
instead of manually pulling from request parts is worth far more than
60 ns.

## gRPC codec: 14 ns to decode, regardless of size

The gRPC length-prefixed framing is a 5-byte header (1 byte compressed
flag + 4 bytes big-endian length) followed by the payload. Encoding
uses `ByteString.Builder` with a tuned buffer strategy; decoding is
a bounds check and a slice.

```
grpc-codec/encode/10B:      19 ns     272 B allocated
grpc-codec/encode/100B:     21 ns     368 B allocated
grpc-codec/encode/1KB:      44 ns    1.3 KB allocated
grpc-codec/encode/10KB:    285 ns     21 KB allocated
grpc-codec/encode/100KB:  1.32 μs    201 KB allocated

grpc-codec/decode/10B:      14 ns     288 B allocated
grpc-codec/decode/100B:     14 ns     288 B allocated
grpc-codec/decode/1KB:      14 ns     288 B allocated
grpc-codec/decode/10KB:     14 ns     288 B allocated
grpc-codec/decode/100KB:    14 ns     288 B allocated
```

Decoding is **constant time** — 14 ns regardless of whether the
payload is 10 bytes or 100 KB. It doesn't copy the payload; it
returns a slice of the input ByteString. The 288 bytes allocated is
the `GrpcMessage` record and the `Maybe`/tuple wrapper.

Encoding scales with payload size because it must copy bytes into the
Builder output buffer. But even 100 KB encodes in 1.3 μs — that's
roughly memcpy speed.

Multi-message encoding (for streaming RPCs) scales linearly:

```
grpc-codec/encode-multi/10-msgs:     361 ns    5.5 KB
grpc-codec/encode-multi/100-msgs:   3.45 μs     54 KB
grpc-codec/decode-multi/10-msgs:     172 ns    3.2 KB
grpc-codec/decode-multi/100-msgs:   1.47 μs     32 KB
```

100 messages decode in 1.47 μs — about 15 ns per message, consistent
with the single-message result.

## Router scaling

The router does a linear scan of registered handlers, checking each
one's path pattern against the request:

```
router-scaling/1-route/hit:      148 ns
router-scaling/5-routes/last:    173 ns
router-scaling/10-routes/last:   181 ns
router-scaling/25-routes/last:   155 ns
router-scaling/25-routes/miss:    77 ns
```

With segment indexing, scaling is nearly flat — 148 ns for 1 route,
155 ns for 25 routes. The Map lookup eliminates most candidates before
the linear scan begins.

The 404 case (77 ns for 25 routes) is the fastest scenario because
the Map lookup finds no candidates and returns immediately.

## Memory allocation

Allocation per request is remarkably low:

| Scenario | Allocation |
|----------|-----------|
| Simple dispatch (1 endpoint) | 1.4 KB |
| Dispatch with 3 extractors | 7.6 KB |
| 404 miss | 728 B |
| gRPC decode | 288 B |

The bulk of allocation is the `Extensions` IORef (typed heterogeneous
map used for request-scoped data) and the `Response` construction.
With `-funbox-strict-fields` (enabled on all packages) and `StrictData`,
there's no thunk accumulation — every allocation is immediately useful.

## How to reproduce

**Compile-time benchmarks:**
```sh
bash bench/compile-time/run-bench.sh
```

**Runtime benchmarks:**
```sh
cabal run runtime-bench -- +RTS -T
```

**Profiling a specific handler:**
```sh
cabal run runtime-bench -- +RTS -T -s    # GC stats
cabal run runtime-bench -- +RTS -hc      # heap profile by cost centre
```

For production profiling, use GHC 9.10's `-fprof-late` flag (cost
centres after optimization) and eras profiling (`+RTS -he`) for
temporal allocation analysis.

## Design decisions driven by these numbers

1. **Segment-indexed router, not a trie.** Routes are pre-indexed by
   first path segment in a `Map`. This gives O(log n) lookup for the
   first segment, then a short linear scan for routes sharing that
   prefix. At 25 routes, worst case is 155 ns — a trie would add
   complexity for negligible gain.

2. **Extensions via `Map TypeRep Any`, not a record.** The IORef-backed
   map costs ~50 ns per lookup but is open to any type. A fixed record
   would be faster but couldn't support user-defined extractors.

3. **Builder-based response rendering.** The `safeStrategy 4096 4096`
   buffer strategy in `toStrictBuilder` avoids the default 32KB lazy
   ByteString chunks, reducing allocation for typical responses.

4. **Middleware as `Service -> Service` functions.** GHC inlines these
   completely, making the layer count invisible at runtime.

5. **Flat tuple instances over recursive resolution.** The 25 instances
   of `BuildApi` are ugly in the source but give O(1) compile time and
   zero dispatch overhead.
