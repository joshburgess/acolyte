# gRPC support

acolyte serves gRPC from the same API type that drives REST,
OpenAPI generation, and type-safe clients. One type, four interpretations.

## Quick start

```haskell
import Acolyte.Core
import Acolyte.Server (Json)
import Acolyte.Grpc
import Spire.Grpc (grpcServer)
import Spire.Server.H2 (runServerH2, defaultH2Config)

type HealthPath = '[ 'Lit "health" ]
type UsersPath  = '[ 'Lit "users" ]

type API =
  '[ Get  HealthPath Text
   , Post UsersPath  (Json CreateUser) (Json User)
   ]

-- gRPC handlers (raw bytes in, raw bytes out)
healthH :: GrpcHandlerFn
healthH _ = pure $ Right (grpcEncode ("ok" :: Text))

createUserH :: GrpcHandlerFn
createUserH reqBytes = case grpcDecode reqBytes of
  Just (CreateUser name) -> pure $ Right (grpcEncode (User name))
  Nothing -> pure $ Left "invalid request"

main :: IO ()
main = do
  let services = mkGrpcServiceMap @API "myapp" "MyService"
        (healthH, createUserH)
  runServerH2 (defaultH2Config 50051) (grpcServer services)
```

Test with grpcurl:
```sh
grpcurl -plaintext -d '{}' localhost:50051 myapp.MyService/Health
grpcurl -plaintext -d '{"name":"alice"}' localhost:50051 myapp.MyService/Users
```

## How it works

### Same API type, different interpretation

```haskell
type API = '[ Get HealthPath Text, Post UsersPath (Json CreateUser) (Json User) ]

-- REST server (acolyte-server):
restSvc = mkServer @API restHandlers

-- gRPC server (acolyte-grpc):
grpcSvc = grpcServer (mkGrpcServiceMap @API "pkg" "Svc" grpcHandlers)

-- OpenAPI spec (acolyte-openapi):
spec = generateSpec @API "My API" "1.0"

-- .proto file (acolyte-grpc):
proto = generateProto @API "pkg" "Svc"
```

### Method naming

gRPC method names are derived from the first literal path segment,
capitalized:

| Path | gRPC Method |
|------|------------|
| `'[ 'Lit "health" ]` | `Health` |
| `'[ 'Lit "users" ]` | `Users` |
| `'[ 'Lit "users", 'Capture Int ]` | `Users` (capture skipped) |

The full gRPC path becomes `/{package}.{service}/{method}`, e.g.
`/myapp.MyService/Health`.

### Serialization

`GrpcCodec` is the serialization class. It's deliberately minimal:
no proto-lens dependency, no code generation required.

```haskell
class GrpcCodec a where
  grpcEncode :: a -> ByteString
  grpcDecode :: ByteString -> Maybe a
```

Implement it with whatever codec you use:

```haskell
-- With proto-lens:
instance GrpcCodec MyProtoMessage where
  grpcEncode = Proto.encodeMessage
  grpcDecode = either (const Nothing) Just . Proto.decodeMessage

-- With aeson (for grpc+json):
instance GrpcCodec MyType where
  grpcEncode = LBS.toStrict . Aeson.encode
  grpcDecode = Aeson.decodeStrict'

-- Manual:
instance GrpcCodec HealthStatus where
  grpcEncode (HealthStatus t) = TE.encodeUtf8 t
  grpcDecode bs = Just (HealthStatus (TE.decodeUtf8 bs))
```

### Compile-time validation

`GrpcReady` checks at compile time that every request and response
type in the API has a `GrpcCodec` instance:

```haskell
-- This compiles only if Text, CreateUser, and User all have GrpcCodec:
server :: GrpcReady API => ...
```

Missing a `GrpcCodec` instance? You get a compile error, not a
runtime crash.

## .proto generation

Generate a `.proto` file directly from the API type:

```haskell
let proto = generateProto @API "myapp" "UserService"
```

Produces:
```protobuf
syntax = "proto3";

package myapp;

service UserService {
  rpc Health (Empty) returns (StringValue);
  rpc Users (CreateUser) returns (User);
}

// Message definitions (generate from HasProtoFields or write manually):
// message CreateUser { ... }
// message User { ... }
```

For full message definitions, implement `HasProtoFields` on your types:

```haskell
instance HasProtoFields User where
  protoFields =
    [ ProtoField "name" 1 ProtoString
    , ProtoField "email" 2 ProtoString
    , ProtoField "id" 3 ProtoInt64
    ]
```

## Architecture

```
acolyte-grpc     API type -> GrpcReady check, .proto, mkGrpcServiceMap
         |
    spire-grpc              gRPC wire protocol: framing, status, dispatch
         |
    spire-server/H2         HTTP/2 transport (via http2 package)
         |
      network               TCP sockets (zero WAI)
```

Each layer is independent:

- `spire-grpc` knows nothing about API types: it dispatches from
  a `GrpcServiceMap` of `(service, method) -> handler`
- `acolyte-grpc` builds the service map from the API type
- `spire-server/H2` handles HTTP/2 framing and doesn't know about gRPC
- The same `Service IO (Request Body) (Response Body)` abstraction
  connects all layers

## Status codes

All 17 gRPC status codes are provided:

```haskell
grpcOk, grpcCancelled, grpcUnknown, grpcInvalidArgument,
grpcDeadlineExceeded, grpcNotFound, grpcAlreadyExists,
grpcPermissionDenied, grpcResourceExhausted, grpcFailedPrecondition,
grpcAborted, grpcOutOfRange, grpcUnimplemented, grpcInternal,
grpcUnavailable, grpcDataLoss, grpcUnauthenticated :: Text -> GrpcStatus
```

Error responses use `Left`:

```haskell
myHandler :: GrpcHandlerFn
myHandler reqBytes = case grpcDecode reqBytes of
  Nothing -> pure $ Left "invalid request"  -- becomes grpcInternal
  Just req -> do
    result <- processRequest req
    case result of
      Nothing -> pure $ Left "not found"
      Just r  -> pure $ Right (grpcEncode r)
```

## Server streaming

```haskell
listHandler :: ByteString -> IO (Either GrpcStatus [ByteString])
listHandler reqBytes = do
  items <- queryItems (grpcDecode reqBytes)
  pure $ Right (map grpcEncode items)

let services = grpcServiceMap
      [ ("pkg.Svc", "List", serverStreamHandler listHandler) ]
```

## Client streaming

The client sends multiple messages; the server collects them and returns
a single response.

```haskell
import Spire.Grpc (clientStreamHandler, grpcServiceMap, grpcServer)

recordRoute :: [ByteString] -> IO (Either GrpcStatus ByteString)
recordRoute points = do
  let count = length points
  pure $ Right (grpcEncode (RouteSummary count))

let services = grpcServiceMap
      [ ("pkg.Svc", "RecordRoute", clientStreamHandler recordRoute) ]
```

The handler receives a `[ByteString]` -- one entry per length-prefixed
message from the client. Return `Left` with a `GrpcStatus` to signal
an error.

## Bidirectional streaming

Both sides send multiple messages. The current implementation is
buffered: all client messages are collected before processing.

```haskell
import Spire.Grpc (bidiStreamHandler, grpcServiceMap, grpcServer)

routeChat :: [ByteString] -> IO (Either GrpcStatus [ByteString])
routeChat notes = do
  responses <- mapM processNote notes
  pure $ Right responses

let services = grpcServiceMap
      [ ("pkg.Svc", "RouteChat", bidiStreamHandler routeChat) ]
```

The handler returns `[ByteString]` -- each entry becomes a separate
length-prefixed gRPC message in the response stream.

## Health check service

Add the standard `grpc.health.v1.Health/Check` endpoint to any gRPC
server with `withHealthCheck`:

```haskell
import Spire.Grpc (grpcServer, grpcServiceMap)
import Spire.Grpc.Health (withHealthCheck, HealthStatus (..))

main :: IO ()
main = do
  let services = withHealthCheck (pure Serving) $ grpcServiceMap
        [ ("myapp.Greeter", "SayHello", unaryHandler sayHello)
        ]
  runServerH2 (defaultH2Config 50051) (grpcServer services)
```

Test with grpcurl:
```sh
grpcurl -plaintext -d '{}' localhost:50051 grpc.health.v1.Health/Check
```

`HealthStatus` values: `Serving`, `NotServing`, `Unknown`.

## Compression

gRPC messages can be gzip-compressed. The `Spire.Grpc.Compression`
module provides encode/decode helpers:

```haskell
import Spire.Grpc.Compression

-- Compress a message payload before framing:
let compressed = encodeMessageCompressed payload

-- Decompress all compressed messages in a gRPC body:
let decompressed = decompressGrpcBody rawBody
```

When the 5-byte gRPC header has the compressed flag set (byte 0 = `0x01`),
the payload is gzip-compressed. The `grpc-encoding: gzip` header signals
the compression algorithm to the peer.

## Packages

| Package | What it does |
|---------|-------------|
| `spire-grpc` | gRPC wire protocol: 5-byte framing, status codes, service dispatch, multiplexing, reflection. No protobuf dependency. |
| `acolyte-grpc` | API type interpretation: `GrpcCodec`, `GrpcReady`, `.proto` generation, `mkGrpcServiceMap`. |
| `acolyte-codegen` | Code generation from `.proto` files to Haskell API types. |
| `spire-server` (H2 module) | HTTP/2 transport via the `http2` package. Zero WAI. |

## Multiplexing

Serve REST and gRPC on the same port using `multiplex`. It routes
requests by content-type: `application/grpc` goes to the gRPC service,
everything else goes to the REST service.

```haskell
import Spire.Grpc (grpcServer, multiplex)
import Spire.Server.H2 (runServerH2, defaultH2Config)

main :: IO ()
main = do
  let grpcSvc = grpcServer (mkGrpcServiceMap @API "myapp" "MyService" grpcHandlers)
  let restSvc = mkServer @API restHandlers
  let combined = multiplex (adaptToBody restSvc) grpcSvc
  runServerH2 (defaultH2Config 8080) combined
```

Both REST and gRPC share the same API type. Clients connect to the
same port and are routed automatically.

## Server reflection

Enable server reflection so that tools like `grpcurl` and `grpcui`
can discover services without a `.proto` file:

```haskell
import Spire.Grpc (grpcServer, withReflection)

main :: IO ()
main = do
  let services = mkGrpcServiceMap @API "myapp" "MyService" handlers
  let svc = withReflection services (grpcServer services)
  runServerH2 (defaultH2Config 50051) svc
```

Now `grpcurl` works without `-proto`:
```sh
grpcurl -plaintext localhost:50051 list
grpcurl -plaintext localhost:50051 describe myapp.MyService
grpcurl -plaintext -d '{}' localhost:50051 myapp.MyService/Health
```

## Complete .proto generation

`generateProto` produces a skeleton `.proto` with service and RPC
definitions but placeholder message comments. For complete `.proto`
files with full message definitions, use `generateProtoFull` with
`MessageDef` and `HasProtoFields`:

```haskell
import Acolyte.Grpc (generateProtoFull, MessageDef(..), HasProtoFields(..))

instance HasProtoFields CreateUser where
  protoFields =
    [ ProtoField "name" 1 ProtoString
    ]

instance HasProtoFields User where
  protoFields =
    [ ProtoField "name"  1 ProtoString
    , ProtoField "email" 2 ProtoString
    , ProtoField "id"    3 ProtoInt64
    ]

let proto = generateProtoFull @API "myapp" "UserService"
```

This produces a complete `.proto` with `message` blocks:
```protobuf
syntax = "proto3";

package myapp;

service UserService {
  rpc Health (Empty) returns (StringValue);
  rpc Users (CreateUser) returns (User);
}

message CreateUser {
  string name = 1;
}

message User {
  string name  = 1;
  string email = 2;
  int64  id    = 3;
}
```

## .proto to Haskell

The `acolyte-codegen` package can parse `.proto` files and
generate Haskell API types, closing the loop for interop with
existing gRPC ecosystems:

```haskell
import Acolyte.Codegen.Proto (parseProtoFile, generateHaskellApi)

main :: IO ()
main = do
  proto <- parseProtoFile "service.proto"
  let haskellCode = generateHaskellApi proto
  writeFile "Gen/Api.hs" haskellCode
```

This generates a module with the API type, `GrpcCodec` instances,
and `HasProtoFields` instances derived from the `.proto` message
definitions. Combined with `generateProtoFull` in the other
direction, you get round-trip compatibility between `.proto` and
Haskell API types.

## Content negotiation

REST endpoints served alongside gRPC support format negotiation via
the `Negotiate` wrapper in the API type:

```haskell
type API =
  '[ Get HealthPath (Negotiate '[Json, PlainText] HealthStatus)
   , Post UsersPath (Json CreateUser) (Negotiate '[Json, Yaml] User)
   ]
```

The server inspects the `Accept` header and selects the best matching
format. gRPC interpretation ignores `Negotiate` and uses `GrpcCodec`
directly. This means the same API type works for both REST (with
content negotiation) and gRPC (with protobuf or JSON codec).
