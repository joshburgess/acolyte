# Streaming request and response bodies

servant-reimagined has two body paths: a **strict path** for normal
handlers and a **streaming path** for large payloads. Most of the time
you use the strict path. This guide covers when and how to use streaming.

## The two paths

```
Strict path (default):
  Service IO (Request ByteString) (Response ByteString)
    ↑ servant-reimagined-server produces this
    ↑ mkServer, effectfulServer, all handlers use this

Streaming path:
  Service IO (Request Body) (Response Body)
    ↑ tower-server and tower-wai consume this
    ↑ http-core defines the Body type
```

The `adaptToBody` function in tower-server bridges between them: it
collects the incoming body to strict bytes, runs your handler, then
wraps the response bytes as a `Body`.

For most APIs (JSON endpoints, form submissions, small payloads), the
strict path is all you need. The entire body is in memory as a
`ByteString` by the time your handler runs.

## When you need streaming

- **Large file uploads**: collecting a 500 MB file into a `ByteString`
  before your handler runs will spike memory
- **Large file downloads**: serializing a large response into a
  `ByteString` before sending defeats the purpose
- **Server-sent events**: responses that stay open and send chunks
  incrementally
- **Proxying**: forwarding a request body to another service without
  buffering it

## The Body type

`http-core` defines `Body` with four variants:

```haskell
data Body
  = BodyStrict !ByteString           -- already in memory
  | BodyStream (IO (Maybe BodyChunk)) -- pull-based stream
  | BodyFile   !FilePath !(Maybe FileRange)  -- served from disk
  | BodyEmpty                         -- no body
```

`BodyStream` is a pull-based producer. The consumer calls the IO action
repeatedly. It returns `Just (Chunk bytes)` for data, `Just ChunkFlush`
as a flush hint, and `Nothing` when done.

## Streaming responses

Write a handler that works at the `Body` level by building a `Service`
directly instead of going through `mkServer`:

```haskell
import Http.Core
import Http.Core.Body
import Tower.Service (Service (..))
import Tower.Server (runServer)
import Data.IORef

-- Stream a large CSV file
streamCsv :: Service IO (Request Body) (Response Body)
streamCsv = Service $ \req -> do
  ref <- newIORef (csvRows myData)
  let pull = do
        rows <- readIORef ref
        case rows of
          []     -> pure Nothing
          (r:rs) -> writeIORef ref rs >> pure (Just (Chunk r))
  pure $ Response
    status200
    [("Content-Type", "text/csv")]
    (BodyStream pull)

-- Serve a file from disk (enables sendfile(2) zero-copy)
serveFile :: FilePath -> Service IO (Request Body) (Response Body)
serveFile path = Service $ \_ ->
  pure $ Response
    status200
    [("Content-Type", "application/octet-stream")]
    (BodyFile path Nothing)
```

Run it directly on tower-server (which speaks `Body` natively):

```haskell
main :: IO ()
main = runServer 3000 streamCsv
```

## Streaming requests

For large uploads, skip the strict `ByteString` path and work with
`Body` directly:

```haskell
import Http.Core.Body (foldBodyChunks, bodyToStrict)

handleUpload :: Service IO (Request Body) (Response Body)
handleUpload = Service $ \req -> do
  -- Option 1: process chunks incrementally (constant memory)
  totalSize <- foldBodyChunks
    (\acc chunk -> pure (acc + BS.length chunk))
    0
    (requestBody req)

  -- Option 2: collect to strict bytes (if it fits in memory)
  allBytes <- bodyToStrict (requestBody req)

  pure $ Response status200 [] (fromBytes "uploaded")
```

## Mixing strict handlers with streaming endpoints

You can run your normal `mkServer` API alongside streaming endpoints
by combining services:

```haskell
import Tower.Server (runServer, adaptToBody)

main :: IO ()
main = do
  -- Normal API (strict ByteString handlers)
  let api = mkServer @MyAPI handlers

  -- Streaming endpoint
  let fileDownload = Service $ \req ->
        if requestPath req == ["download"]
        then pure $ Response status200 [] (BodyFile "large.bin" Nothing)
        else runService (adaptToBody api) req

  runServer 3000 fileDownload
```

The `adaptToBody` adapter bridges your strict service into the
streaming world. For the normal API routes, it collects the body and
runs your handler. For the download route, it serves the file directly.

## Constructing streaming bodies

```haskell
import Http.Core.Body

-- From a list (for tests and simple cases)
body <- streamFromList ["chunk1", "chunk2", "chunk3"]

-- From an IORef (pull-based)
ref <- newIORef chunks
let body = streamBody $ do
      cs <- readIORef ref
      case cs of
        []     -> pure Nothing
        (c:rs) -> writeIORef ref rs >> pure (Just (Chunk c))

-- From a file
let body = fileBody "/path/to/file.bin" Nothing

-- From a file with byte range (for Range requests)
let body = fileBody "/path/to/file.bin" (Just (FileRange 1024 4096))

-- Strict (small payloads)
let body = fromBytes "hello"
let body = fromText "hello"
let body = fromLazyBytes lazyBs
```

## Consuming bodies

```haskell
import Http.Core.Body

-- Collect to strict ByteString
bytes <- bodyToStrict someBody

-- Fold over chunks (constant memory)
hash <- foldBodyChunks
  (\ctx chunk -> pure (hashUpdate ctx chunk))
  hashInit
  someBody

-- Check if empty
if isEmptyBody someBody then ... else ...
```

## tower-server vs tower-wai

Both backends consume `Service IO (Request Body) (Response Body)`:

- **tower-server** handles `BodyStream` by pulling chunks and sending
  them over the socket. `BodyFile` reads from disk. No WAI involved.
- **tower-wai** converts `Body` to WAI's `responseStream`,
  `responseFile`, or `responseLBS` depending on the variant.

Your streaming code works with either backend without changes.
