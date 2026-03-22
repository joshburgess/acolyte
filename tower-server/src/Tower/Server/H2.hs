-- | HTTP/2 support for tower-server (placeholder).
--
-- This module provides the types and entry points for HTTP/2 support.
-- HTTP/2 dispatches requests to the same tower Service as HTTP/1.1.
--
-- Full HTTP/2 support requires the @http2@ package and is more complex
-- than HTTP/1.1 due to multiplexed streams, HPACK compression, and
-- flow control. This module will be fleshed out when we integrate
-- with the http2 package's server API.
--
-- For now, HTTP/1.1 via 'Tower.Server.runServer' is the primary path.
-- HTTP/2 will be added as a transport option alongside HTTP/1.1 and TLS.
module Tower.Server.H2
  ( -- * Configuration
    H2Config (..)
  , defaultH2Config
  ) where


-- | HTTP/2 server configuration.
data H2Config = H2Config
  { h2Port         :: !Int
  , h2Host         :: !String
  , h2Workers      :: !Int    -- ^ Number of worker threads for multiplexed streams
  , h2MaxStreams    :: !Int    -- ^ Max concurrent streams per connection
  } deriving (Show)

-- | Sensible defaults for HTTP/2.
defaultH2Config :: Int -> H2Config
defaultH2Config port = H2Config
  { h2Port      = port
  , h2Host      = "127.0.0.1"
  , h2Workers   = 4
  , h2MaxStreams = 100
  }
