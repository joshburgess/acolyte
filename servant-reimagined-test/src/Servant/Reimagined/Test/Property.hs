-- | Property-based testing for servant-reimagined APIs.
--
-- Generate random valid requests for any endpoint in an API type.
--
-- @
-- prop_noCrash :: Property
-- prop_noCrash = forAll (arbitraryApiRequest @MyAPI) $ \req -> ioProperty $ do
--   resp <- runService myServer req
--   pure (responseStatus' resp `elem` [200, 201, 400, 401, 404])
-- @
module Servant.Reimagined.Test.Property
  ( -- * Random request generation
    ArbitraryEndpoint (..)
  , ArbitraryAPI (..)
    -- * Helpers
  , randomCapture
  , generatePath
  , genJsonObject
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe (unsafePerformIO)

import Test.QuickCheck

import Http.Core
import Servant.Reimagined.Server.Handler (HasEndpointInfo (..))


-- | Generate a random valid request for a specific endpoint.
class ArbitraryEndpoint endpoint where
  arbitraryEndpointRequest :: Gen (Request ByteString)

instance HasEndpointInfo e => ArbitraryEndpoint e where
  arbitraryEndpointRequest = do
    let method  = endpointMethod @e
        pattern = endpointPattern @e
    pathSegments <- generatePath pattern
    let pathRaw = "/" <> BS8.intercalate "/" (map TE.encodeUtf8 pathSegments)
    body <- oneof [pure "", genJsonObject]
    let exts = unsafePerformIO emptyExtensions
    pure Request
      { requestMethod     = method
      , requestPathRaw    = pathRaw
      , requestPath       = pathSegments
      , requestQuery      = []
      , requestHeaders    = [("Content-Type", "application/json")]
      , requestBody       = body
      , requestExtensions = exts
      }


-- | Generate a random request for any endpoint in an API.
class ArbitraryAPI (api :: [Type]) where
  arbitraryApiRequest :: Gen (Request ByteString)

instance ArbitraryEndpoint e => ArbitraryAPI '[e] where
  arbitraryApiRequest = arbitraryEndpointRequest @e

instance (ArbitraryEndpoint e, ArbitraryAPI (e2 ': rest))
  => ArbitraryAPI (e ': e2 ': rest) where
    arbitraryApiRequest = oneof
      [ arbitraryEndpointRequest @e
      , arbitraryApiRequest @(e2 ': rest)
      ]


-- ===================================================================
-- Helpers
-- ===================================================================

-- | Generate a random capture value (int or short string).
randomCapture :: Gen Text
randomCapture = oneof
  [ T.pack . show <$> chooseInt (1, 9999)
  , T.pack <$> listOf1 (elements ['a'..'z'])
  ]


-- | Generate path segments from a pattern, replacing captures with random values.
generatePath :: Text -> Gen [Text]
generatePath pattern =
  let parts = filter (not . T.null) (T.splitOn "/" pattern)
  in mapM genSegment parts
  where
    genSegment seg
      | "{" `T.isPrefixOf` seg = randomCapture
      | otherwise = pure seg


-- | Generate a random small JSON object.
genJsonObject :: Gen ByteString
genJsonObject = do
  nFields <- chooseInt (0, 3)
  fields <- vectorOf nFields genField
  pure $ BS8.pack $ "{" ++ intercalateS "," fields ++ "}"
  where
    genField = do
      key <- listOf1 (elements ['a'..'z'])
      val <- oneof
        [ show <$> chooseInt (0 :: Int, 100)
        , (\s -> "\"" ++ s ++ "\"") <$> listOf1 (elements ['a'..'z'])
        ]
      pure $ "\"" ++ key ++ "\":" ++ val

    intercalateS _ [] = ""
    intercalateS _ [x] = x
    intercalateS sep (x:xs) = x ++ sep ++ intercalateS sep xs
