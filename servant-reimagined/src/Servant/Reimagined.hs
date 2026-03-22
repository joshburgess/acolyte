-- | @servant-reimagined@ — composable, type-safe web framework.
--
-- This is the facade package. For most uses, import
-- 'Servant.Reimagined.Prelude' which re-exports everything.
--
-- For more control, import individual packages:
--
-- * "Servant.Reimagined.Core" — type-level API types
-- * "Servant.Reimagined.Server" — server interpretation
-- * "Servant.Reimagined.Client" — client interpretation
-- * "Servant.Reimagined.OpenApi" — OpenAPI spec generation
-- * "Tower" — Service\/Layer\/Middleware composition
-- * "Http.Core" — backend-agnostic Request\/Response
module Servant.Reimagined
  ( module Servant.Reimagined.Prelude
  ) where

import Servant.Reimagined.Prelude
