-- | @acolyte@ — composable, type-safe web framework.
--
-- This is the facade package. For most uses, import
-- 'Acolyte.Prelude' which re-exports everything.
--
-- For more control, import individual packages:
--
-- * "Acolyte.Core" — type-level API types
-- * "Acolyte.Server" — server interpretation
-- * "Acolyte.Client" — client interpretation
-- * "Acolyte.OpenApi" — OpenAPI spec generation
-- * "Spire" — Service\/Layer\/Middleware composition
-- * "Http.Core" — backend-agnostic Request\/Response
module Acolyte
  ( module Acolyte.Prelude
  ) where

import Acolyte.Prelude
