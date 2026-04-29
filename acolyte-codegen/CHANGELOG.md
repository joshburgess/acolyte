# Revision history for acolyte-codegen

## 0.1.0.1 -- 2026-04-28

* Bump `cabal-version` to 3.12. Older `cabal-install` (shipped with
  GHC < 9.10) was parsing the file before rejecting it on
  `default-language: GHC2024`, producing PlanningFailed on
  Hackage's GHC 9.8 build matrix row. Bumping forces clean
  rejection on older toolchains. No code changes.

## 0.1.0.0 -- 2026-04-27

* Initial release. Generates acolyte API types from OpenAPI and
  Swagger specs, plus protobuf schema emission and diffing.
