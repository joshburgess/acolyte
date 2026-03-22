-- | Type-level path encoding using promoted lists and GHC.TypeLits.
--
-- Paths are type-level lists of 'PathSegment's. Literal segments use
-- 'Symbol' from GHC.TypeLits directly — no marker types, no TH, no
-- proc macros. This is one of the main advantages over typeway's Rust
-- implementation, which needs proc macros to work around the absence
-- of const generic strings.
--
-- @
-- type UsersPath    = '[ Lit "users" ]
-- type UserByIdPath = '[ Lit "users", Capture Int ]
-- type PostsPath    = '[ Lit "users", Capture Int, Lit "posts", Capture Int ]
-- @
module Servant.Reimagined.Core.Path
  ( -- * Path segment kind
    PathSegment (..)
    -- * Type families
  , Captures
  , CapturesTuple
  , CountCaptures
    -- * Re-exports for convenience
  , Symbol
  , Nat
  , Type
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, Nat, type (+))
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))


-- | A segment in a URL path, used at the type level.
--
-- * @Lit s@ matches the literal string @s@ (e.g., @Lit "users"@).
-- * @Capture t@ captures a path segment parsed as type @t@ (e.g., @Capture Int@).
-- * @CaptureRest@ captures all remaining segments as @[Text]@.
data PathSegment
  = Lit Symbol        -- ^ Literal path segment
  | Capture Type      -- ^ Captured path parameter
  | CaptureRest       -- ^ Wildcard tail capture


-- | Extract the list of captured types from a path.
--
-- This is a closed type family — evaluated top-to-bottom by GHC with
-- no backtracking. O(n) in path length, no recursive instance resolution.
--
-- @
-- Captures '[]                              ~ '[]
-- Captures '[ Lit "users" ]                 ~ '[]
-- Captures '[ Lit "users", Capture Int ]    ~ '[Int]
-- Captures '[ Capture Int, Capture String ] ~ '[Int, String]
-- @
type Captures :: [PathSegment] -> [Type]
type family Captures (path :: [PathSegment]) :: [Type] where
  Captures '[]                    = '[]
  Captures (Lit _       ': rest)  = Captures rest
  Captures (Capture t   ': rest)  = t ': Captures rest
  Captures (CaptureRest ': _)     = '[[String]]


-- | Count the number of captures in a path (for arity checking).
type CountCaptures :: [PathSegment] -> Nat
type family CountCaptures (path :: [PathSegment]) :: Nat where
  CountCaptures '[]                    = 0
  CountCaptures (Lit _       ': rest)  = CountCaptures rest
  CountCaptures (Capture _   ': rest)  = 1 + CountCaptures rest
  CountCaptures (CaptureRest ': _)     = 1


-- | Convert a type-level list of capture types to a tuple.
--
-- Single captures are unwrapped for ergonomics: @Capture Int@ gives the
-- handler an @Int@, not a @(Int,)@.
--
-- @
-- CapturesTuple '[]              ~ ()
-- CapturesTuple '[Int]           ~ Int
-- CapturesTuple '[Int, String]   ~ (Int, String)
-- CapturesTuple '[Int, String, Bool] ~ (Int, String, Bool)
-- @
type CapturesTuple :: [Type] -> Type
type family CapturesTuple (cs :: [Type]) :: Type where
  CapturesTuple '[]              = ()
  CapturesTuple '[a]             = a
  CapturesTuple '[a, b]          = (a, b)
  CapturesTuple '[a, b, c]       = (a, b, c)
  CapturesTuple '[a, b, c, d]    = (a, b, c, d)
  CapturesTuple '[a, b, c, d, e] = (a, b, c, d, e)
  CapturesTuple '[a, b, c, d, e, f]          = (a, b, c, d, e, f)
  CapturesTuple '[a, b, c, d, e, f, g]       = (a, b, c, d, e, f, g)
  CapturesTuple '[a, b, c, d, e, f, g, h]    = (a, b, c, d, e, f, g, h)
