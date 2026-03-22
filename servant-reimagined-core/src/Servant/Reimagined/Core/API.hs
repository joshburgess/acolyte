-- | API-level combinators: the completeness check and handler binding.
--
-- The key design decision: APIs are type-level lists of endpoints, and
-- the 'Serves' constraint uses a type class with flat tuple instances
-- (generated for arities 1–16) rather than recursive resolution.
--
-- This avoids Servant's exponential compile-time blowup from recursive
-- ':<|>' constraint solving. Each tuple arity is a single, direct instance
-- — the compiler does O(1) work per API instantiation.
module Servant.Reimagined.Core.API
  ( -- * API specification
    type API
    -- * Completeness check
  , Serves (..)
    -- * Length checking
  , Length
  , TupleArity
  , type (==)
    -- * Custom type errors
  , HandlerMismatchMsg
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Nat, type (+), TypeError, ErrorMessage (..))
import Data.Type.Equality (type (==))
import Data.Type.Bool (If)


-- | An API is a type-level list of endpoint types.
--
-- @
-- type UsersAPI =
--   '[ Get  '[ Lit "users" ]                (Json [User])
--    , Get  '[ Lit "users", Capture Int ]    (Json User)
--    , Post '[ Lit "users" ] (Json CreateUser) (Json User)
--    ]
-- @
type API = [Type]


-- | Compute the length of a type-level list.
type Length :: [k] -> Nat
type family Length (xs :: [k]) :: Nat where
  Length '[]       = 0
  Length (_ ': xs) = 1 + Length xs


-- | Compute the arity of a handler tuple (for error messages).
type TupleArity :: Type -> Nat
type family TupleArity h :: Nat where
  TupleArity ()                 = 0
  TupleArity (a, b)             = 2
  TupleArity (a, b, c)          = 3
  TupleArity (a, b, c, d)       = 4
  TupleArity (a, b, c, d, e)    = 5
  TupleArity (a, b, c, d, e, f) = 6
  TupleArity (a, b, c, d, e, f, g) = 7
  TupleArity (a, b, c, d, e, f, g, h) = 8
  TupleArity _                  = 1  -- bare value = single handler


-- | Custom error message for handler/endpoint count mismatch.
type HandlerMismatchMsg :: Nat -> Nat -> ErrorMessage
type family HandlerMismatchMsg (nEndpoints :: Nat) (nHandlers :: Nat) :: ErrorMessage where
  HandlerMismatchMsg n m =
    'Text "API has " ':<>: 'ShowType n ':<>: 'Text " endpoint(s) but "
    ':<>: 'ShowType m ':<>: 'Text " handler(s) were provided."
    ':$$: 'Text "Every endpoint must have exactly one handler."


-- | The API completeness check.
--
-- @Serves api handlers@ holds when @handlers@ is a tuple whose length
-- matches the length of @api@. Each position in the handler tuple
-- corresponds to the endpoint at the same position in the API list.
--
-- The handler types themselves are opaque at this level — the server
-- package adds additional constraints (correct extractors, compatible
-- return types) when it interprets the API. The core only checks count.
--
-- The design uses a class with flat instances per arity rather than
-- recursive resolution. This is O(1) per instantiation for the compiler.
--
-- @
-- -- This compiles (3 endpoints, 3 handlers):
-- example :: Serves '[E1, E2, E3] (H1, H2, H3) => ()
--
-- -- This fails (3 endpoints, 2 handlers):
-- bad :: Serves '[E1, E2, E3] (H1, H2) => ()
-- @
type Serves :: [Type] -> Type -> Constraint
class Serves (api :: [Type]) handlers where
  -- | Witness that the handler tuple is complete.
  --
  -- The server package overrides this with actual handler registration.
  -- At the core level, this is just a unit witness that forces GHC to
  -- solve the constraint (and fail if the handler count is wrong).
  handlerCount :: Int

-- Arity 1
instance
  ( Length api ~ 1
  , api ~ '[e1]
  ) => Serves api h1 where
  handlerCount = 1

-- Arity 2
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 2
  , api ~ '[e1, e2]
  ) => Serves api (h1, h2) where
  handlerCount = 2

-- Arity 3
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 3
  , api ~ '[e1, e2, e3]
  ) => Serves api (h1, h2, h3) where
  handlerCount = 3

-- Arity 4
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 4
  , api ~ '[e1, e2, e3, e4]
  ) => Serves api (h1, h2, h3, h4) where
  handlerCount = 4

-- Arity 5
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 5
  , api ~ '[e1, e2, e3, e4, e5]
  ) => Serves api (h1, h2, h3, h4, h5) where
  handlerCount = 5

-- Arity 6
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 6
  , api ~ '[e1, e2, e3, e4, e5, e6]
  ) => Serves api (h1, h2, h3, h4, h5, h6) where
  handlerCount = 6

-- Arity 7
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 7
  , api ~ '[e1, e2, e3, e4, e5, e6, e7]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7) where
  handlerCount = 7

-- Arity 8
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 8
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8) where
  handlerCount = 8

-- Arity 9
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 9
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9) where
  handlerCount = 9

-- Arity 10
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 10
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10) where
  handlerCount = 10

-- Arity 11
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 11
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11) where
  handlerCount = 11

-- Arity 12
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 12
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12) where
  handlerCount = 12

-- Arity 13
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 13
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13) where
  handlerCount = 13

-- Arity 14
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 14
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14) where
  handlerCount = 14

-- Arity 15
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 15
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15) where
  handlerCount = 15

-- Arity 16
instance
  {-# OVERLAPPING #-}
  ( Length api ~ 16
  , api ~ '[e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16]
  ) => Serves api (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16) where
  handlerCount = 16


