-- | API-level combinators: the completeness check and handler binding.
--
-- The key design decision: APIs are type-level lists of endpoints.
-- The 'Serves' constraint is a type synonym that uses 'CheckArity'
-- to compare the API length against the handler tuple arity via
-- closed type families. No class, no instances, no recursive
-- constraint resolution — O(1) for the compiler.
module Acolyte.Core.API
  ( -- * API specification
    type API
    -- * Completeness check
  , Serves
    -- * Length checking
  , Length
  , TupleArity
  , type (==)
    -- * Arity checking
  , CheckArity
    -- * Type-level list append
  , type (++)
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


-- | Compute the arity of a handler tuple.
-- Covers arities 0–25 (matching 'BuildServer' coverage).
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
  TupleArity (a, b, c, d, e, f, g, h, i) = 9
  TupleArity (a, b, c, d, e, f, g, h, i, j) = 10
  TupleArity (a, b, c, d, e, f, g, h, i, j, k) = 11
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l) = 12
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m) = 13
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = 14
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = 15
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = 16
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = 17
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = 18
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = 19
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = 20
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = 21
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = 22
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = 23
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = 24
  TupleArity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = 25
  TupleArity _                  = 1  -- bare value = single handler


-- | Type-level list append.
--
-- Used to combine sub-APIs into a larger API:
--
-- @
-- type FullAPI = UsersAPI ++ ArticlesAPI ++ CommentsAPI
-- @
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++


-- | Custom error message for handler/endpoint count mismatch.
type HandlerMismatchMsg :: Nat -> Nat -> ErrorMessage
type family HandlerMismatchMsg (nEndpoints :: Nat) (nHandlers :: Nat) :: ErrorMessage where
  HandlerMismatchMsg n m =
    'Text "API has " ':<>: 'ShowType n ':<>: 'Text " endpoint(s) but "
    ':<>: 'ShowType m ':<>: 'Text " handler(s) were provided."
    ':$$: 'Text "Every endpoint must have exactly one handler."


-- | Verify that the API endpoint count matches the handler count.
-- Produces a clear 'TypeError' on mismatch instead of a confusing
-- GHC constraint error.
type CheckArity :: Nat -> Nat -> Constraint
type family CheckArity (apiLen :: Nat) (handlerCount :: Nat) :: Constraint where
  CheckArity n n = ()
  CheckArity n m = TypeError (HandlerMismatchMsg n m)


-- | The API completeness check.
--
-- @Serves api handlers@ holds when the handler tuple arity matches
-- the number of endpoints in the API list. If they don't match,
-- produces a clear compile error:
--
-- @
-- API has 4 endpoint(s) but 3 handler(s) were provided.
-- Every endpoint must have exactly one handler.
-- @
--
-- This is a constraint synonym — no class, no instances, no
-- overlapping pragmas. 'CheckArity' does the work via a closed
-- type family that reduces to @()@ on match or 'TypeError' on
-- mismatch.
type Serves (api :: [Type]) (handlers :: Type) =
  CheckArity (Length api) (TupleArity handlers)

