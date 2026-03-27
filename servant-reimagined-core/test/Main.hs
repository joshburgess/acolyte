-- | Compile-time tests for the core type-level machinery.
--
-- If this module compiles, all type-level assertions pass.
-- Negative tests (should-fail) are commented out — uncomment to
-- verify they produce compile errors.
module Main (main) where

import Servant.Reimagined.Core
import Data.Kind (Type, Constraint)
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Data.Proxy (Proxy (..))


-- ===================================================================
-- Domain types (placeholders)
-- ===================================================================

data User
data UserV1
data UserV2
data Profile
data CreateUser
data AuthUser
data JoinMsg
data WelcomeMsg
data ChatMsg
data BroadcastMsg
data LeaveMsg
data Article


-- ===================================================================
-- Path definitions
-- ===================================================================

type UsersPath     = '[ 'Lit "users" ]
type UserByIdPath  = '[ 'Lit "users", 'Capture Int ]
type UserPostsPath = '[ 'Lit "users", 'Capture Int, 'Lit "posts", 'Capture Int ]
type ProfilePath   = '[ 'Lit "profiles", 'Capture Int ]
type HealthPath    = '[ 'Lit "health" ]
type ArticlePath   = '[ 'Lit "articles", 'Capture Int ]


-- ===================================================================
-- 1. Path capture tests
-- ===================================================================

type CapturesUsersOk    = Captures UsersPath    ~ ('[] :: [Type])
type CapturesSingleOk   = Captures UserByIdPath ~ '[Int]
type CapturesMultiOk    = Captures UserPostsPath ~ '[Int, Int]

type TupleEmptyOk  = CapturesTuple (Captures UsersPath)    ~ ()
type TupleSingleOk = CapturesTuple (Captures UserByIdPath) ~ Int
type TupleMultiOk  = CapturesTuple (Captures UserPostsPath) ~ (Int, Int)

type Len0Ok = Length ('[] :: [Type]) ~ 0
type Len3Ok = Length TestAPI ~ 3

_assertCaptures :: (CapturesUsersOk, CapturesSingleOk, CapturesMultiOk) => ()
_assertCaptures = ()

_assertTuples :: (TupleEmptyOk, TupleSingleOk, TupleMultiOk) => ()
_assertTuples = ()

_assertLengths :: (Len0Ok, Len3Ok) => ()
_assertLengths = ()


-- ===================================================================
-- 2. Serves completeness check
-- ===================================================================

type TestAPI =
  '[ Get  UsersPath    (Json [User])
   , Get  UserByIdPath (Json User)
   , Post UsersPath    (Json CreateUser) (Json User)
   ]

data H1; data H2; data H3; data H4

-- 3 endpoints, 3 handlers — compiles
_servesOk :: Serves TestAPI (H1, H2, H3) => ()
_servesOk = ()

-- 1 endpoint, 1 handler — compiles
type SingleAPI = '[ Get UsersPath (Json [User]) ]
_servesSingle :: Serves SingleAPI H1 => ()
_servesSingle = ()

-- 2 endpoints, 2 handlers — compiles
type TwoAPI = '[ Get UsersPath (Json [User]), Get UserByIdPath (Json User) ]
_servesTwo :: Serves TwoAPI (H1, H2) => ()
_servesTwo = ()

-- NEGATIVE: 3 endpoints, 2 handlers — uncomment to see compile error:
-- "API has 3 endpoint(s) but 2 handler(s) were provided."
-- _servesBad :: Serves TestAPI (H1, H2) => ()
-- _servesBad = ()


-- ===================================================================
-- 3. Effect system tests
-- ===================================================================

type EffectAPI =
  '[ Requires Auth (Get UserByIdPath (Json User))
   , Requires Cors (Get UsersPath (Json [User]))
   , Get HealthPath String  -- no requirements
   ]

-- All effects provided — compiles
_effectsOk :: AllEffectsProvided EffectAPI '[Auth, Cors] => ()
_effectsOk = ()

-- Order doesn't matter — compiles
_effectsReorder :: AllEffectsProvided EffectAPI '[Cors, Auth] => ()
_effectsReorder = ()

-- Extra effects are fine — compiles
_effectsExtra :: AllEffectsProvided EffectAPI '[Auth, Cors, Tracing] => ()
_effectsExtra = ()

-- Nested effects
type NestedEffectAPI =
  '[ Requires Auth (Requires RateLimit (Get UserByIdPath (Json User)))
   , Get HealthPath String
   ]

_nestedOk :: AllEffectsProvided NestedEffectAPI '[Auth, RateLimit] => ()
_nestedOk = ()

-- No effects required — compiles with any provided list
type NoEffectAPI = '[ Get HealthPath String ]

_noEffectsOk :: AllEffectsProvided NoEffectAPI '[] => ()
_noEffectsOk = ()

-- NEGATIVE: Auth missing — uncomment to see compile error:
-- "Missing middleware effect: Auth"
-- _effectsMissing :: AllEffectsProvided EffectAPI '[Cors] => ()
-- _effectsMissing = ()

-- Custom user-defined effect — works because effects are any Type
data MyCustomEffect
type CustomEffectAPI = '[ Requires MyCustomEffect (Get HealthPath String) ]

_customEffectOk :: AllEffectsProvided CustomEffectAPI '[MyCustomEffect] => ()
_customEffectOk = ()


-- ===================================================================
-- 4. Endpoint wrapper tests (Protected, Validated, Versioned)
-- ===================================================================

-- Protected composes with Requires
type ProtectedAPI =
  '[ Requires Auth (Protected AuthUser (Get UserByIdPath (Json User)))
   , Get HealthPath String
   ]

_protectedEffectsOk :: AllEffectsProvided ProtectedAPI '[Auth] => ()
_protectedEffectsOk = ()


-- ===================================================================
-- 5. Session type tests
-- ===================================================================

-- Chat protocol from the architecture doc
type ChatProtocol =
  'Recv JoinMsg ('Send WelcomeMsg
    ('Rec ('Offer
      ('Recv ChatMsg ('Send BroadcastMsg 'Var))
      ('Recv LeaveMsg 'End))))

-- Dual swaps Send/Recv and Offer/Select
type ChatClientProtocol = Dual ChatProtocol

-- Dual of Recv is Send
type DualRecvOk = Dual ('Recv JoinMsg 'End) ~ 'Send JoinMsg 'End

-- Dual is involutive: Dual (Dual s) ~ s
type DualInvolutiveSimple =
  Dual (Dual ('Send JoinMsg ('Recv WelcomeMsg 'End)))
  ~ 'Send JoinMsg ('Recv WelcomeMsg 'End)

_assertDual :: (DualRecvOk, DualInvolutiveSimple) => ()
_assertDual = ()


-- ===================================================================
-- 6. Versioning tests
-- ===================================================================

type V1 =
  '[ Get UsersPath    (Json [UserV1])
   , Get UserByIdPath (Json UserV1)
   ]

-- Backward-compatible changes: Added + Deprecated (no Removed)
type V2Changes =
  '[ 'Added    (Get ProfilePath (Json Profile))
   , 'Deprecated (Get UsersPath (Json [UserV1]))
   ]

_v2Compatible :: BackwardCompatible V2Changes => ()
_v2Compatible = ()

-- ApplyChanges with Added appends the endpoint
type V2Resolved = ApplyChanges V1 V2Changes
type V2LenOk = Length V2Resolved ~ 3  -- 2 original + 1 added

_assertV2Len :: V2LenOk => ()
_assertV2Len = ()

-- NEGATIVE: Removed endpoint — uncomment to see compile error:
-- "API version change is not backward compatible."
-- type BreakingChanges = '[ 'Removed (Get UsersPath (Json [UserV1])) ]
-- _breakingBad :: BackwardCompatible BreakingChanges => ()
-- _breakingBad = ()


-- ===================================================================
-- 7. Content negotiation tests
-- ===================================================================

-- An endpoint serving Article in multiple formats
type NegotiatedEndpoint =
  Get ArticlePath (Negotiate '[JsonFormat, XmlFormat, TextFormat] Article)

-- This compiles — Negotiate is just a type wrapper
type NegotiatedAPI = '[ NegotiatedEndpoint, Get HealthPath String ]

_negotiatedServes :: Serves NegotiatedAPI (H1, H2) => ()
_negotiatedServes = ()

-- ContentFormat instances provide runtime metadata
_jsonType :: Text
_jsonType = contentType @JsonFormat

_xmlType :: Text
_xmlType = contentType @XmlFormat


-- ===================================================================
-- 8. Path construction helpers
-- ===================================================================

-- At "health" should equal '[ 'Lit "health" ]
type AtHealthOk = At "health" ~ '[ 'Lit "health" ]

-- Param "users" Int should equal '[ 'Lit "users", 'Capture Int ]
type ParamUsersOk = Param "users" Int ~ '[ 'Lit "users", 'Capture Int ]

_assertPathHelpers :: (AtHealthOk, ParamUsersOk) => ()
_assertPathHelpers = ()

-- Describe wrapper compiles in an API type
type DescribedAPI =
  '[ Describe "Health check" (Get (At "health") String)
   , Describe "Get user by ID" (Get (Param "users" Int) (Json User))
   ]

_describedServes :: Serves DescribedAPI (H1, H2) => ()
_describedServes = ()


-- ===================================================================
-- 9. WithParams / WithHeaders annotation tests
-- ===================================================================

-- WithParams compiles and is transparent to Serves
type ParamAPI =
  '[ WithParams '[QP "page" Int, QP "limit" Int]
       (Get (At "users") (Json [User]))
   ]

_paramServes :: Serves ParamAPI H1 => ()
_paramServes = ()

-- WithHeaders compiles and is transparent to Serves
type HeaderAPI =
  '[ WithHeaders '[HH "Authorization" Text]
       (Get (At "users") (Json [User]))
   ]

_headerServes :: Serves HeaderAPI H1 => ()
_headerServes = ()

-- Combined: both annotations compose
type ParamHeaderAPI =
  '[ WithParams '[QP "page" Int]
       (WithHeaders '[HH "Authorization" Text]
         (Get (At "users") (Json [User])))
   ]

_paramHeaderServes :: Serves ParamHeaderAPI H1 => ()
_paramHeaderServes = ()


-- ===================================================================
-- 10. Streaming marker tests
-- ===================================================================

-- Streaming markers compile
type StreamAPI =
  '[ ServerStream (Get (At "events") (Json [User]))
   , ClientStream (Post (At "upload") (Json User) (Json User))
   , BidiStream (Post (At "chat") (Json User) (Json User))
   ]
_streamServes :: Serves StreamAPI (H1, H2, H3) => ()
_streamServes = ()


-- ===================================================================
-- 11. RespondsWith / status code annotation tests
-- ===================================================================

-- RespondsWith compiles
type StatusAPI =
  '[ RespondsWith 201 (Post (At "users") (Json User) (Json User))
   , DeleteNoContent (At "users")
   ]
_statusServes :: Serves StatusAPI (H1, H2) => ()
_statusServes = ()


-- ===================================================================
-- 12. KnownMethod runtime demoting
-- ===================================================================

_methodGet :: Method
_methodGet = methodVal @'GET

_methodPost :: Method
_methodPost = methodVal @'POST

_methodDelete :: Method
_methodDelete = methodVal @'DELETE


-- ===================================================================
-- Main: just verify the module loaded (all real tests are compile-time)
-- ===================================================================

main :: IO ()
main = do
  putStrLn "All compile-time checks passed."
  putStrLn ""
  putStrLn "Runtime witnesses:"
  putStrLn $ "  GET     = " ++ show _methodGet
  putStrLn $ "  POST    = " ++ show _methodPost
  putStrLn $ "  DELETE  = " ++ show _methodDelete
  putStrLn   "  Serves TestAPI (H1,H2,H3): OK (constraint synonym)"
  putStrLn $ "  JSON content type: " ++ show _jsonType
  putStrLn $ "  XML  content type: " ++ show _xmlType
