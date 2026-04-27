{-# LANGUAGE DeriveAnyClass #-}
-- | CRUD API with structured error handling, named routes, and type annotations.
--
-- Run:  cabal run crud
-- Test: curl http://localhost:3000/items
--       curl -X POST http://localhost:3000/items -d '{"name":"Widget","price":9.99}'
--       curl http://localhost:3000/items/1
--       curl -X PUT http://localhost:3000/items/1 -d '{"name":"Gadget","price":19.99}'
--       curl -X DELETE http://localhost:3000/items/1
module Main (main) where

import Data.Aeson (FromJSON, ToJSON, (.=), object, (.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import Acolyte.Prelude


-- ===================================================================
-- Domain types
-- ===================================================================

data Item = Item { itemId :: Int, itemName :: Text, itemPrice :: Double }
  deriving (Show)

instance ToJSON Item where
  toJSON i = object ["id" .= itemId i, "name" .= itemName i, "price" .= itemPrice i]

data CreateItem = CreateItem { ciName :: Text, ciPrice :: Double }
  deriving (Show)

instance FromJSON CreateItem where
  parseJSON = Aeson.withObject "CreateItem" $ \o ->
    CreateItem <$> o .: "name" <*> o .: "price"


-- ===================================================================
-- Validation
-- ===================================================================

-- | Validator for CreateItem: name must not be empty, price must be positive.
data CreateItemValidator

instance Validate CreateItemValidator CreateItem where
  validate ci
    | T.null (ciName ci)  = Left "name is required"
    | ciPrice ci <= 0     = Left "price must be positive"
    | otherwise           = Right ci


-- ===================================================================
-- Structured error type with IntoResponse instance
-- ===================================================================

data AppError
  = NotFound Text
  | BadRequest Text
  | Unprocessable Text
  deriving (Show)

appErrorStatus :: AppError -> Status
appErrorStatus (NotFound _)      = status404
appErrorStatus (BadRequest _)    = status400
appErrorStatus (Unprocessable _) = status422

appErrorMessage :: AppError -> Text
appErrorMessage (NotFound m)      = m
appErrorMessage (BadRequest m)    = m
appErrorMessage (Unprocessable m) = m

instance IntoResponse AppError where
  intoResponse err =
    let s = appErrorStatus err
        body = LBS.toStrict $ Aeson.encode $ object
          [ "error" .= object
            [ "status"  .= statusCode s
            , "message" .= appErrorMessage err
            ]
          ]
    in Response s [("Content-Type", "application/json")] body


-- ===================================================================
-- API type (with Describe and WithParams annotations)
-- ===================================================================

type ItemsPath  = At "items"
type ItemPath   = Param "items" Int

type CrudAPI =
  '[ Named "listItems"  (Describe "List all items" (WithParams '[QP "limit" Int] (Get ItemsPath (Json [Item]))))
   , Named "createItem" (Describe "Create a new item" (Post ItemsPath (Json CreateItem) (Json Item)))
   , Named "getItem"    (Describe "Get item by ID" (Get ItemPath (Json Item)))
   , Named "updateItem" (Describe "Update an item" (Put ItemPath (Json CreateItem) (Json Item)))
   , Named "deleteItem" (Describe "Delete an item" (Delete ItemPath (Json Item)))
   ]


-- ===================================================================
-- Handlers
-- ===================================================================

type Store = IORef (Int, [Item])  -- (nextId, items)

listItemsH :: Store -> IO (Json [Item])
listItemsH ref = do
  (_, items) <- readIORef ref
  pure (Json items)

createItemH :: Store -> ValidatedBody CreateItemValidator CreateItem -> IO (Json Item)
createItemH ref (ValidatedBody (CreateItem name price)) = do
  (nextId, items) <- readIORef ref
  let item = Item nextId name price
  writeIORef ref (nextId + 1, items ++ [item])
  pure (Json item)

getItemH :: Store -> PathCapture Int -> IO (Either AppError (Json Item))
getItemH ref (PathCapture iid) = do
  (_, items) <- readIORef ref
  case filter (\i -> itemId i == iid) items of
    (x:_) -> pure (Right (Json x))
    []    -> pure (Left (NotFound (T.pack ("Item " ++ show iid ++ " not found"))))

updateItemH :: Store -> PathCapture Int -> JsonBody CreateItem -> IO (Either AppError (Json Item))
updateItemH ref (PathCapture iid) (JsonBody (CreateItem name price)) = do
  (nid, items) <- readIORef ref
  case span (\i -> itemId i /= iid) items of
    (_, [])     -> pure (Left (NotFound (T.pack ("Item " ++ show iid ++ " not found"))))
    (before, _old:after) -> do
      let updated = Item iid name price
      writeIORef ref (nid, before ++ [updated] ++ after)
      pure (Right (Json updated))

deleteItemH :: Store -> PathCapture Int -> IO (Either AppError (Json Item))
deleteItemH ref (PathCapture iid) = do
  (nid, items) <- readIORef ref
  case span (\i -> itemId i /= iid) items of
    (_, [])      -> pure (Left (NotFound (T.pack ("Item " ++ show iid ++ " not found"))))
    (before, old:after) -> do
      writeIORef ref (nid, before ++ after)
      pure (Right (Json old))


-- ===================================================================
-- Named routes record
-- ===================================================================

-- | Handler record for CrudAPI — field names match Named endpoint labels.
data CrudHandlers = CrudHandlers
  { listItems  :: IO (Json [Item])
  , createItem :: ValidatedBody CreateItemValidator CreateItem -> IO (Json Item)
  , getItem    :: PathCapture Int -> IO (Either AppError (Json Item))
  , updateItem :: PathCapture Int -> JsonBody CreateItem -> IO (Either AppError (Json Item))
  , deleteItem :: PathCapture Int -> IO (Either AppError (Json Item))
  }


-- ===================================================================
-- Client record (for reference)
-- ===================================================================

-- mkClientRecord derives a typed client record from the Named API:
--
-- @
-- import Acolyte.Client (mkClientRecord, ClientConfig)
--
-- data CrudClient = CrudClient
--   { listItems  :: IO (Json [Item])
--   , createItem :: Json CreateItem -> IO (Json Item)
--   , getItem    :: Int -> IO (Json Item)
--   , updateItem :: Int -> Json CreateItem -> IO (Json Item)
--   , deleteItem :: Int -> IO (Json Item)
--   }
--
-- mkCrudClient :: ClientConfig -> CrudClient
-- mkCrudClient = mkClientRecord @CrudAPI
-- @


-- ===================================================================
-- Server + main
-- ===================================================================

-- Alternative: positional wiring (Named is transparent to tuples)
-- server store = mkApi @CrudAPI
--   ( listItemsH store
--   , createItemH store
--   , getItemH store
--   , updateItemH store
--   , deleteItemH store
--   )

server :: Store -> Service IO (Request ByteString) (Response ByteString)
server store = mkRecordApi @CrudAPI CrudHandlers
  { listItems  = listItemsH store
  , createItem = createItemH store
  , getItem    = getItemH store
  , updateItem = updateItemH store
  , deleteItem = deleteItemH store
  }

main :: IO ()
main = do
  store <- newIORef (1, [])
  putStrLn "CRUD example on http://localhost:3000"
  putStrLn "  GET    /items       - list all"
  putStrLn "  POST   /items       - create"
  putStrLn "  GET    /items/:id   - get by id"
  putStrLn "  PUT    /items/:id   - update"
  putStrLn "  DELETE /items/:id   - delete"
  runServerBS 3000 (server store)
