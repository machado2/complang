{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty (scotty, ActionM, param, json, jsonData, status)
import Database.PostgreSQL.Simple (connectPostgreSQL, query, query_, execute, Only(..))
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Data.Aeson (object, (.=), (.:), withObject, ToJSON, FromJSON)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Text (Text)
import System.Environment (lookupEnv)
import Network.HTTP.Types (status201, status204, status404)
import Data.Int (Int64)

-- Define the User type for output.
data User = User
  { userId    :: Int
  , userName  :: Text
  , userEmail :: Text
  } deriving (Show, Generic)

instance ToJSON User where
  toJSON (User uid uname uemail) =
    object [ "id"    .= uid
           , "name"  .= uname
           , "email" .= uemail
           ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

-- Define the NewUser type for input (create/update)
data NewUser = NewUser
  { newUserName  :: Text
  , newUserEmail :: Text
  } deriving (Show, Generic)

instance FromJSON NewUser where
  parseJSON = withObject "NewUser" $ \v ->
    NewUser <$> v .: "name" <*> v .: "email"

main :: IO ()
main = do
  -- Get password from PGPASSWORD env var.
  mPassword <- lookupEnv "PGPASSWORD"
  let password = maybe "" id mPassword
  let connString = "host=host.docker.internal port=5432 dbname=complang user=testuser password=" ++ password
  conn <- connectPostgreSQL (BS.pack connString)
  scotty 8080 $ do
    -- POST /users: Create a new user.
    post "/users" $ do
      newUser <- jsonData :: ActionM NewUser
      result <- liftIO $ query conn "INSERT INTO users (name, email) VALUES (?,?) RETURNING id"
                             (newUserName newUser, newUserEmail newUser) :: ActionM [Only Int]
      case result of
        [Only newId] -> do
          let user = User newId (newUserName newUser) (newUserEmail newUser)
          status status201
          json user
        _ -> do
          status status404
          json $ object ["error" .= ("Failed to create user" :: Text)]
    
    -- GET /users: Return all users.
    get "/users" $ do
      users <- liftIO $ query_ conn "SELECT id, name, email FROM users" :: ActionM [User]
      json users

    -- GET /users/:id: Return a single user.
    get "/users/:id" $ do
      uid <- param "id"
      results <- liftIO $ query conn "SELECT id, name, email FROM users WHERE id = ?" (Only uid) :: ActionM [User]
      case results of
        [] -> do
          status status404
          json $ object ["error" .= ("User not found" :: Text)]
        (user:_) -> json user

    -- PUT /users/:id: Update a user.
    put "/users/:id" $ do
      uid <- param "id"
      newUser <- jsonData :: ActionM NewUser
      res <- liftIO $ execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?"
                           (newUserName newUser, newUserEmail newUser, uid) :: ActionM Int64
      if res == 0
        then do
          status status404
          json $ object ["error" .= ("User not found" :: Text)]
        else do
          let user = User uid (newUserName newUser) (newUserEmail newUser)
          json user

    -- DELETE /users/:id: Delete a user.
    delete "/users/:id" $ do
      uid <- param "id"
      res <- liftIO $ execute conn "DELETE FROM users WHERE id = ?" (Only uid) :: ActionM Int64
      if res == 0
        then do
          status status404
          json $ object ["error" .= ("User not found" :: Text)]
        else status status204
