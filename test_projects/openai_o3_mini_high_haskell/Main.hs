{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON, genericToJSON, genericParseJSON, defaultOptions, fieldLabelModifier)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status201, status204, status404)
import System.Environment (getEnv)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as TL

-- | User record for output with custom JSON field names.
data User = User
  { userId    :: Int
  , userName  :: String
  , userEmail :: String
  } deriving (Show, Generic)

instance ToJSON User where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \f ->
      case f of
        "userId"    -> "id"
        "userName"  -> "name"
        "userEmail" -> "email"
        _           -> f
    }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

-- | NewUser for incoming JSON data for POST/PUT requests.
data NewUser = NewUser
  { newName  :: String
  , newEmail :: String
  } deriving (Show, Generic)

instance FromJSON NewUser where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \f ->
      case f of
        "newName"  -> "name"
        "newEmail" -> "email"
        _          -> f
    }

main :: IO ()
main = do
  pwd <- getEnv "PGPASSWORD"
  let connInfo = defaultConnectInfo
                   { connectHost     = "host.docker.internal",
                     connectPort     = 5432,
                     connectUser     = "testuser",
                     connectPassword = pwd,
                     connectDatabase = "complang"
                   }
  conn <- connect connInfo
  scotty 8080 $ do
    -- POST /users: Create a new user.
    post "/users" $ do
      newUser <- jsonData :: ActionM NewUser
      res <- liftIO $ query conn
               "INSERT INTO users (name, email) VALUES (?,?) RETURNING id"
               (newName newUser, newEmail newUser) :: ActionM [Only Int]
      case res of
        [Only uid] -> do
          status status201
          json $ User uid (newName newUser) (newEmail newUser)
        _ -> status status404

    -- GET /users: List all users.
    get "/users" $ do
      users <- liftIO $ query_ conn "SELECT id, name, email FROM users" :: ActionM [User]
      json users

    -- GET /users/:id: Retrieve a single user.
    get "/users/:id" $ do
      uid <- param "id"
      users <- liftIO $ query conn
                "SELECT id, name, email FROM users WHERE id = ?"
                (Only uid) :: ActionM [User]
      case users of
        [] -> status status404
        (user:_) -> json user

    -- PUT /users/:id: Update an existing user.
    put "/users/:id" $ do
      uid <- param "id"
      newUser <- jsonData :: ActionM NewUser
      updated <- liftIO $ execute conn
                 "UPDATE users SET name = ?, email = ? WHERE id = ?"
                 (newName newUser, newEmail newUser, uid)
      if updated == 0 then status status404 else status status204

    -- DELETE /users/:id: Delete a user.
    delete "/users/:id" $ do
      uid <- param "id"
      removed <- liftIO $ execute conn
                 "DELETE FROM users WHERE id = ?"
                 (Only uid)
      if removed == 0 then status status404 else status status204