
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status201, status204, status404)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as PG
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:), (.=), encode)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Web.Scotty

data User = User
  { userId    :: Int
  , userName  :: T.Text
  , userEmail :: T.Text
  } deriving (Show)

instance ToJSON User where
  toJSON (User id name email) =
    object ["id" .= id, "name" .= name, "email" .= email]

instance FromJSON User where
  parseJSON = withObject "User" $  ->
    User <$> v .: "id" <*> v .: "name" <*> v .: "email"

main :: IO ()
main = do
  conn <- PG.connect PG.defaultConnectInfo {
      PG.connectHost = "host.docker.internal",
      PG.connectPort = 5432,
      PG.connectDatabase = "complang",
      PG.connectUser = "testuser",
      PG.connectPassword = TE.decodeUtf8 <$> lookupEnv "PGPASSWORD"
    }
  
  putStrLn "Server started on port 8080"
  scotty 8080 $ do
    -- Create user
    post "/users" $ do
      user <- jsonData :: ActionM User
      liftIO $ PG.execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (userName user, userEmail user)
      status status201
      json user
      
    -- Retrieve all users
    get "/users" $ do
      users <- liftIO $ PG.query_ conn "SELECT id, name, email FROM users"
      json users
      
    -- Retrieve a user by ID
    get "/users/:id" $ do
      id <- param "id"
      user <- liftIO $ PG.query conn "SELECT id, name, email FROM users WHERE id = ?" (Only id)
      case user of
        [] -> status status404 >> json ("User not found" :: T.Text)
        (u:_) -> json u
      
    -- Update a user
    put "/users/:id" $ do
      id <- param "id"
      user <- jsonData :: ActionM User
      rowsAffected <- liftIO $ PG.execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (userName user, userEmail user, id)
      if rowsAffected == 0
        then status status404 >> json ("User not found" :: T.Text)
        else status status204
    
    -- Delete a user
    delete "/users/:id" $ do
      id <- param "id"
      rowsAffected <- liftIO $ PG.execute conn "DELETE FROM users WHERE id = ?" (Only id)
      if rowsAffected == 0
        then status status404 >> json ("User not found" :: T.Text)
        else status status204
