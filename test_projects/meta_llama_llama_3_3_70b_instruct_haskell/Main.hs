
module Main where

import Web.Scotty
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics

data User = User { userId :: Int, name :: String, email :: String } deriving (Generic, Show, ToJSON, FromJSON)

main :: IO ()
main = scotty 8080 $ do
  post "/users" $ do
    user <- jsonData `rescue` (\_ -> return "{"error": "Invalid JSON"}" :: Text)
    conn <- liftIO $ connectPostgreSQL "host:host.docker.internal port=5432 user=testuser password=" ++ envPGPASSWORD ++ " dbname=complang"
    userId <- liftIO $ withResource conn $ \conn -> do
      [only] <- query conn "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id" (name user, email user)
      return $ only
    json $ User userId (name user) (email user)

  get "/users" $ do
    conn <- liftIO $ connectPostgreSQL "host:host.docker.internal port=5432 user=testuser password=" ++ envPGPASSWORD ++ " dbname=complang"
    users <- liftIO $ withResource conn $ query_ conn "SELECT * FROM users"
    json users

  get "/users/:id" $ do
    id <- param "id"
    conn <- liftIO $ connectPostgreSQL "host:host.docker.internal port=5432 user=testuser password=" ++ envPGPASSWORD ++ " dbname=complang"
    user <- liftIO $ withResource conn $ query conn "SELECT * FROM users WHERE id = ?" (Only id)
    case user of
      [user] -> json user
      _ -> status 404 >> json "{"error": "User not found"}"

  put "/users/:id" $ do
    id <- param "id"
    user <- jsonData `rescue` (\_ -> return "{"error": "Invalid JSON"}" :: Text)
    conn <- liftIO $ connectPostgreSQL "host:host.docker.internal port=5432 user=testuser password=" ++ envPGPASSWORD ++ " dbname=complang"
    liftIO $ withResource conn $ execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (name user, email user, id)
    status 200

  delete "/users/:id" $ do
    id <- param "id"
    conn <- liftIO $ connectPostgreSQL "host:host.docker.internal port=5432 user=testuser password=" ++ envPGPASSWORD ++ " dbname=complang"
    liftIO $ withResource conn $ execute conn "DELETE FROM users WHERE id = ?" (Only id)
    status 200

envPGPASSWORD = System Environmental variable PGPASSWORD 
