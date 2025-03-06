{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import Data.Aeson.Types (Parser, parseJSON, withObject, (.:))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Database.PostgreSQL.Simple
import System.Environment (getEnv)


-- Data type representing a user
data User = User { userId :: Int, userName :: String, userEmail :: String } deriving (Show, Eq)

instance ToJSON User where
  toJSON (User id name email) = object [ "id" .= id, "name" .= name, "email" .= email ]

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "email"

-- Function to establish database connection
connectDB :: IO Connection
connectDB = do
    pgPass <- getEnv "PGPASSWORD"
    connectPostgreSQL $ T.encodeUtf8 $ T.pack $ "host=host.docker.internal port=5432 dbname=complang user=testuser password=" ++ pgPass

-- Handler for creating a new user
createUser :: Connection -> ActionM ()
createUser conn = do
    body <- bodyT :: ActionM Text
    case decode (T.encodeUtf8 body) of
        Nothing -> raiseStatus status400 $ T.pack "Invalid JSON"
        Just user -> do
            let name = userName user
            let email = userEmail user
            result <- liftIO $ execute conn "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id" (name, email)
            [Only id] <- liftIO $ query_ conn "SELECT last_insert_id()"

            setHeader "Content-Type" "application/json"

            let newUser = User id name email
            json newUser


                 -- status created201
            status status201



-- Handler for getting all users
getUsers :: Connection -> ActionM ()
getUsers conn = do
    users <- liftIO $ query_ conn "SELECT id, name, email FROM users" :: IO [User]
    setHeader "Content-Type" "application/json"
    json users

-- Handler for getting a single user by ID
getUser :: Connection -> ActionM ()
getUser conn = do
    userId <- param "id"
    maybeUser <- liftIO $ query conn "SELECT id, name, email FROM users WHERE id = ?" (Only userId) :: IO [User]
    case maybeUser of
        [user] -> do
            setHeader "Content-Type" "application/json"
            json user
        [] -> raiseStatus status404 $ T.pack "User not found"

-- Handler for updating a user
updateUser :: Connection -> ActionM ()
updateUser conn = do
    userId <- param "id"
    body <- bodyT :: ActionM Text
    case decode (T.encodeUtf8 body) of
        Nothing -> raiseStatus status400 $ T.pack "Invalid JSON"
        Just user -> do
            let name = userName user
            let email = userEmail user
            rowsAffected <- liftIO $ execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (name, email, userId)
            if rowsAffected == 0
                then raiseStatus status404 $ T.pack "User not found"
                else status status204

-- Handler for deleting a user
deleteUser :: Connection -> ActionM ()
deleteUser conn = do
    userId <- param "id"
    rowsAffected <- liftIO $ execute conn "DELETE FROM users WHERE id = ?" (Only userId)
    if rowsAffected == 0
        then raiseStatus status404 $ T.pack "User not found"
        else status status204

main :: IO ()
main = do
    conn <- connectDB
    scotty 8080 $ do
        post "/users" $ createUser conn
        get "/users" $ getUsers conn
        get "/users/:id" $ getUser conn
        put "/users/:id" $ updateUser conn
        delete "/users/:id" $ deleteUser conn