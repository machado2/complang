{-# LANGUAGE OverloadedStrings #-}

module DB where

import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Pool (Pool, createPool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Models
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as BS

-- Define how to convert a row to a User
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

-- Create a connection pool to the PostgreSQL database
createConnPool :: IO (Pool Connection)
createConnPool = do
  password <- getEnv "PGPASSWORD"
  let connInfo = defaultConnectInfo
        { connectHost = "host.docker.internal"
        , connectPort = 5432
        , connectUser = "testuser"
        , connectPassword = password
        , connectDatabase = "complang"
        }
  createPool (connect connInfo) close 1 10 10

-- Create a new user
createUser :: Pool Connection -> UserInput -> IO (Maybe User)
createUser pool userInput = do
  results <- withResource pool $ \conn ->
    query conn 
      "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email" 
      (inputName userInput, inputEmail userInput)
  case results of
    [user] -> return $ Just user
    _      -> return Nothing

-- Get all users
getAllUsers :: Pool Connection -> IO [User]
getAllUsers pool = withResource pool $ \conn ->
  query_ conn "SELECT id, name, email FROM users ORDER BY id"

-- Get user by ID
getUserById :: Pool Connection -> Int -> IO (Maybe User)
getUserById pool userId = do
  results <- withResource pool $ \conn ->
    query conn "SELECT id, name, email FROM users WHERE id = ?" [userId]
  case results of
    [user] -> return $ Just user
    _      -> return Nothing

-- Update user
updateUser :: Pool Connection -> Int -> UserInput -> IO Bool
updateUser pool userId userInput = do
  n <- withResource pool $ \conn ->
    execute conn 
      "UPDATE users SET name = ?, email = ? WHERE id = ?" 
      (inputName userInput, inputEmail userInput, userId)
  return $ n > 0

-- Delete user
deleteUser :: Pool Connection -> Int -> IO Bool
deleteUser pool userId = do
  n <- withResource pool $ \conn ->
    execute conn "DELETE FROM users WHERE id = ?" [userId]
  return $ n > 0
