{-# LANGUAGE OverloadedStrings #-}
module Users where

import Database.PostgreSQL.Simple
import Data.Aeson
import Data.Text

-- User data type
data User = User { id :: Int, name :: String, email :: String }
  deriving (Show, Eq)

-- Converts User to JSON
instance ToJSON User where
  toJSON (User id name email) = object ["id" .= id, "name" .= name, "email" .= email]

-- Function to create a new user
createUser :: Connection -> String -> String -> IO User
createUser conn name email = do
  [Only id] <- query conn "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id" (name, email)
  return $ User id name email

-- Function to get all users
getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT id, name, email FROM users"