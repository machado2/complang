
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad.IO.Class

data User = User
  { userId :: Int,
    userName :: T.Text,
    userEmail :: T.Text
  } deriving (Show, Eq)

instance ToJSON User where
  toJSON (User id name email) =
    object ["id" .= id, "name" .= name, "email" .= email]

instance FromJSON User where
  parseJSON (Object v) =
    User
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "email"
  parseJSON _ = mzero

data NewUser = NewUser
  { newUserName :: T.Text,
    newUserEmail :: T.Text
  } deriving (Show)

instance FromJSON NewUser where
  parseJSON (Object v) =
    NewUser
      <$> v .: "name"
      <*> v .: "email"
  parseJSON _ = mzero

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

insertUser :: Connection -> NewUser -> IO (Maybe User)
insertUser conn (NewUser name email) = do
  result <- query conn "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email" (name, email)
  case result of
    [user] -> return $ Just user
    _ -> return Nothing

getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT id, name, email FROM users"

getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn userId = do
  result <- query conn "SELECT id, name, email FROM users WHERE id = ?" (Only userId)
  case result of
    [user] -> return $ Just user
    _ -> return Nothing

updateUserById :: Connection -> Int -> NewUser -> IO Bool
updateUserById conn userId (NewUser name email) = do
  rowsAffected <- execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (name, email, userId)
  return (rowsAffected > 0)

deleteUserById :: Connection -> Int -> IO Bool
deleteUserById conn userId = do
  rowsAffected <- execute conn "DELETE FROM users WHERE id = ?" (Only userId)
  return (rowsAffected > 0)
