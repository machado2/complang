{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Aeson
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)

data User = User { id :: Int, name :: String, email :: String } deriving (Show, Generic)

instance ToJSON User where
    toJSON (User id name email) = object ["id" .= id, "name" .= name, "email" .= email]

main :: IO ()
main = do
    connStr <- getEnv "DATABASE_URL"
    conn <- connectPostgreSQL connStr
    scotty 8080 $ do
      get "/users" $ do
        users <- liftIO $ query_ conn "SELECT id, name, email FROM users" :: ActionM [User]
        json users
      post "/users" $ do
        UserInput name email <- jsonData :: ActionM UserInput
        liftIO $ do
            [Only userId] <- query conn "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id" (name, email)
            json $ User userId name email
      -- Add remaining endpoints here.