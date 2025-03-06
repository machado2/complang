{-# LANGUAGE OverloadedStrings #-}
module Routes where

import Web.Scotty
import Database.PostgreSQL.Simple
import Users -- importing User functions

-- Function to define routes
defineRoutes :: Connection -> ScottyM ()
defineRoutes conn = do
  get "/users" $ do
    users <- liftIO $ getAllUsers conn
    json users

  post "/users" $ do
    UserInput name email <- jsonData :: ActionM UserInput
    user <- liftIO $ createUser conn name email
    json user

  -- Add more routes for GET by id, PUT, DELETE etc. here.