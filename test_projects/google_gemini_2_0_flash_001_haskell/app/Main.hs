
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status201, status400, status404,HeaderName, hContentType)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Database.PostgreSQL.Simple
import Database
import System.Environment (getEnv)
import Control.Monad.IO.Class

main :: IO ()
main = do
  pgPassword <- getEnv "PGPASSWORD"
  let connInfo = ConnectInfo {connectHost = "host.docker.internal", connectPort = 5432, connectUser = "testuser", connectPassword = pgPassword, connectDatabase = "complang"}
  conn <- connect connInfo
  putStrLn "Server listening on port 8080"
  run 8080 (app conn)

app :: Connection -> Application
app conn req respond = do
  let path = pathInfo req
  let method = requestMethod req
  case (method, path) of
    ("POST", ["users"]) -> createUser conn req respond
    ("GET", ["users"]) -> listUsers conn respond
    ("GET", ["users", userId]) -> getUser conn userId req respond
    ("PUT", ["users", userId]) -> updateUser conn userId req respond
    ("DELETE", ["users", userId]) -> deleteUser conn userId req respond
    _ -> respond $ responseLBS status404 [(hContentType, "text/plain")] "Not Found"

createUser :: Connection -> Request -> (Response -> IO ()) -> IO ()
createUser conn req respond = do
  body <- requestBody req
  case decode body of
    Just user -> do
      createdUser <- insertUser conn user
      case createdUser of
        Just user -> do
          let responseBody = encode user
          respond $ responseLBS status201 [(hContentType, "application/json")] responseBody
        Nothing -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Failed to create user"
    Nothing -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Invalid JSON"

listUsers :: Connection -> (Response -> IO ()) -> IO ()
listUsers conn respond = do
  users <- getAllUsers conn
  let responseBody = encode users
  respond $ responseLBS status200 [(hContentType, "application/json")] responseBody

getUser :: Connection -> T.Text -> Request -> (Response -> IO ()) -> IO ()
getUser conn userId req respond = do
  case readEither $ T.unpack userId of
    Right userIdInt -> do
      maybeUser <- getUserById conn userIdInt
      case maybeUser of
        Just user -> do
          let responseBody = encode user
          respond $ responseLBS status200 [(hContentType, "application/json")] responseBody
        Nothing -> respond $ responseLBS status404 [(hContentType, "text/plain")] "User not found"
    Left _ -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Invalid user ID"

updateUser :: Connection -> T.Text -> Request -> (Response -> IO ()) -> IO ()
updateUser conn userId req respond = do
  body <- requestBody req
  case readEither $ T.unpack userId of
    Right userIdInt -> do
      case decode body of
        Just user -> do
          updated <- updateUserById conn userIdInt user
          if updated
            then respond $ responseLBS status204 [] ""
            else respond $ responseLBS status404 [(hContentType, "text/plain")] "User not found"
        Nothing -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Invalid JSON"
    Left _ -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Invalid user ID"

deleteUser :: Connection -> T.Text -> Request -> (Response -> IO ()) -> IO ()
deleteUser conn userId req respond = do
  case readEither $ T.unpack userId of
    Right userIdInt -> do
      deleted <- deleteUserById conn userIdInt
      if deleted
        then respond $ responseLBS status204 [] ""
        else respond $ responseLBS status404 [(hContentType, "text/plain")] "User not found"
    Left _ -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Invalid user ID"
