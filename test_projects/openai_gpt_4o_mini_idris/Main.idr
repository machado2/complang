module Main

import Network.HTTP
import Network.HTTP.Server
import Data.String
import Data.List
import Database.PostgreSQL.Simple
import Control.Exception
import System.Environment

record User where
  constructor MkUser
  id: Int
  name: String
  email: String

instance Show User where
  show (MkUser id name email) = concat ["{ "id": ", show id, ", "name": "", name, "", "email": "", email, "" }"]

handleRequest : Request -> (Response, IO ())
handleRequest req = case requestMethod req of
  "POST" => handleCreateUser req
  "GET" => handleGetUsers req
  _ => (response 405 [], return ())

handleCreateUser : Request -> (Response, IO ())
handleCreateUser req = do
  body <- getRequestBody req
  let user = decodeUser body
  -- Insert user into the database
  -- Return created user with id
  let newUser = MkUser 1 (user.name) (user.email) -- This would be replaced with actual DB call
  let responseBody = show newUser
  return (response 201 [("Content-Type", "application/json")] responseBody, return ())

handleGetUsers : Request -> (Response, IO ())
handleGetUsers req = do
  users <- fetchUsersFromDB
  let responseBody = "[" ++ intercalate "," (map show users) ++ "]"
  return (response 200 [("Content-Type", "application/json")] responseBody, return ())

fetchUsersFromDB : IO [User]
fetchUsersFromDB = do
  conn <- connect defaultConnectInfo { connectDatabase = "complang"
                                      , connectUser = "testuser"
                                      , connectPassword = System.getEnv "PGPASSWORD"
                                      , connectHost = "host.docker.internal"
                                      , connectPort = 5432 }
  -- Fetch users, and return as list of User records
  return []

main : IO ()
main = serve (Port 8080) handleRequest
