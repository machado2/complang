module Main

import Data.String
import Data.List
import Data.Maybe
import System
import System.Environment
import TCP
import Network.Socket
import Palmer.HTTP
import Palmer.HTTP.Types
import Palmer.HTTP.Router
import Palmer.HTTP.Server
import Palmer.HTTP.Response
import Palmer.JSON
import Palmer.JSON.Decode
import Palmer.JSON.Encode
import Database.PostgreSQL

%default total

-- Data Models
record User where
  constructor MkUser
  id    : Int
  name  : String
  email : String

-- JSON Instances
JSONValue User where
  toJSON (MkUser id name email) =
    JObject [ ("id",    toJSON id)
            , ("name",  toJSON name)
            , ("email", toJSON email) ]

-- Decode the JSON
fromJSON : JSONValue -> Maybe User
fromJSON (JObject obj) = do
  id    <- fromJSON =<< lookup "id"    obj
  name  <- fromJSON =<< lookup "name"  obj
  email <- fromJSON =<< lookup "email" obj
  pure $ MkUser id name email
fromJSON _ = Nothing

-- Database Configuration
dbHost     : String
dbHost     = "host.docker.internal"

dbPort     : String
dbPort     = "5432"

dbName     : String
dbName     = "complang"

dbUser     : String
dbUser     = "testuser"

-- NOTE: password needs to be set in the env variable PGPASSWORD
-- dbPassword : String
-- dbPassword = "Saloon5-Moody-Observing"

-- Helper function to get the password from the environment
getDbPassword : IO String
getDbPassword = getEnv "PGPASSWORD" `catch` \(me : IOError) => pure ""

-- Database connection string
getConnectionString : String -> String
getConnectionString password =
  "host=" ++ dbHost ++ " port=" ++ dbPort ++ " dbname=" ++ dbName ++ " user=" ++ dbUser ++ " password=" ++ password

-- Handler to get all users
getAllUsers : IO Response
getAllUsers = do
  password <- getDbPassword
  conn <- connectDB (getConnectionString password)
  case conn of
       Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
       Right c => do
         result <- query c "SELECT id, name, email FROM users" []
         close c
         case result of
              Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
              Right qr =>
                let
                  users : List User
                  users = mapMaybe (\row =>
                                      case row of
                                           [PGInt id, PGText name, PGText email] =>
                                             Just $ MkUser (cast id) (cast name) (cast email)
                                           _ => Nothing
                                  ) (qrRows qr)
                in
                  pure $ jsonResponse 200 $ toJSON users

-- Handler to create a new user
createUser : Request -> IO Response
createUser req = do
  password <- getDbPassword
  conn <- connectDB (getConnectionString password)
  case conn of
       Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
       Right c => do
          body <- getBody req
          case decodeJSON body of
               Nothing => pure $ textResponse 400 "Invalid JSON"
               Just json =>
                 case fromJSON json of
                      Nothing => pure $ textResponse 400 "Invalid User data"
                      Just (MkUser _ name email) => do
                        result <- query c "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email" [PGText name, PGText email]
                        close c
                        case result of
                             Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
                             Right qr =>
                                case qrRows qr of
                                     [[PGInt id, PGText name', PGText email']] =>
                                         let newUser = MkUser (cast id) (cast name') (cast email') in
                                         pure $ jsonResponse 201 $ toJSON newUser
                                     _ => pure $ textResponse 500 "Failed to create user"

-- Handler to get a user by ID
getUserById : String -> IO Response
getUserById userId = do
  password <- getDbPassword
  conn <- connectDB (getConnectionString password)
  case conn of
       Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
       Right c => do
         result <- query c "SELECT id, name, email FROM users WHERE id = $1" [PGInt $ cast $ parseInteger userId]
         close c
         case result of
              Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
              Right qr =>
                case qrRows qr of
                     [] => pure $ textResponse 404 "User not found"
                     [[PGInt id, PGText name, PGText email]] =>
                         let user = MkUser (cast id) (cast name) (cast email) in
                         pure $ jsonResponse 200 $ toJSON user
                     _ => pure $ textResponse 500 "Internal Server Error"

-- Handler to update a user
updateUser : String -> Request -> IO Response
updateUser userId req = do
  password <- getDbPassword
  conn <- connectDB (getConnectionString password)
  case conn of
       Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
       Right c => do
          body <- getBody req
          case decodeJSON body of
               Nothing => pure $ textResponse 400 "Invalid JSON"
               Just json =>
                 case fromJSON json of
                      Nothing => pure $ textResponse 400 "Invalid User data"
                      Just (MkUser _ name email) => do
                        result <- query c "UPDATE users SET name = $1, email = $2 WHERE id = $3" [PGText name, PGText email, PGInt $ cast $ parseInteger userId]
                        close c
                        case result of
                             Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
                             Right qr =>
                               if affectedRows qr > 0
                               then pure $ textResponse 204 ""
                               else pure $ textResponse 404 "User not found"

-- Handler to delete a user
deleteUser : String -> IO Response
deleteUser userId = do
  password <- getDbPassword
  conn <- connectDB (getConnectionString password)
  case conn of
       Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
       Right c => do
         result <- query c "DELETE FROM users WHERE id = $1" [PGInt $ cast $ parseInteger userId]
         close c
         case result of
              Left err => pure $ jsonResponse 500 $ JObject [("error", JString $ pack $ show err)]
              Right qr =>
                if affectedRows qr > 0
                then pure $ textResponse 204 ""
                else pure $ textResponse 404 "User not found"

-- Routing
router : Router IO
router = createRouter
  [ Route GET "/users" getAllUsers
  , Route POST "/users" createUser
  , Route GET "/users/:id" (\req, params => maybe (pure $ textResponse 400 "Invalid ID") getUserById (lookup "id" params))
  , Route PUT "/users/:id" (\req, params => maybe (pure $ textResponse 400 "Invalid ID") (\id => updateUser id req) (lookup "id" params))
  , Route DELETE "/users/:id" (\req, params => maybe (pure $ textResponse 400 "Invalid ID") deleteUser (lookup "id" params))
  ]

-- Main function
main : IO ()
main = do
  putStrLn "Starting server on port 8080..."
  startServer router 8080

