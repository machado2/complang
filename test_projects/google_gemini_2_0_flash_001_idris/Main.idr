
module Main

import Elerea
import Elerea.Routing
import Elerea.JSON
import Elerea.HTTP
import Elerea.Form
import Data.Either
import Data.String
import Data.Integer
import System
import Network.Socket
import Database.PostgreSQL
import Data.Maybe

%language Elerea

-- User Definition
data User = User (id : Integer) (name : String) (email : String)

record UserInput where
  constructor MkUserInput
  name : String
  email : String

toUser : Integer -> UserInput -> User
toUser id (MkUserInput name email) = User id name email

fromValue : Value -> Either String UserInput
fromValue (Object obj) =
  case lookup "name" obj of
    Nothing => Left "Missing name field"
    Just (String name) =>
      case lookup "email" obj of
        Nothing => Left "Missing email field"
        Just (String email) => Right (MkUserInput name email)
        Just _ => Left "Invalid email field"
    Just _ => Left "Invalid name field"
FromValue _ = Left "Invalid UserInput format"


-- JSON instances
Show User where
  show (User id name email) =
    "{\\"id\\":" ++ show id ++ ", \\"name\\":\\"" ++ name ++ "\\", \\"email\\":\\"" ++ email ++ "\\"}";

-- Database Interaction
getPort : IO Integer
getPort = do env <- getEnv "PORT"
             case env of
               Nothing => pure 8080
               Just p => case parseInteger p of
                            Nothing => pure 8080
                            Just i => pure i

dbConnectionString : IO String
dbConnectionString = do
  pw <- getEnv "PGPASSWORD"
  case pw of
    Nothing => pure "host=host.docker.internal port=5432 dbname=complang user=testuser password=Saloon5-Moody-Observing"
    Just password => pure $ "host=host.docker.internal port=5432 dbname=complang user=testuser password=" ++ password

-- | Execute a database query.  We use the bracket pattern to ensure
--   that the connection is closed even in case of errors.
withConnection : (Connection -> IO a) -> IO a
withConnection action = do
  connStr <- dbConnectionString
  bracket (connectPostgreSQL connStr) close action

createUser : String -> String -> IO String  -- name, email
createUser name email = withConnection $ \conn => do
  result <- query conn ("INSERT INTO users (name, email) VALUES ('" ++ name ++ "', '" ++ email ++ "') RETURNING id, name, email;") []
  case result of
    Left err => pure $ "Error: " ++ show err
    Right rows =>
      case flattenRows rows of
        [ [ PGInteger id, PGText name, PGText email ] ] =>
          pure $ "{\"id\":" ++ show id ++ ", \"name\":\"" ++ name ++ "\", \"email\":\"" ++ email ++ "\"}";
        _ => pure "Error: Unexpected result from database"

getUsers : IO String
getUsers = withConnection $ \conn => do
  result <- query conn "SELECT id, name, email FROM users;" []
  case result of
    Left err => pure $ "Error: " ++ show err
    Right rows =>
      let
        users = map (\row =>
          case flattenRows [row] of
            [ PGInteger id, PGText name, PGText email ] =>
              "{\"id\":" ++ show id ++ ", \"name\":\"" ++ name ++ "\", \"email\":\"" ++ email ++ "\"}";
            _ => "{}"
          ) rows
      in pure $ "[" ++ concatWith ", " users ++ "]"

getUser : String -> IO String -- id
getUser userId = withConnection $ \conn => do
  result <- query conn ("SELECT id, name, email FROM users WHERE id = " ++ userId ++ ";") []
  case result of
    Left err => pure $ "Error: " ++ show err
    Right rows =>
      case flattenRows =<< rows of
        [ PGInteger id, PGText name, PGText email ] =>
          pure $ "{\"id\":" ++ show id ++ ", \"name\":\"" ++ name ++ "\", \"email\":\"" ++ email ++ "\"}";
        _ => pure $ response "Not Found" 404 --"Error: User not found"
updateUser : String -> String -> String -> IO String  -- id, name, email
updateUser userId name email = withConnection $ \conn => do
  result <- query conn ("UPDATE users SET name = '" ++ name ++ "', email = '" ++ email ++ "' WHERE id = " ++ userId ++ ";") []
  case result of
    Left err => pure $ "Error: " ++ show err
    Right _ => pure "" -- Successfully updated

deleteUser : String -> IO String -- id
deleteUser userId = withConnection $ \conn => do
  result <- query conn ("DELETE FROM users WHERE id = " ++ userId ++ ";") []
  case result of
    Left err => pure $ "Error: " ++ show err
    Right _ => pure "" -- Successfully deleted

concatWith : String -> List String -> String
concatWith sep [] = ""
concatWith sep [x] = x
concatWith sep (x :: xs) = x ++ sep ++ concatWith sep xs

response : String -> Integer -> String
response body status = "HTTP/1.1 " ++ show status ++ " OK\nContent-Type: application/json\n\n" ++ body

-- Routing Table
routes : List Route
routes =
  [ Route GET "/users" (eq => do res <- getUsers; pure (response res 200))
  , Route POST "/users" (eq => do
      case req ^. requestBody of
        Nothing => pure (response "Missing body" 400)
        Just body => case fromValue body of
          Left err => pure (response err 400)
          Right userInput => do
            res <- createUser (userInput.name) (userInput.email)
            pure (response res 201)
    )
  , Route GET "/users/:id" (eq => do
      let userId = param "id" req
      res <- getUser userId
      if res == response "Not Found" 404 then
          pure (response "User not found" 404)
      else
          pure (response res 200))
  , Route PUT "/users/:id" (eq => do
      let userId = param "id" req
      case req ^. requestBody of
        Nothing => pure (response "Missing body" 400)
        Just body => case fromValue body of
          Left err => pure (response err 400)
          Right userInput => do
            res <- updateUser userId (userInput.name) (userInput.email)
            pure (response "" 204)
    )
  , Route DELETE "/users/:id" (eq => do
      let userId = param "id" req
      res <- deleteUser userId;
      pure (response "" 204))
  ]


main : IO ()
main = do
  port <- getPort
  putStrLn $ "Starting server on port " ++ show port
  startServer routes port
