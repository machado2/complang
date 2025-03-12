module Main

%default total

import Data.List
import Data.Char
import System.Environment

-- A dummy DB connection type.
record Connection where
  constructor MkConnection

-- User record definition.
record User where
  constructor MkUser
  id    : Int
  name  : String
  email : String

-- Convert a User record to a JSON string.
userToJson : User -> String
userToJson (MkUser uid name email) =
  "{"id": " ++ show uid ++ ", "name": "" ++ name ++ "", "email": "" ++ email ++ ""}"

-- Convert a list of Users to a JSON array.
usersToJson : List User -> String
usersToJson users = "[" ++ intercalate ", " (map userToJson users) ++ "]"

-- Simulate connecting to the PostgreSQL DB.
connectDB : String -> IO Connection
connectDB connStr = do
  putStrLn ("Connecting to DB with: " ++ connStr)
  pure (MkConnection)

-- Simulate inserting a new user into the database.
createUserDB : Connection -> String -> String -> IO User
createUserDB conn name email = do
  -- In a real implementation, execute SQL INSERT returning the new id.
  let newId = 1
  putStrLn ("Inserting user: " ++ name ++ ", " ++ email)
  pure (MkUser newId name email)

-- Simulate fetching all users from the database.
getUsersDB : Connection -> IO (List User)
getUsersDB conn = do
  putStrLn "Fetching all users"
  pure [MkUser 1 "Alice" "alice@example.com", MkUser 2 "Bob" "bob@example.com"]

-- Simulate fetching a user by id.
getUserByIdDB : Connection -> Int -> IO (Maybe User)
getUserByIdDB conn uid = do
  putStrLn ("Fetching user with id: " ++ show uid)
  if uid == 1 then pure (Just (MkUser 1 "Alice" "alice@example.com"))
  else pure Nothing

-- Simulate updating a user.
updateUserDB : Connection -> Int -> String -> String -> IO Bool
updateUserDB conn uid name email = do
  putStrLn ("Updating user " ++ show uid ++ " to: " ++ name ++ ", " ++ email)
  if uid == 1 then pure True else pure False

-- Simulate deleting a user.
deleteUserDB : Connection -> Int -> IO Bool
deleteUserDB conn uid = do
  putStrLn ("Deleting user with id: " ++ show uid)
  if uid == 1 then pure True else pure False

-- Dummy request/response data types.
record Request where
  constructor MkRequest
  method : String
  path   : String
  body   : String

record Response where
  constructor MkResponse
  statusCode : Int
  body       : String

-- A very simple parser for a JSON with the exact expected format:
-- {"name": "Some Name", "email": "someone@example.com"}
parseUserJson : String -> Either String (String, String)
parseUserJson json =
  case stripPrefix "{"name": "" json of
    Just rest =>
         let (name, rest2) = break (== '"') rest in
         case stripPrefix "", "email": "" rest2 of
           Just rest3 =>
                let (email, rest4) = break (== '"') rest3 in
                case stripPrefix ""}" rest4 of
                  Just _  => Right (name, email)
                  Nothing => Left "Invalid JSON: missing end token"
           Nothing => Left "Invalid JSON: email field not found"
    Nothing => Left "Invalid JSON: name field not found"

-- Helper to convert a string to an Int (only works for non-negative integers).
castInt : String -> Maybe Int
castInt s =
  if all isDigit s then
     Just (foldl (\acc c => acc * 10 + digitToInt c) 0 s)
  else Nothing

-- A dummy HTTP server that listens on a port and calls a handler.
listenAndServe : Int -> (Request -> IO Response) -> IO ()
listenAndServe port handler = do
  putStrLn ("Server is listening on port " ++ show port)
  -- In a real server this loop would accept network connections.
  pure ()

-- Our API request handler that implements the CRUD endpoints.
handleRequest : Connection -> Request -> IO Response
handleRequest conn req =
  case (req.method, req.path) of
    -- Create user: POST /users
    ("POST", "/users") =>
         case parseUserJson req.body of
           Right (name, email) =>
               do user <- createUserDB conn name email
                  pure (MkResponse 201 (userToJson user))
           Left err => pure (MkResponse 400 ("Invalid JSON: " ++ err))
    -- List all users: GET /users
    ("GET", "/users") =>
         do users <- getUsersDB conn
            pure (MkResponse 200 (usersToJson users))
    -- Get user by id: GET /users/{id}
    ("GET", path) =>
         if (take 7 path == "/users/") then
           let idStr = drop 7 path in
           case castInt idStr of
             Just uid =>
                  do maybeUser <- getUserByIdDB conn uid
                     case maybeUser of
                       Just user => pure (MkResponse 200 (userToJson user))
                       Nothing   => pure (MkResponse 404 "User not found")
             Nothing => pure (MkResponse 400 "Invalid ID")
         else pure (MkResponse 404 "Not Found")
    -- Update user: PUT /users/{id}
    ("PUT", path) =>
         if (take 7 path == "/users/") then
           let idStr = drop 7 path in
           case castInt idStr of
             Just uid =>
                  case parseUserJson req.body of
                    Right (name, email) =>
                         do updated <- updateUserDB conn uid name email
                            if updated then pure (MkResponse 200 "User updated")
                                       else pure (MkResponse 404 "User not found")
                    Left err => pure (MkResponse 400 ("Invalid JSON: " ++ err))
             Nothing => pure (MkResponse 400 "Invalid ID")
         else pure (MkResponse 404 "Not Found")
    -- Delete user: DELETE /users/{id}
    ("DELETE", path) =>
         if (take 7 path == "/users/") then
           let idStr = drop 7 path in
           case castInt idStr of
             Just uid =>
                  do deleted <- deleteUserDB conn uid
                     if deleted then pure (MkResponse 200 "User deleted")
                                else pure (MkResponse 404 "User not found")
             Nothing => pure (MkResponse 400 "Invalid ID")
         else pure (MkResponse 404 "Not Found")
    -- Fallback for unmatched routes.
    _ => pure (MkResponse 404 "Not Found")

-- Main entry point.
main : IO ()
main = do
  maybePwd <- getEnv "PGPASSWORD"
  case maybePwd of
    Nothing  => putStrLn "PGPASSWORD not set"
    Just pwd =>
       let connStr = "host=host.docker.internal port=5432 dbname=complang user=testuser password=" ++ pwd in
       conn <- connectDB connStr
       listenAndServe 8080 (handleRequest conn)
       pure ()
