
module Main

import Effects
import Effects.Exception
import Data.List
import Data.Maybe

%language ElabReflection

-- Placeholder for database interaction library
-- Assuming a library with functions like:
-- dbConnect : String -> String -> String -> String -> IO DBConnection
-- dbQuery : DBConnection -> String -> IO (List (List String))
-- dbExecute : DBConnection -> String -> String -> IO ()
-- dbClose : DBConnection -> IO ()

-- Placeholder for web framework library
-- Assuming a library with functions like:
-- startServer : Int -> (HTTPRequest -> IO HTTPResponse) -> IO ()
-- HTTPRequest : Type
-- HTTPResponse : Type
-- JSON : Type
-- jsonEncode : JSON -> String
-- jsonDecode : String -> Maybe JSON

-- Types
data User = User { id : Int, name : String, email : String }

-- Convert a database row to a User
rowToUser : List String -> Maybe User
rowToUser [id, name, email] = Just (User (stringToInteger id) name email)
rowToUser _ = Nothing

-- Database connection parameters (read from environment variables)
dbHost : String
dbHost = "host.docker.internal"

dbPort : String
dbPort = "5432"

dbName : String
dbName = "complang"

dbUser : String
dbUser = "testuser"

dbPassword : IO String
dbPassword = getEnv "PGPASSWORD"

-- Helper function to convert String to Int safely
stringToInteger : String -> String -> Int
stringToInteger str def = defaultTo 0 (parseInteger str)

-- | Get all users
getUsers : IO (List User)
getUsers = do
  pwd <- dbPassword
  -- Placeholder for database connection
  let connStr = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " dbname=" ++ dbName ++ " user=" ++ dbUser ++ " password=" ++ pwd
  --conn <- liftIO $ dbConnect dbHost dbName dbUser pwd
  putStrLn "Connecting to database..."
  -- Placeholder for database query
  let query = "SELECT id, name, email FROM users"
  --result <- liftIO $ dbQuery conn query
  --liftIO $ dbClose conn
  putStrLn "Querying database..."
  --let users = mapMaybe rowToUser result
  pure [] -- users placeholder -- Remove later

-- | Get user by ID
getUserById : Int -> IO (Maybe User)
getUserById userId = do
  pwd <- dbPassword
    -- Placeholder for database connection
  let connStr = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " dbname=" ++ dbName ++ " user=" ++ dbUser ++ " password=" ++ pwd
  --conn <- liftIO $ dbConnect dbHost dbName dbUser pwd
  putStrLn "Connecting to database..."
  -- Placeholder for database query
  let query = "SELECT id, name, email FROM users WHERE id = " ++ show userId
  --result <- liftIO $ dbQuery conn query
  --liftIO $ dbClose conn
  putStrLn "Querying database..."
  --case result of
  --[] -> pure Nothing
  --row :: _ -> case rowToUser row of
       --Just user => pure (Just user)
       --Nothing => pure Nothing
  pure Nothing -- user placeholder -- Remove later

-- | Create a new user
createUser : String -> String -> IO (Maybe User)
createUser name email = do
  pwd <- dbPassword
  -- Placeholder for database connection
  let connStr = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " dbname=" ++ dbName ++ " user=" ++ dbUser ++ " password=" ++ pwd
  --conn <- liftIO $ dbConnect dbHost dbName dbUser pwd
  putStrLn "Connecting to database..."
  -- Placeholder for database insertion
  let query = "INSERT INTO users (name, email) VALUES ('" ++ name ++ "', '" ++ email ++ "') RETURNING id, name, email"
  --result <- liftIO $ dbQuery conn query
  --liftIO $ dbClose conn
  putStrLn "Inserting user..."
  --case result of
  --[] -> pure Nothing
  --row :: _ -> case rowToUser row of
   --  Just user => pure (Just user)
    -- Nothing => pure Nothing
  pure Nothing  -- user placeholder -- Remove later

-- | Update an existing user
updateUser : Int -> String -> String -> IO Bool
updateUser userId name email = do
  pwd <- dbPassword
  -- Placeholder for database connection
  let connStr = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " dbname=" ++ dbName ++ " user=" ++ dbUser ++ " password=" ++ pwd
  --conn <- liftIO $ dbConnect dbHost dbName dbUser pwd
  putStrLn "Connecting to database..."
  -- Placeholder for database update
  let query = "UPDATE users SET name = '" ++ name ++ "', email = '" ++ email ++ "' WHERE id = " ++ show userId
  --rowsAffected <- liftIO $ dbExecute conn query
  --liftIO $ dbClose conn
  putStrLn "Updating user..."
  pure False  -- rowsAffected placeholder -- Remove later

-- | Delete a user
deleteUser : Int -> IO Bool
deleteUser userId = do
  pwd <- dbPassword
  -- Placeholder for database connection
  let connStr = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " dbname=" ++ dbName ++ " user=" ++ dbUser ++ " password=" ++ pwd
  --conn <- liftIO $ dbConnect dbHost dbName dbUser pwd
  putStrLn "Connecting to database..."
  -- Placeholder for database deletion
  let query = "DELETE FROM users WHERE id = " ++ show userId
  --rowsAffected <- liftIO $ dbExecute conn query
  --liftIO $ dbClose conn
  putStrLn "Deleting user..."
  pure False  -- rowsAffected placeholder -- Remove later

-- | HTTP request handler
handleRequest : String -> IO String --HTTPRequest to String for now for simplicity
handleRequest req = do
  putStrLn $ "Received request: " ++ req
  pure "OK"

-- | Main function
main : IO ()
main = do
  putStrLn "Starting server..."
  -- Placeholder for starting the server
  --liftIO $ startServer 8080 handleRequest
  pure ()
