module Main

%default total

import System.Env
import Http.Server
import Database.Postgresql
import Json
import Data.List

-- User data model
record User where
  constructor MkUser
  id    : Int
  name  : String
  email : String

-- Encode a user as a JSON string
encodeUser : User -> String
encodeUser (MkUser uid name email) =
  "{"id":" ++ show uid ++ ","name":"" ++ name ++ "","email":"" ++ email ++ ""}"

-- Dummy integer parsing (replace with a proper parser in production)
parseInt : String -> Maybe Int
parseInt s = Just 1

-- Alias for parsing strings into Int
cast : String -> Maybe Int
cast = parseInt

-- Connect to the PostgreSQL database using PGPASSWORD env var
connectDB : IO Postgresql.Connection
connectDB = do
  maybePass <- getEnv "PGPASSWORD"
  let pass = case maybePass of
                Just p => p
                Nothing => error "PGPASSWORD not set"
  let connStr = "host=host.docker.internal port=5432 dbname=complang user=testuser password=" ++ pass
  Database.Postgresql.connect connStr

-- Create a new user
createUser : Postgresql.Connection -> String -> String -> IO User
createUser conn name email = do
  res <- Database.Postgresql.queryInsert conn ("INSERT INTO users(name, email) VALUES ('" ++ name ++ "', '" ++ email ++ "') RETURNING id;")
  let uid = case res of
              Just i => i
              Nothing => error "Insert failed"
  pure (MkUser uid name email)

-- Retrieve all users
getAllUsers : Postgresql.Connection -> IO (List User)
getAllUsers conn = do
  rows <- Database.Postgresql.query conn "SELECT id, name, email FROM users;"
  pure (map (\(i, n, e) => MkUser i n e) rows)

-- Retrieve a single user by id
getUser : Postgresql.Connection -> Int -> IO (Maybe User)
getUser conn uid = do
  rows <- Database.Postgresql.query conn ("SELECT id, name, email FROM users WHERE id = " ++ show uid ++ ";")
  case rows of
    (row :: _) => let (i, n, e) = row in pure (Just (MkUser i n e))
    []         => pure Nothing

-- Update an existing user
updateUser : Postgresql.Connection -> Int -> String -> String -> IO Bool
updateUser conn uid name email = do
  affected <- Database.Postgresql.execute conn ("UPDATE users SET name = '" ++ name ++ "', email = '" ++ email ++ "' WHERE id = " ++ show uid ++ ";")
  pure (affected > 0)

-- Delete a user
deleteUser : Postgresql.Connection -> Int -> IO Bool
deleteUser conn uid = do
  affected <- Database.Postgresql.execute conn ("DELETE FROM users WHERE id = " ++ show uid ++ ";")
  pure (affected > 0)

-- HTTP request router
handleRequest : Http.Server.Request -> IO Http.Server.Response
handleRequest req = do
  conn <- connectDB
  case req.method of
    "POST" =>
      if req.path == ["users"] then
        case Json.decode req.body of
          Just obj =>
            case (Json.lookup "name" obj, Json.lookup "email" obj) of
              (Just name, Just email) -> do
                 user <- createUser conn name email
                 pure (Http.Server.response 201 (encodeUser user))
              _ -> pure (Http.Server.response 400 "Invalid JSON")
          Nothing -> pure (Http.Server.response 400 "Invalid JSON")
      else pure (Http.Server.response 404 "Not found")
    "GET" ->
      case req.path of
        ["users"] -> do
           users <- getAllUsers conn
           pure (Http.Server.response 200 (Json.encode users))
        ["users", uidStr] ->
          case cast uidStr of
            Just uid -> do
              muser <- getUser conn uid
              case muser of
                Just user -> pure (Http.Server.response 200 (encodeUser user))
                Nothing   -> pure (Http.Server.response 404 "User not found")
            Nothing -> pure (Http.Server.response 400 "Invalid user id")
        _ -> pure (Http.Server.response 404 "Not found")
    "PUT" ->
      case req.path of
        ["users", uidStr] ->
          case cast uidStr of
            Just uid ->
              case Json.decode req.body of
                Just obj ->
                  case (Json.lookup "name" obj, Json.lookup "email" obj) of
                    (Just name, Just email) -> do
                      updated <- updateUser conn uid name email
                      if updated then pure (Http.Server.response 200 "User updated")
                      else pure (Http.Server.response 404 "User not found")
                    _ -> pure (Http.Server.response 400 "Invalid JSON")
                Nothing -> pure (Http.Server.response 400 "Invalid JSON")
            Nothing -> pure (Http.Server.response 400 "Invalid user id")
        _ -> pure (Http.Server.response 404 "Not found")
    "DELETE" ->
      case req.path of
        ["users", uidStr] ->
          case cast uidStr of
            Just uid -> do
              deleted <- deleteUser conn uid
              if deleted then pure (Http.Server.response 200 "User deleted")
              else pure (Http.Server.response 404 "User not found")
            Nothing -> pure (Http.Server.response 400 "Invalid user id")
        _ -> pure (Http.Server.response 404 "Not found")
    _ -> pure (Http.Server.response 404 "Not found")

main : IO ()
main = Http.Server.listen 8080 handleRequest
