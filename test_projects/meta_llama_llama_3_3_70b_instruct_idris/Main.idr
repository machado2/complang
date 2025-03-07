module Main

import qualified-postgresql
import qualified-postgresql-psql
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

-- Define the user data type
data User : Type where
  MkUser : Int -> String -> String -> User

-- Define the API endpoints
usersApi : Application
usersApi req respond = do
  -- Connect to the PostgreSQL database
  conn <- connect (mkConfig { host = "host.docker.internal"
                           , port = 5432
                           , database = "complang"
                           , user = "testuser"
                           , password = System.getEnv "PGPASSWORD"
                            })
  -- Handle POST /users
  case pathInfo req of
    ["users"] => do
      -- Create a new user
      body <- requestBody req
      case jsonDecode body of
        Just (MkUser name email) => do
          -- Insert the user into the database
          _ <- execute conn "INSERT INTO users (name, email) VALUES (?,?)" (name, email)
          -- Return the created user
          respond $ responseLBS status201 [(hContentType, "application/json")] (jsonEncode $ MkUser 1 name email)
        Nothing => respond $ responseLBS status400 [(hContentType, "application/json")] "Invalid request body"
    ["users", id] => do
      -- Handle GET /users/{id}
      case method req of
        "GET" => do
          -- Retrieve the user from the database
          _ <- execute conn "SELECT * FROM users WHERE id =?" (id,)
          -- Return the user
          respond $ responseLBS status200 [(hContentType, "application/json")] (jsonEncode $ MkUser (cast id) "" "")
        "PUT" => do
          -- Update the user
          body <- requestBody req
          case jsonDecode body of
            Just (MkUser name email) => do
              -- Update the user in the database
              _ <- execute conn "UPDATE users SET name =?, email =? WHERE id =?" (name, email, id)
              -- Return the updated user
              respond $ responseLBS status200 [(hContentType, "application/json")] (jsonEncode $ MkUser (cast id) name email)
            Nothing => respond $ responseLBS status400 [(hContentType, "application/json")] "Invalid request body"
        "DELETE" => do
          -- Delete the user
          _ <- execute conn "DELETE FROM users WHERE id =?" (id,)
          -- Return a success response
          respond $ responseLBS status204 [(hContentType, "application/json")] ""
    _ => respond $ responseLBS status404 [(hContentType, "application/json")] "Not found"

-- Start the API server
main : IO ()
main = run 8080 usersApi