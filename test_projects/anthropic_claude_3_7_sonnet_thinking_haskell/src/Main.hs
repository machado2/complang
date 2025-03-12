module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404, status201, status204)
import System.Environment (lookupEnv)

import Database (initDB, closeDB)
import Api (app)

main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  
  -- Get PostgreSQL password from environment variable
  pgPassword <- lookupEnv "PGPASSWORD"
  case pgPassword of
    Nothing -> putStrLn "Warning: PGPASSWORD environment variable not set"
    Just _  -> putStrLn "Found PGPASSWORD environment variable"
  
  -- Initialize database connection
  conn <- initDB
  
  -- Run the application
  putStrLn "Server is running on port 8080"
  run 8080 (app conn)
  
  -- Close database connection when done
  closeDB conn
