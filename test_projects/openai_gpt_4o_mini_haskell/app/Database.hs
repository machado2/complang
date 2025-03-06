module Database where

import Database.PostgreSQL.Simple
import System.Environment (getEnv)

-- Function to connect to the database
connectDB :: IO Connection
connectDB = do
    connStr <- getEnv "DATABASE_URL"
    connectPostgreSQL connStr