module Config where

import Database.PostgreSQL.Simple
import System.Environment (getEnv)

-- Function to get the database connection string
getConnString :: IO String
getConnString = do
    dbpassword <- getEnv "PGPASSWORD"
    return $ "host=host.docker.internal dbname=complang user=testuser password=" ++ dbpassword
