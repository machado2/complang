
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Aeson
import GHC.Generics
import Data.Typeable
import Database.PostgreSQL.Simple
import System.Environment
import Control.Monad.IO.Class
import Control.Monad
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Servant.Server
import Database.PostgreSQL.Simple.SqlQQ
import Control.Exception (catch,SomeException)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- Data types
data User = User
  { userId :: Int,
    userName :: String,
    userEmail :: String
  } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data UserCreate = UserCreate
  { createName :: String,
    createEmail :: String
  } deriving (Show, Generic)

instance FromJSON UserCreate
instance ToJSON UserCreate

-- API definition
type API = "users" :> Get '[JSON] [User]
      :<|> "users" :> ReqBody '[JSON] UserCreate :> Post '[JSON] User
      :<|> "users" :> Capture "id" Int :> Get '[JSON] User
      :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] UserCreate :> Put '[JSON] ()
      :<|> "users" :> Capture "id" Int :> Delete '[JSON] ()

-- Handlers
getUsers :: Connection -> Handler [User]
getUsers conn = liftIO $ do
  result <- query_ conn [sql|SELECT id, name, email FROM users|]
  return $ map (\(id, name, email) -> User id name email) result

createUser :: Connection -> UserCreate -> Handler User
createUser conn userCreate = liftIO $ do
  result <- query conn [sql|INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email|] (createName userCreate, createEmail userCreate)
  `catch` \(e :: SomeException) -> liftIO $ print e >> throwError err500 {errBody = BS.pack "Failed to create user"}
  case result of
    [(id,name,email)] -> return $ User id name email
    _ -> throwError err500 { errBody = BS.pack "Failed to create user" }

getUser :: Connection -> Int -> Handler User
getUser conn userId = liftIO $ do
  result <- query conn [sql|SELECT id, name, email FROM users WHERE id = ?|] (Only userId)
  case result of
    [(id, name, email)] -> return $ User id name email
    _ -> throwError err404 { errBody = BS.pack "User not found" }

updateUser :: Connection -> Int -> UserCreate -> Handler ()
updateUser conn userId userCreate = liftIO $ do
  affectedRows <- execute conn [sql|UPDATE users SET name = ?, email = ? WHERE id = ?|] (createName userCreate, createEmail userCreate, userId)
  if affectedRows == 0
     then throwError err404 { errBody = BS.pack "User not found" }
     else return ()

deleteUser :: Connection -> Int -> Handler ()
deleteUser conn userId = liftIO $ do
  affectedRows <- execute conn [sql|DELETE FROM users WHERE id = ?|] (Only userId)
  if affectedRows == 0
     then throwError err404 { errBody = BS.pack "User not found" }
     else return ()

-- Server
server :: Connection -> Server API
server conn = getUsers conn
         :<|> createUser conn
         :<|> getUser conn
         :<|> updateUser conn
         :<|> deleteUser conn

app :: Connection -> Application
app conn = serve api (server conn)
  where
    api :: Proxy API
    api = Proxy

-- Database setup
initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  result <- query_ conn [sql|SELECT EXISTS (SELECT 1 FROM pg_tables WHERE tablename = 'users')|]
  case result of
    [[exists]] | exists == (True :: Bool) -> return ()
    _ -> do
      execute_ conn [sql|CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT, email TEXT)|]
      return ()

-- Main function
main :: IO ()
main = do
  pgPass <- getEnv "PGPASSWORD"
  let connInfo = BS.pack $ "host=host.docker.internal port=5432 dbname=test_google_gemini_2_0_flash_001_haskell user=postgres password=" ++ pgPass

  conn <- connectPostgreSQL connInfo
  initializeDatabase conn

  let port = 8080
  putStrLn $ "Starting server on port " ++ show port
  run port (app conn)
