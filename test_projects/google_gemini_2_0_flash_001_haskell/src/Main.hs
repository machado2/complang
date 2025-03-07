{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Servant
import Servant.Server
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Lazy.Char8 (pack)
import System.Environment (getEnv)
import Data.Pool
import Data.ResourcePool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    name Text
    email Text
    deriving Generic
|]


type UserAPI = "users" :> (
       ReqBody '[JSON] (UserWithoutId) :> Post '[JSON] User
  :<|> Get '[JSON] [User]
  :<|> Capture "user_id" Int :> (
         Get '[JSON] User
    :<|> ReqBody '[JSON] (UserWithoutId) :> PutNoContent
    :<|> DeleteNoContent
     )
   )

data UserWithoutId = UserWithoutId
  { userName :: Text
  , userEmail :: Text
  } deriving (Generic, Show)

instance FromJSON UserWithoutId
instance ToJSON UserWithoutId

instance ToJSON User

type AppM env = ReaderT env IO

main :: IO ()
main = do
  pgPass <- getEnv "PGPASSWORD"
  let connStr = "host=host.docker.internal port=5432 dbname=complang user=testuser password=" ++ pgPass
  pool <- createPostgresqlPool (pack connStr) 10

  runResourceT $ do
      runSqlPool (runMigration migrateAll) pool

  let
    server :: Pool Postgresql -> Server UserAPI
    server pool = createUser :<|> listUsers :<|> userDetails :<|> updateUser :<|> deleteUser
      where
        createUser :: UserWithoutId -> AppM (Pool Postgresql) User
        createUser userWithoutId = do
          userId <- liftIO $ runSqlPool
            (insert $ User (userName userWithoutId) (userEmail userWithoutId)) pool
          liftIO $ runSqlPool (get userId) pool >>= \(Just user) -> pure user

        listUsers :: AppM (Pool Postgresql) [User]
        listUsers = liftIO $ runSqlPool (selectList [] [Asc UserId]) pool >>= returnUsers

        userDetails :: Int -> AppM (Pool Postgresql) User
        userDetails userId = do
          mUser <- liftIO $ runSqlPool (get (toSqlKey $ fromIntegral userId)) pool
          case mUser of
            Just user -> return user
            Nothing   -> throwError err404

        updateUser :: Int -> UserWithoutId -> AppM (Pool Postgresql) NoContent
        updateUser userId userWithoutId = do
          mUser <- liftIO $ runSqlPool (get (toSqlKey $ fromIntegral userId)) pool
          case mUser of
            Just _ -> do
              liftIO $ runSqlPool (replace (toSqlKey $ fromIntegral userId) (User (userName userWithoutId) (userEmail userWithoutId))) pool
              return NoContent
            Nothing   -> throwError err404

        deleteUser :: Int -> AppM (Pool Postgresql) NoContent
        deleteUser userId = do
          mUser <- liftIO $ runSqlPool (get (toSqlKey $ fromIntegral userId)) pool
          case mUser of
            Just _ -> do
              liftIO $ runSqlPool (delete (toSqlKey $ fromIntegral userId)) pool
              return NoContent
            Nothing   -> throwError err404

        returnUsers :: [Entity User] -> IO [User]
        returnUsers entities = return $ map entityVal entities


    app :: Pool Postgresql -> Application
    app pool = serve userAPI (server pool)
      where
        userAPI :: Proxy UserAPI
        userAPI = Proxy

  putStrLn "Starting server on port 8080"
  run 8080 (app pool)
