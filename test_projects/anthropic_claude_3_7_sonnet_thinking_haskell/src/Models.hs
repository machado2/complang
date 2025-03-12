{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject)
import GHC.Generics (Generic)

-- User model representing the users table in our database
data User = User
  { userId :: Maybe Int  -- Maybe because for creation we don't have ID yet
  , userName :: String
  , userEmail :: String
  } deriving (Show, Generic)

-- User input for creation/update (without ID)
data UserInput = UserInput
  { inputName :: String
  , inputEmail :: String
  } deriving (Show, Generic)

-- JSON instances for User
instance ToJSON User where
  toJSON user = object
    [ "id" .= userId user
    , "name" .= userName user
    , "email" .= userEmail user
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "email"

-- JSON instances for UserInput
instance ToJSON UserInput where
  toJSON input = object
    [ "name" .= inputName input
    , "email" .= inputEmail input
    ]

instance FromJSON UserInput where
  parseJSON = withObject "UserInput" $ \v -> UserInput
    <$> v .: "name"
    <*> v .: "email"
