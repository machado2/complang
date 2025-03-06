{-# LANGUAGE OverloadedStrings #-}
module UserInput where

import Data.Aeson

-- Define the data structure for user input
data UserInput = UserInput { inputName :: String, inputEmail :: String } deriving (Show, Generic)

instance FromJSON UserInput where
    parseJSON = withObject "UserInput" $ \v -> UserInput <$> v .: "name" <*> v .: "email"