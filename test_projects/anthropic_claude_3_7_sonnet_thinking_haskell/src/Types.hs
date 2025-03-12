module Types where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Data.Text (Text)

-- User data type for the complete user with ID
data User = User 
    { userId :: Int
    , userName :: Text
    , userEmail :: Text
    } deriving (Show, Eq)

-- User data type for creating or updating a user (without ID)
data UserPayload = UserPayload
    { payloadName :: Text
    , payloadEmail :: Text
    } deriving (Show, Eq)

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

-- JSON instances for UserPayload
instance ToJSON UserPayload where
    toJSON user = object
        [ "name" .= payloadName user
        , "email" .= payloadEmail user
        ]

instance FromJSON UserPayload where
    parseJSON = withObject "UserPayload" $ \v -> UserPayload
        <$> v .: "name"
        <*> v .: "email"

-- Database instances
instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToField UserPayload where
    toField = error "UserPayload is not directly convertible to a field"
