
module Models

import Data.SortedMap
import Language.JSON

||| User record type
public export
record User where
  constructor MkUser
  id : Maybe Integer
  name : String
  email : String

||| Convert a user to JSON
public export
userToJSON : User -> JSON
userToJSON (MkUser id name email) =
  JObject $ SortedMap.fromList [
    ("id", maybe JNull (JNumber . cast) id),
    ("name", JString name),
    ("email", JString email)
  ]

||| Convert a list of users to JSON
public export
usersToJSON : List User -> JSON
usersToJSON users = JArray $ map userToJSON users

||| Try to parse a user from JSON
public export
parseUser : JSON -> Maybe User
parseUser (JObject obj) = do
  name <- lookup "name" obj >>= getString
  email <- lookup "email" obj >>= getString
  pure $ MkUser Nothing name email
  where
    getString : JSON -> Maybe String
    getString (JString str) = Just str
    getString _ = Nothing
parseUser _ = Nothing
