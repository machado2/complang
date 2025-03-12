import json

type
  User = object
    id: int
    name: string
    email: string

# Custom serializer for User type.
converter to(JsonNode, User):
  result = %* {
    "id": $User.id,
    "name": $User.name,
    "email": $User.email
  }

converter to(User, JsonNode):
  result = User(
    id: source["id"].getInt(),
    name: source["name"].getStr(),
    email: source["email"].getStr()
  )
