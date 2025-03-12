
import jester
import jsony
import db_postgres
import os

type
  User = object
    id: int
    name: string
    email: string

let pgPassword = os.getenv("PGPASSWORD")

# Connect to the database
let db = connect(
  host = "host.docker.internal",
  port = 5432,
  user = "testuser",
  password = pgPassword,
  database = "complang"
)

# JSON serialization for User type
converter jsonConverter {.fromUser, toUser.} = (
  (User, JsonNode, void):
    result = %* {
      "id": $User.id,
      "name": $User.name,
      "email": $User.email
    }
  ,
  (JsonNode, User, void):
    result = User(
      id: JsonNode["id"].getStr().parseInt(),
      name: JsonNode["name"].getStr(),
      email: JsonNode["email"].getStr()
    )
)

# Error handling
proc userNotFound(id: int): string =
  return "User with id " & $id & " not found"

# API Endpoints
routes:
  post "/users":
    let body = request.body
    try:
      let userData = parseJson(body)
      let newUser: User = to(userData, User)

      # Insert into database
      let insertSql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id"
      var userId: int
      db.exec(insertSql, [newUser.name, newUser.email]) do (row: Row) =>
        userId = row.getInt(0)

      let createdUser = User(id: userId, name: newUser.name, email: newUser.email)
      response.status = Http201
      response.body = createdUser.toJson()
    except JsonParsingError:
      response.status = Http400
      response.body = "Invalid JSON format"
    except Exception as e:
      response.status = Http500
      response.body = "Internal Server Error: " & e.msg

  get "/users":
    var users: seq[User] = @[]
    db.exec("SELECT id, name, email FROM users") do (row: Row) =>
      users.add(User(id: row.getInt(0), name: row.getText(1), email: row.getText(2)))
    response.body = users.toJson

  get "/users/{id}":
    let id = param("id").parseInt
    let sql = "SELECT id, name, email FROM users WHERE id = $1"
    var user: User
    var found = false
    db.exec(sql, [id]) do (row: Row) =>
      user = User(id: row.getInt(0), name: row.getText(1), email: row.getText(2))
      found = true
    if found:
      response.body = user.toJson
    else:
      response.status = Http404
      response.body = userNotFound(id)

  put "/users/{id}":
    let id = param("id").parseInt
    let body = request.body
    try:
      let userData = parseJson(body)
      let updatedUser: User = to(userData, User)

      let updateSql = "UPDATE users SET name = $1, email = $2 WHERE id = $3"
      let rowsAffected = db.exec(updateSql, [updatedUser.name, updatedUser.email, id])
      if rowsAffected > 0:
        response.status = Http204
      else:
        response.status = Http404
        response.body = userNotFound(id)
    except JsonParsingError:
      response.status = Http400
      response.body = "Invalid JSON format"
    except Exception as e:
      response.status = Http500
      response.body = "Internal Server Error: " & e.msg

  delete "/users/{id}":
    let id = param("id").parseInt
    let deleteSql = "DELETE FROM users WHERE id = $1"
    let rowsAffected = db.exec(deleteSql, [id])
    if rowsAffected > 0:
      response.status = Http204
    else:
      response.status = Http404
      response.body = userNotFound(id)


# Compile options
{.passL: "-L/usr/lib/postgresql".}
{.passL: "-lpq".}
# Start the server
serve(port = 8080)

