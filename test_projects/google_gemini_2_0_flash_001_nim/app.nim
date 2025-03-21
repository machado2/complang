
import jester
import jsony
import db_connector
import os

# Load environment variables
let pgUser = "testuser"
let pgPass = os.getenv("PGPASSWORD", "") # Get password from env var, default to empty string if not set
let pgDatabase = "complang"
let pgHost = "host.docker.internal"
let pgPort = 5432

# Database connection string
let connectionString = "host=" & pgHost & " port=" & $pgPort & " dbname=" & pgDatabase & " user=" & pgUser & " password=" & pgPass

# User data structure
type
  User = ref object
    id: int
    name: string
    email: string

# JSON serialization for User object
converter to(Json, User):
  obj(
    "id": from(x.id),
    "name": from(x.name),
    "email": from(x.email)
  )

converter to(User, Json):
  User(
    id: x["id"].getInt(),
    name: x["name"].getString(),
    email: x["email"].getString()
  )

# API Routes

# GET /users: Returns a list of all users
get "/users":
  var users: seq[User] = @[]
  db connect:
    var query = db.prepare("SELECT id, name, email FROM users")
    defer: query.close() # Ensure resource cleanup
    let result = query.executeQuery()
    defer: result.close() # Ensure resource cleanup
    while result.next():
      let id = result.getInt(1)
      let name = result.getString(2)
      let email = result.getString(3)
      let user = User(id: id, name: name, email: email)
      users.add(user)
  resp Json(users)

# GET /users/{id}: Returns a single user
get "/users/:id":
  let id = parseInt(param("id"))
  db connect:
    var query = db.prepare("SELECT id, name, email FROM users WHERE id = $1")
    defer: query.close() # Ensure resource cleanup
    query.setInt(1, id)
    let result = query.executeQuery()
    defer: result.close() # Ensure resource cleanup
    if result.next():
      let id = result.getInt(1)
      let name = result.getString(2)
      let email = result.getString(3)
      resp Json(User(id: id, name: name, email: email))
    else:
      resp Http404, "User not found"

# POST /users: Creates a user
post "/users":
  let payload = parseJson(req.body)
  let name = payload["name"].getString()
  let email = payload["email"].getString()
  db connect:
    var query = db.prepare("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email")
    defer: query.close() # Ensure resource cleanup
    query.setString(1, name)
    query.setString(2, email)
    let result = query.executeQuery()
    defer: result.close() # Ensure resource cleanup
    if result.next():
      let id = result.getInt(1)
      let name = result.getString(2)
      let email = result.getString(3)
      resp Http201, Json(User(id: id, name: name, email: email))
    else:
      resp Http500, "Failed to create user"

# PUT /users/{id}: Updates a user
put "/users/:id":
  let id = parseInt(param("id"))
  let payload = parseJson(req.body)
  let name = payload["name"].getString()
  let email = payload["email"].getString()
  db connect:
    var query = db.prepare("UPDATE users SET name = $1, email = $2 WHERE id = $3")
    defer: query.close() # Ensure resource cleanup
    query.setString(1, name)
    query.setString(2, email)
    query.setInt(3, id)
    let updated = query.executeUpdate()
    if updated > 0:
      resp Http204
    else:
      resp Http404, "User not found"

# DELETE /users/{id}: Deletes a user
delete "/users/:id":
  let id = parseInt(param("id"))
  db connect:
    var query = db.prepare("DELETE FROM users WHERE id = $1")
    defer: query.close() # Ensure resource cleanup
    query.setInt(1, id)
    let deleted = query.executeUpdate()
    if deleted > 0:
      resp Http204
    else:
      resp Http404, "User not found"

# Start the server
serve(port = 8080)
