import jester, asyncdispatch, json, os, strutils, options
import db_postgres

type User = object
  id: int
  name: string
  email: string

# Convert User to JSON
proc toJson(user: User): JsonNode =
  result = %* {
    "id": user.id,
    "name": user.name,
    "email": user.email
  }

# Database connection
proc getDbConn(): DbConn =
  let password = getEnv("PGPASSWORD")
  return db_postgres.open("host.docker.internal", "testuser", password, "complang")

# Database operations
proc getAllUsers(): seq[User] =
  var db = getDbConn()
  defer: db.close()
  
  var users: seq[User] = @[]
  for row in db.getAllRows(sql"SELECT id, name, email FROM users"):
    users.add(User(id: parseInt(row[0]), name: row[1], email: row[2]))
  
  return users

proc getUserById(id: int): Option[User] =
  var db = getDbConn()
  defer: db.close()
  
  let rows = db.getAllRows(sql"SELECT id, name, email FROM users WHERE id = $1", $id)
  if rows.len == 0: 
    return none(User)
  
  let row = rows[0]
  return some(User(id: parseInt(row[0]), name: row[1], email: row[2]))

proc createUser(name, email: string): User =
  var db = getDbConn()
  defer: db.close()
  
  db.exec(sql"INSERT INTO users(name, email) VALUES($1, $2)", name, email)
  
  let rows = db.getAllRows(sql"SELECT id FROM users WHERE name = $1 AND email = $2 ORDER BY id DESC LIMIT 1", name, email)
  let id = parseInt(rows[0][0])
  
  return User(id: id, name: name, email: email)

proc updateUser(id: int, name, email: string): bool =
  var db = getDbConn()
  defer: db.close()
  
  try:
    db.exec(sql"UPDATE users SET name = $1, email = $2 WHERE id = $3", name, email, $id)
    
    # Check if the update affected any rows
    let rows = db.getAllRows(sql"SELECT id FROM users WHERE id = $1", $id)
    return rows.len > 0
  except:
    return false

proc deleteUser(id: int): bool =
  var db = getDbConn()
  defer: db.close()
  
  try:
    # Check if user exists first
    let existsRows = db.getAllRows(sql"SELECT id FROM users WHERE id = $1", $id)
    if existsRows.len == 0:
      return false
    
    db.exec(sql"DELETE FROM users WHERE id = $1", $id)
    return true
  except:
    return false

# Set up Jester
settings:
  port = Port(8080)
  bindAddr = "0.0.0.0"

# Define routes
routes:
  get "/users":
    let users = getAllUsers()
    var jsonArray = newJArray()
    for user in users:
      jsonArray.add(toJson(user))
    resp Http200, $jsonArray, "application/json"
  
  get "/users/@id":
    try:
      let id = parseInt(@"id")
      let userOpt = getUserById(id)
      
      if userOpt.isSome:
        resp Http200, $toJson(userOpt.get()), "application/json"
      else:
        resp Http404, $(%*{"error":"User not found"}), "application/json"
    except ValueError:
      resp Http400, $(%*{"error":"Invalid ID format"}), "application/json"
  
  post "/users":
    try:
      let data = parseJson(request.body)
      if not data.hasKey("name") or not data.hasKey("email"):
        resp Http400, $(%*{"error":"Name and email are required"}), "application/json"
      
      let name = data["name"].getStr()
      let email = data["email"].getStr()
      let user = createUser(name, email)
      
      resp Http201, $toJson(user), "application/json"
    except:
      resp Http500, $(%*{"error":"Internal server error"}), "application/json"
  
  put "/users/@id":
    try:
      let id = parseInt(@"id")
      let data = parseJson(request.body)
      
      if not data.hasKey("name") or not data.hasKey("email"):
        resp Http400, $(%*{"error":"Name and email are required"}), "application/json"
      
      let name = data["name"].getStr()
      let email = data["email"].getStr()
      let success = updateUser(id, name, email)
      
      if success:
        resp Http204, "", "application/json"
      else:
        resp Http404, $(%*{"error":"User not found"}), "application/json"
    except ValueError:
      resp Http400, $(%*{"error":"Invalid ID format"}), "application/json"
    except:
      resp Http500, $(%*{"error":"Internal server error"}), "application/json"
  
  delete "/users/@id":
    try:
      let id = parseInt(@"id")
      let success = deleteUser(id)
      
      if success:
        resp Http204, "", "application/json"
      else:
        resp Http404, $(%*{"error":"User not found"}), "application/json"
    except ValueError:
      resp Http400, $(%*{"error":"Invalid ID format"}), "application/json"
    except:
      resp Http500, $(%*{"error":"Internal server error"}), "application/json"
