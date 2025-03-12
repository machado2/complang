
import jester
import db_postgres
import json
import os
import strutils

# Database connection parameters
const
  dbHost = "host.docker.internal"
  dbPort = 5432
  dbName = "complang" 
  dbUser = "testuser"

type
  User = object
    id: int
    name: string
    email: string

proc openDb(): DbConn =
  let dbPassword = getEnv("PGPASSWORD")
  return open(dbHost, dbUser, dbPassword, dbName, port = dbPort)

proc userToJson(user: User): JsonNode =
  result = %* {
    "id": user.id,
    "name": user.name,
    "email": user.email
  }

proc jsonToUser(node: JsonNode): User =
  result.id = if node.hasKey("id"): node["id"].getInt() else: 0
  result.name = node["name"].getStr()
  result.email = node["email"].getStr()

settings:
  port = Port(8080)

routes:
  # POST /users: Creates a user
  # Accepts JSON { "name": string, "email": string }
  # Returns 201 with { "id": int, "name": string, "email": string }
  post "/users":
    try:
      let reqBody = request.body
      if reqBody.len == 0:
        resp Http400, $(%* {"error": "No JSON data provided"}), "application/json"
        return

      let json = parseJson(reqBody)
      
      if not json.hasKey("name") or not json.hasKey("email"):
        resp Http400, $(%* {"error": "Missing required fields: name and email"}), "application/json"
        return
      
      let user = jsonToUser(json)
      
      var db = openDb()
      defer: db.close()
      
      let query = sql"INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
      let row = db.getRow(query, user.name, user.email)
      
      let newUser = User(
        id: parseInt(row[0]),
        name: row[1],
        email: row[2]
      )
      
      resp Http201, $userToJson(newUser), "application/json"
    except JsonParsingError:
      resp Http400, $(%* {"error": "Invalid JSON format"}), "application/json"
    except DbError:
      resp Http500, $(%* {"error": "Database error: " & getCurrentExceptionMsg()}), "application/json"
    except:
      resp Http500, $(%* {"error": "Internal server error: " & getCurrentExceptionMsg()}), "application/json"
  
  # GET /users: Returns all users
  # Returns a list of all users as [{"id": int, "name": string, "email": string}, ...], status 200
  get "/users":
    try:
      var db = openDb()
      defer: db.close()
      
      let query = sql"SELECT id, name, email FROM users"
      
      var usersJson = newJArray()
      for row in db.fastRows(query):
        let user = User(
          id: parseInt(row[0]),
          name: row[1],
          email: row[2]
        )
        usersJson.add(userToJson(user))
      
      resp Http200, $usersJson, "application/json"
    except DbError:
      resp Http500, $(%* {"error": "Database error: " & getCurrentExceptionMsg()}), "application/json"
    except:
      resp Http500, $(%* {"error": "Internal server error: " & getCurrentExceptionMsg()}), "application/json"
  
  # GET /users/{id}: Returns a single user
  # Returns a single user as { "id": int, "name": string, "email": string }, status 200, or 404 if not found
  get "/users/@id":
    try:
      let id = parseInt(@"id")
      
      var db = openDb()
      defer: db.close()
      
      let query = sql"SELECT id, name, email FROM users WHERE id = $1"
      let row = db.getRow(query, $id)
      
      if row.len == 0 or row[0] == "":
        resp Http404, $(%* {"error": "User not found"}), "application/json"
      else:
        let user = User(
          id: parseInt(row[0]),
          name: row[1],
          email: row[2]
        )
        
        resp Http200, $userToJson(user), "application/json"
    except ValueError:
      resp Http400, $(%* {"error": "Invalid user ID format"}), "application/json"
    except DbError:
      resp Http500, $(%* {"error": "Database error: " & getCurrentExceptionMsg()}), "application/json"
    except:
      resp Http500, $(%* {"error": "Internal server error: " & getCurrentExceptionMsg()}), "application/json"
  
  # PUT /users/{id}: Updates a user
  # Accepts { "name": string, "email": string }, returns 204, or 404 if not found
  put "/users/@id":
    try:
      let id = parseInt(@"id")
      
      let reqBody = request.body
      if reqBody.len == 0:
        resp Http400, $(%* {"error": "No JSON data provided"}), "application/json"
        return

      let json = parseJson(reqBody)
      
      if not json.hasKey("name") or not json.hasKey("email"):
        resp Http400, $(%* {"error": "Missing required fields: name and email"}), "application/json"
        return
      
      let user = jsonToUser(json)
      
      var db = openDb()
      defer: db.close()
      
      # Check if user exists
      let checkQuery = sql"SELECT id FROM users WHERE id = $1"
      let checkRow = db.getRow(checkQuery, $id)
      
      if checkRow.len == 0 or checkRow[0] == "":
        resp Http404, $(%* {"error": "User not found"}), "application/json"
      else:
        let updateQuery = sql"UPDATE users SET name = $1, email = $2 WHERE id = $3"
        db.exec(updateQuery, user.name, user.email, $id)
        
        resp Http204
    except ValueError:
      resp Http400, $(%* {"error": "Invalid user ID format"}), "application/json"
    except JsonParsingError:
      resp Http400, $(%* {"error": "Invalid JSON format"}), "application/json"
    except DbError:
      resp Http500, $(%* {"error": "Database error: " & getCurrentExceptionMsg()}), "application/json"
    except:
      resp Http500, $(%* {"error": "Internal server error: " & getCurrentExceptionMsg()}), "application/json"
  
  # DELETE /users/{id}: Deletes a user
  # Returns 204, or 404 if not found
  delete "/users/@id":
    try:
      let id = parseInt(@"id")
      
      var db = openDb()
      defer: db.close()
      
      # Check if user exists
      let checkQuery = sql"SELECT id FROM users WHERE id = $1"
      let checkRow = db.getRow(checkQuery, $id)
      
      if checkRow.len == 0 or checkRow[0] == "":
        resp Http404, $(%* {"error": "User not found"}), "application/json"
      else:
        let deleteQuery = sql"DELETE FROM users WHERE id = $1"
        db.exec(deleteQuery, $id)
        
        resp Http204
    except ValueError:
      resp Http400, $(%* {"error": "Invalid user ID format"}), "application/json"
    except DbError:
      resp Http500, $(%* {"error": "Database error: " & getCurrentExceptionMsg()}), "application/json"
    except:
      resp Http500, $(%* {"error": "Internal server error: " & getCurrentExceptionMsg()}), "application/json"
