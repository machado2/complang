import jester
import db_postgres
import json
import os

# Helper procedure to escape single quotes in strings (for SQL safety)
proc escape(s: string): string =
  return s.replace("'", "''")

# Get the PostgreSQL password from the environment variable.
let pgPassword = getEnv("PGPASSWORD")
if pgPassword.len == 0:
  quit("PGPASSWORD environment variable not set")

# Open a connection to the PostgreSQL database.
var db = open("complang", user="testuser", password=pgPassword, host="host.docker.internal", port=5432)

# Configure the server to listen on port 8080.
settings.port = 8080

routes:
  # POST /users: Create a user.
  post "/users":
    try:
      let bodyJson = parseJson(request.body)
      let name = bodyJson["name"].getStr
      let email = bodyJson["email"].getStr
      let insertQuery = "INSERT INTO users (name, email) VALUES ('" & escape(name) & "', '" & escape(email) & "') RETURNING id;"
      let result = db.exec(insertQuery)
      if result.len > 0:
        let newId = result[0][0].getInt
        var resObj = newJObject()
        resObj["id"] = newJNumber(newId)
        resObj["name"] = newJString(name)
        resObj["email"] = newJString(email)
        response.status = 201
        response.send($resObj)
      else:
        response.status = 500
        response.send("Error creating user")
    except:
      response.status = 400
      response.send("Invalid JSON body or missing fields")

  # GET /users: Return a list of all users.
  get "/users":
    var arr = newJArray()
    for row in db.exec("SELECT id, name, email FROM users;"):
      var obj = newJObject()
      obj["id"] = newJNumber(row[0].getInt)
      obj["name"] = newJString(row[1].getStr)
      obj["email"] = newJString(row[2].getStr)
      arr.add(obj)
    response.send($arr)

  # GET /users/{id}: Returns a single user by id.
  get "/users/:id":
    let userId = param("id")
    let query = "SELECT id, name, email FROM users WHERE id = " & userId & ";"
    let result = db.exec(query)
    if result.len == 0:
      response.status = 404
      response.send("User not found")
    else:
      let row = result[0]
      var obj = newJObject()
      obj["id"] = newJNumber(row[0].getInt)
      obj["name"] = newJString(row[1].getStr)
      obj["email"] = newJString(row[2].getStr)
      response.send($obj)

  # PUT /users/{id}: Update a user.
  put "/users/:id":
    let userId = param("id")
    try:
      let bodyJson = parseJson(request.body)
      let name = bodyJson["name"].getStr
      let email = bodyJson["email"].getStr
      let updateQuery = "UPDATE users SET name = '" & escape(name) & "', email = '" & escape(email) & "' WHERE id = " & userId & ";"
      let _ = db.exec(updateQuery)
      # Check if any row was affected (assuming db.affectedRows is available)
      if db.affectedRows == 0:
        response.status = 404
        response.send("User not found")
      else:
        response.status = 200
        response.send("User updated")
    except:
      response.status = 400
      response.send("Invalid JSON body or missing fields")

  # DELETE /users/{id}: Delete a user.
  delete "/users/:id":
    let userId = param("id")
    let deleteQuery = "DELETE FROM users WHERE id = " & userId & ";"
    let _ = db.exec(deleteQuery)
    if db.affectedRows == 0:
      response.status = 404
      response.send("User not found")
    else:
      response.status = 200
      response.send("User deleted")
