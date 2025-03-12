local lapis = require("lapis")
local app = lapis.Application()
local pgmoon = require("pgmoon")

-- Helper function to establish a PostgreSQL connection using pgmoon
local function get_db()
  local db = pgmoon.new({
    host = "host.docker.internal",
    port = 5432,
    database = "complang",
    user = "testuser",
    password = os.getenv("PGPASSWORD")
  })
  local ok, err = db:connect()
  if not ok then
    return nil, err
  end
  return db
end

-- POST /users: Create a new user
app:post("/users", function(self)
  local name = self.params.name
  local email = self.params.email
  if not name or not email then
    return { status = 400, json = { error = "Missing fields: name and email required" } }
  end
  local db, err = get_db()
  if not db then
    return { status = 500, json = { error = "DB connection error: " .. err } }
  end
  name = db:escape_literal(name)
  email = db:escape_literal(email)
  local query = string.format("INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email", name, email)
  local res, err = db:query(query)
  if not res then
    return { status = 500, json = { error = "Insert error: " .. err } }
  end
  db:keepalive()
  return { status = 201, json = res[1] }
end)

-- GET /users: Retrieve all users
app:get("/users", function(self)
  local db, err = get_db()
  if not db then
    return { status = 500, json = { error = "DB connection error: " .. err } }
  end
  local res, err = db:query("SELECT id, name, email FROM users")
  if not res then
    return { status = 500, json = { error = "Query error: " .. err } }
  end
  db:keepalive()
  return { status = 200, json = res }
end)

-- GET /users/:id: Retrieve a single user by id
app:get("/users/:id", function(self)
  local id = tonumber(self.params.id)
  if not id then
    return { status = 400, json = { error = "Invalid user id" } }
  end
  local db, err = get_db()
  if not db then
    return { status = 500, json = { error = "DB connection error: " .. err } }
  end
  local query = string.format("SELECT id, name, email FROM users WHERE id = %d", id)
  local res, err = db:query(query)
  if not res then
    return { status = 500, json = { error = "Query error: " .. err } }
  end
  db:keepalive()
  if #res == 0 then
    return { status = 404, json = { error = "User not found" } }
  end
  return { status = 200, json = res[1] }
end)

-- PUT /users/:id: Update a user
app:put("/users/:id", function(self)
  local id = tonumber(self.params.id)
  if not id then
    return { status = 400, json = { error = "Invalid user id" } }
  end
  local name = self.params.name
  local email = self.params.email
  if not name or not email then
    return { status = 400, json = { error = "Missing fields: name and email required" } }
  end
  local db, err = get_db()
  if not db then
    return { status = 500, json = { error = "DB connection error: " .. err } }
  end
  name = db:escape_literal(name)
  email = db:escape_literal(email)
  local query = string.format("UPDATE users SET name = %s, email = %s WHERE id = %d", name, email, id)
  local res, err = db:query(query)
  if not res then
    return { status = 500, json = { error = "Update error: " .. err } }
  end
  if res.affected_rows and res.affected_rows == 0 then
    db:keepalive()
    return { status = 404, json = { error = "User not found" } }
  end
  db:keepalive()
  return { status = 200 }
end)

-- DELETE /users/:id: Delete a user
app:delete("/users/:id", function(self)
  local id = tonumber(self.params.id)
  if not id then
    return { status = 400, json = { error = "Invalid user id" } }
  end
  local db, err = get_db()
  if not db then
    return { status = 500, json = { error = "DB connection error: " .. err } }
  end
  local query = string.format("DELETE FROM users WHERE id = %d", id)
  local res, err = db:query(query)
  if not res then
    return { status = 500, json = { error = "Delete error: " .. err } }
  end
  if res.affected_rows and res.affected_rows == 0 then
    db:keepalive()
    return { status = 404, json = { error = "User not found" } }
  end
  db:keepalive()
  return { status = 200 }
end)

return app
