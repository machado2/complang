
local lapis = require("lapis")
local postgres = require("lapis.db.postgres")
local cjson = require("cjson")
local os = require("os")

local db = postgres(
  os.getenv("PGDATABASE") or "complang",
  os.getenv("PGUSER") or "testuser",
  os.getenv("PGPASSWORD") or "Saloon5-Moody-Observing",
  os.getenv("PGHOST") or "host.docker.internal",               -- Changed localhost to host.docker.internal
  os.getenv("PGPORT") or 5432
)

local app = lapis.Application()

local function user_to_json(user)
  return {
    id = user.id,
    name = user.name,
    email = user.email
  }
end

app:post("/users", function(self)
  local body = cjson.decode(self.req.body)
  local name = body.name
  local email = body.email

  if not name or not email then
    self:status(400)
    return "Name and email are required"
  end

  local user = db:insert("users", {name = name, email = email})
  self:status(201)
  self:header("Content-Type", "application/json")
  return cjson.encode(user_to_json(user))
end)

app:get("/users", function(self)
  local users = db:query("SELECT * FROM users")
  local result = {}
  for i, user in ipairs(users) do
    result[i] = user_to_json(user)
  end
  self:header("Content-Type", "application/json")
  return cjson.encode(result)
end)

app:get("/users/:id", function(self)
  local id = tonumber(self.params.id)
  local user = db:query("SELECT * FROM users WHERE id = $1", id)[1]

  if not user then
    self:status(404)
    return "User not found"
  end

  self:header("Content-Type", "application/json")
  return cjson.encode(user_to_json(user))
end)

app:put("/users/:id", function(self)
  local id = tonumber(self.params.id)
  local body = cjson.decode(self.req.body)
  local name = body.name
  local email = body.email

  if not name or not email then
    self:status(400)
    return "Name and email are required"
  end

  local updated = db:update("users", {name = name, email = email}, {id = id})

  if updated == 0 then
    self:status(404)
    return "User not found"
  end
  self:status(204)
end)

app:delete("/users/:id", function(self)
  local id = tonumber(self.params.id)
  local deleted = db:delete("users", {id = id})

  if deleted == 0 then
    self:status(404)
    return "User not found"
  end

  self:status(204)
end)

return app
