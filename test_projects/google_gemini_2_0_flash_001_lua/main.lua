
local lapis = require("lapis")
local postgres = require("lapis.db.postgres")
local json = require("json")

local db = postgres("postgres://testuser:$PGPASSWORD@host.docker.internal:5432/complang")

local app = lapis.Application()

app:match("/users", {methods = {"GET", "POST"}}, function()
  if request:method() == "GET" then
    local users = db:query("SELECT id, name, email FROM users")
    return {
      text = json.encode(users),
      headers = {["Content-Type"] = "application/json"}
    }
  elseif request:method() == "POST" then
    local data = json.decode(request:body())
    local name = data.name
    local email = data.email
    local result = db:query("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", name, email)[1]
    response.status = 201
    return {
      text = json.encode(result),
      headers = {["Content-Type"] = "application/json"}
    }
  end
end)

app:match("/users/:id", {methods = {"GET", "PUT", "DELETE"}}, function()
  local id = tonumber(self.params.id)
  if request:method() == "GET" then
    local user = db:query("SELECT id, name, email FROM users WHERE id = $1", id)[1]
    if user then
      return {
        text = json.encode(user),
        headers = {["Content-Type"] = "application/json"}
      }
    else
      response.status = 404
      return "User not found"
    end
  elseif request:method() == "PUT" then
    local data = json.decode(request:body())
    local name = data.name
    local email = data.email
    local result = db:execute("UPDATE users SET name = $1, email = $2 WHERE id = $3", name, email, id)
    if result > 0 then
       response.status = 204
       return ""
    else
      response.status = 404
      return "User not found"
    end
  elseif request:method() == "DELETE" then
    local result = db:execute("DELETE FROM users WHERE id = $1", id)
    if result > 0 then
      response.status = 204
      return ""
    else
      response.status = 404
      return "User not found"
    end
  end
end)

return app
