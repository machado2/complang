
require "kemal"
require "json"
require "pg"

# Connect to PostgreSQL using the provided credentials and environment variable for PGPASSWORD
db = PG.connect(
  host: "host.docker.internal",
  port: 5432,
  user: "testuser",
  password: ENV["PGPASSWORD"]?,
  dbname: "complang"
)

# Helper function to safely parse JSON body without using the "as" cast operator
def parse_json(body : String) : Hash(String, JSON::Any)
  begin
    parsed = JSON.parse(body)
    if parsed.is_a?(Hash(String, JSON::Any))
      parsed
    else
      {} of String => JSON::Any
    end
  rescue
    {} of String => JSON::Any
  end
end

# POST /users: Create a new user
post "/users" do |env|
  body = env.request.body.gets || ""
  data = parse_json(body)
  name = data["name"].as_s
  email = data["email"].as_s

  result = db.exec("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", [name, email])
  new_id = result.first["id"].to_i
  user = { "id" => new_id, "name" => name, "email" => email }
  env.response.content_type = "application/json"
  env.response.status = 201
  user.to_json
end

# GET /users: Retrieve a list of all users
get "/users" do |env|
  users = [] of Hash(String, JSON::Any)
  result = db.exec("SELECT * FROM users")
  result.each do |row|
    users << { "id" => row["id"].to_i, "name" => row["name"], "email" => row["email"] }
  end
  env.response.content_type = "application/json"
  users.to_json
end

# GET /users/:id: Fetch a specific user by id
get "/users/:id" do |env|
  id = env.params.url["id"].to_i
  result = db.exec("SELECT * FROM users WHERE id = $1 LIMIT 1", [id])
  if row = result.first
    user = { "id" => row["id"].to_i, "name" => row["name"], "email" => row["email"] }
    env.response.content_type = "application/json"
    user.to_json
  else
    env.response.status = 404
    { "error" => "User not found" }.to_json
  end
end

# PUT /users/:id: Update an existing user
put "/users/:id" do |env|
  id = env.params.url["id"].to_i
  body = env.request.body.gets || ""
  data = parse_json(body)
  name = data["name"].as_s
  email = data["email"].as_s

  result = db.exec("UPDATE users SET name = $1, email = $2 WHERE id = $3", [name, email, id])
  if result.affected_rows > 0
    env.response.content_type = "application/json"
    { "message" => "User updated", "id" => id, "name" => name, "email" => email }.to_json
  else
    env.response.status = 404
    { "error" => "User not found" }.to_json
  end
end

# DELETE /users/:id: Delete a user
delete "/users/:id" do |env|
  id = env.params.url["id"].to_i
  result = db.exec("DELETE FROM users WHERE id = $1", [id])
  if result.affected_rows > 0
    env.response.content_type = "application/json"
    { "message" => "User deleted" }.to_json
  else
    env.response.status = 404
    { "error" => "User not found" }.to_json
  end
end

# Set the server to listen on port 8080 and start the application
Kemal.config.port = 8080
Kemal.run
