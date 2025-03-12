
require "kemal"
require "pg"
require "db"
require "json"

# Initialize database connection
module DB_UTIL
  class_property connection : DB::Database = DB.open(
    "postgres://testuser:#{ENV["PGPASSWORD"]}@host.docker.internal:5432/complang"
  )
end

# User model
class User
  include JSON::Serializable

  property id : Int32?
  property name : String
  property email : String

  def initialize(@name : String, @email : String, @id : Int32? = nil)
  end
end

# Create a user
post "/users" do |env|
  begin
    user_data = env.params.json
    name = user_data["name"].as(String)
    email = user_data["email"].as(String)

    id = nil
    DB_UTIL.connection.query("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", name, email) do |rs|
      if rs.move_next
        id = rs.read(Int32)
      end
    end

    user = User.new(name: name, email: email, id: id)

    env.response.status_code = 201
    env.response.content_type = "application/json"
    user.to_json
  rescue e
    env.response.status_code = 400
    {"error": e.message}.to_json
  end
end

# Get all users
get "/users" do |env|
  users = [] of User
  DB_UTIL.connection.query("SELECT id, name, email FROM users") do |rs|
    while rs.move_next
      id = rs.read(Int32)
      name = rs.read(String)
      email = rs.read(String)
      users << User.new(name: name, email: email, id: id)
    end
  end

  env.response.content_type = "application/json"
  users.to_json
end

# Get a user by ID
get "/users/:id" do |env|
  id = env.params.url["id"].to_i
  user = nil

  DB_UTIL.connection.query("SELECT id, name, email FROM users WHERE id = $1", id) do |rs|
    if rs.move_next
      user_id = rs.read(Int32)
      name = rs.read(String)
      email = rs.read(String)
      user = User.new(name: name, email: email, id: user_id)
    end
  end

  if user
    env.response.content_type = "application/json"
    user.to_json
  else
    env.response.status_code = 404
    {"error": "User not found"}.to_json
  end
end

# Update a user
put "/users/:id" do |env|
  id = env.params.url["id"].to_i
  user_data = env.params.json
  name = user_data["name"].as(String)
  email = user_data["email"].as(String)

  result = DB_UTIL.connection.exec("UPDATE users SET name = $1, email = $2 WHERE id = $3", name, email, id)

  if result.rows_affected > 0
    env.response.status_code = 204
    ""
  else
    env.response.status_code = 404
    {"error": "User not found"}.to_json
  end
end

# Delete a user
delete "/users/:id" do |env|
  id = env.params.url["id"].to_i

  result = DB_UTIL.connection.exec("DELETE FROM users WHERE id = $1", id)

  if result.rows_affected > 0
    env.response.status_code = 204
    ""
  else
    env.response.status_code = 404
    {"error": "User not found"}.to_json
  end
end

# Start the server
Kemal.config.port = 8080
Kemal.run
