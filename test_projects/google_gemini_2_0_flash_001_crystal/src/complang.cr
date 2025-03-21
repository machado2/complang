require "kemal"
require "pg"
require "json"

DB_HOST = ENV["DB_HOST"]? || "host.docker.internal"
DB_NAME = ENV["DB_NAME"]? || "complang"
DB_USER = ENV["DB_USER"]? || "testuser"
DB_PASSWORD = ENV["PGPASSWORD"]

struct User
  property id : Int32
  property name : String
  property email : String
end

# Helper method to execute SQL queries
def query(sql, params = [] of String)
  db = PG::Connection.open(host: DB_HOST, dbname: DB_NAME, user: DB_USER, password: DB_PASSWORD)
  begin
    result = db.exec(sql, params)
    db.close unless db.finished?
    return result
  rescue PG::Error => e
    puts "Database error: #{e.message}"
    raise e # Re-raise to let Kemal handle it
  end
end

# GET /users
get "/users" do |env|
  result = nil
  users = [] of User
  begin
    result = query("SELECT id, name, email FROM users")
    users = result.map do |row|
      User.new(row["id"].to_i, row["name"].as(String), row["email"].as(String))
    end
    env.response.headers["Content-Type"] = "application/json"
    env.response.print users.to_json
  rescue => e
    env.response.status_code = 500
    env.response.print({ "error" => e.message }.to_json)
  end
  
end

# GET /users/{id}
get "/users/:id" do |env|
  result = nil
  user : User?
  begin
    id = env.params["id"].to_i
    result = query("SELECT id, name, email FROM users WHERE id = $1", [id])
    if result.ntuples > 0
      row = result[0]
      user = User.new(row["id"].to_i, row["name"].as(String), row["email"].as(String))
      env.response.headers["Content-Type"] = "application/json"
      env.response.print user.to_json
    else
      env.response.status_code = 404
      env.response.print { "error" => "User not found" }.to_json
    end
  rescue => e
    env.response.status_code = 500
    env.response.print({ "error" => e.message }.to_json)
  end
end

# POST /users
post "/users" do |env|
  result = nil
  user : User?
  begin
    data = JSON.parse(env.request.body.to_s)
    name = data["name"].as(String)
    email = data["email"].as(String)
    result = query("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", [name, email])
    row = result[0]
    user = User.new(row["id"].to_i, row["name"].as(String), row["email"].as(String))
    env.response.status_code = 201
    env.response.headers["Content-Type"] = "application/json"
    env.response.print user.to_json
  rescue JSON::ParseException => e
    env.response.status_code = 400
    env.response.print({ "error" => "Invalid JSON: #{e.message}" }.to_json)
  rescue => e
    env.response.status_code = 500
    env.response.print({ "error" => e.message }.to_json)
  end
end

# PUT /users/{id}
put "/users/:id" do |env|
  result = nil
  begin
    id = env.params["id"].to_i
    data = JSON.parse(env.request.body.to_s)
    name = data["name"].as(String)
    email = data["email"].as(String)
    result = query("UPDATE users SET name = $1, email = $2 WHERE id = $3", [name, email, id])
    if result.cmd_tuples > 0
      env.response.status_code = 204
      env.response.close
    else
      env.response.status_code = 404
      env.response.print({ "error" => "User not found" }.to_json)
    end
  rescue JSON::ParseException => e
    env.response.status_code = 400
    env.response.print({ "error" => "Invalid JSON: #{e.message}" }.to_json)
  rescue => e
    env.response.status_code = 500
    env.response.print({ "error" => e.message }.to_json)
  end
end

# DELETE /users/{id}
delete "/users/:id" do |env|
  result = nil
  begin
    id = env.params["id"].to_i
    result = query("DELETE FROM users WHERE id = $1", [id])
    if result.cmd_tuples > 0
      env.response.status_code = 204
      env.response.close
    else
      env.response.status_code = 404
      env.response.print({ "error" => "User not found" }.to_json)
    end
  rescue => e
    env.response.status_code = 500
    env.response.print({ "error" => e.message }.to_json)
  end
end

Kemal.run
