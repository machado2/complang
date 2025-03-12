require "kemal"
require "pg"
require "clear_env"

ClearEnv.load

DB_HOST     = ENV["DB_HOST"]     || "host.docker.internal"
DB_PORT     = ENV["DB_PORT"]     || "5432"
DB_NAME     = ENV["DB_NAME"]     || "complang"
DB_USER     = ENV["DB_USER"]     || "testuser"
DB_PASSWORD = ENV["PGPASSWORD"]? || "Saloon5-Moody-Observing"


struct User
  property id    : Int32
  property name  : String
  property email : String

  def initialize(@id : Int32, @name : String, @email : String)
  end

  def to_json(json : JSON::Builder)
    json.object do
      json.field "id",    id
      json.field "name",  name
      json.field "email", email
    end
  end
end


def with_db_connection
  PG.connect(host: DB_HOST, port: DB_PORT.to_i, dbname: DB_NAME, user: DB_USER, password: DB_PASSWORD) do |db|
    yield db
  end
end


Kemal.before do |env|
    env.response.headers["Content-Type"] = "application/json"
end


post "/users" do |env|
  begin
    json = JSON.parse env.request.body.to_s
    name  = json["name"].as_s
    email = json["email"].as_s

    with_db_connection do |db|
      result = db.exec "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", [name, email]
      user = User.new(result[0]["id"].as(Int32), result[0]["name"].as_s, result[0]["email"].as_s)
      env.response.status_code = 201
      user.to_json env.response.body
    end
  rescue ex : JSON::ParseException
        env.response.status_code = 400
        env.response.body = { "error": "Invalid JSON" }.to_json
    rescue ex : PG::Error
        env.response.status_code = 500
        env.response.body = { "error": "Database error: #{ex.message}" }.to_json
  end
end


get "/users" do |env|
  begin
    users = [] of User
    with_db_connection do |db|
      result = db.exec "SELECT id, name, email FROM users"
      result.each do |row|
        users << User.new(row["id"].as(Int32), row["name"].as_s, row["email"].as_s)
      end
    end

    JSON.build do |json|
      json.array do
        users.each do |user|
          user.to_json json
        end
      end
    end
  rescue ex : PG::Error
        env.response.status_code = 500
        env.response.body = { "error": "Database error: #{ex.message}" }.to_json
  end
end


get "/users/:id" do |env|
  begin
    id = env.params["id"].to_i

    with_db_connection do |db|
      result = db.exec "SELECT id, name, email FROM users WHERE id = $1", [id]
      if result.ntuples > 0
        user = User.new(result[0]["id"].as(Int32), result[0]["name"].as_s, result[0]["email"].as_s)
        user.to_json env.response.body
      else
        env.response.status_code = 404
      end
    end
  rescue ex : PG::Error
        env.response.status_code = 500
        env.response.body = { "error": "Database error: #{ex.message}" }.to_json
  end
end


put "/users/:id" do |env|
  begin
    id = env.params["id"].to_i
    json = JSON.parse env.request.body.to_s
    name  = json["name"].as_s
    email = json["email"].as_s

    with_db_connection do |db|
      result = db.exec "UPDATE users SET name = $1, email = $2 WHERE id = $3", [name, email, id]
      if result.cmd_tuples > 0
          env.response.status_code = 204
      else
        env.response.status_code = 404
      end
    end
    env.response.status_code = 204
  rescue ex : JSON::ParseException
        env.response.status_code = 400
        env.response.body = { "error": "Invalid JSON" }.to_json
    rescue ex : PG::Error
        env.response.status_code = 500
        env.response.body = { "error": "Database error: #{ex.message}" }.to_json
  end
end


delete "/users/:id" do |env|
  begin
    id = env.params["id"].to_i

    with_db_connection do |db|
      result = db.exec "DELETE FROM users WHERE id = $1", [id]
      if result.cmd_tuples > 0
        env.response.status_code = 204
      else
        env.response.status_code = 404
      end
    end
  rescue ex : PG::Error
        env.response.status_code = 500
        env.response.body = { "error": "Database error: #{ex.message}" }.to_json
  end
end

Kemal.run
