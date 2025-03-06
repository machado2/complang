require "sinatra"
require "sinatra/json"
require "pg"
require "dotenv"

Dotenv.load

set :bind, '0.0.0.0'
set :port, 8080

before do
  content_type :json
end

helpers do
  def db_connection
    begin
      connection = PG.connect(host: "host.docker.internal", port: 5432, dbname: "complang", user: "testuser", password: ENV["PGPASSWORD"])
      yield connection
    ensure
      connection&.close
    end
  end

  def user_params
    JSON.parse(request.body.read)
  rescue
    halt 400, { message: "Invalid JSON" }.to_json
  end

  def user_hash(user)
    { id: user["id"].to_i, name: user["name"], email: user["email"] }
  end
end

# POST /users: Creates a user
post '/users' do
  params = user_params
  name = params["name"]
  email = params["email"]

  if !name || name.empty? || !email || email.empty?
    halt 400, { message: "Name and email are required" }.to_json
  end

  db_connection do |conn|
    result = conn.exec_params("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", [name, email])
    user = result.first
    status 201
    json user_hash(user)
  end
end

# GET /users: Returns a list of all users
get '/users' do
  users = []
  db_connection do |conn|
    result = conn.exec("SELECT id, name, email FROM users")
    result.each do |user|
      users << user_hash(user)
    end
  end
  json users
end

# GET /users/{id}: Returns a single user
get '/users/:id' do |id|
  db_connection do |conn|
    result = conn.exec_params("SELECT id, name, email FROM users WHERE id = $1", [id])
    user = result.first
    if user
      json user_hash(user)
    else
      status 404
    end
  end
end

# PUT /users/{id}: Updates a user
put '/users/:id' do |id|
  params = user_params
  name = params["name"]
  email = params["email"]

  if !name || name.empty? || !email || email.empty?
    halt 400, { message: "Name and email are required" }.to_json
  end

  db_connection do |conn|
    result = conn.exec_params("UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email", [name, email, id])
    user = result.first
    if user
      status 200
      json user_hash(user)
    else
      status 404
    end
  end
end

# DELETE /users/{id}: Deletes a user
delete '/users/:id' do |id|
  db_connection do |conn|
    result = conn.exec_params("DELETE FROM users WHERE id = $1 RETURNING id", [id])
    if result.values.length > 0
      status 204
    else
      status 404
    end
  end
end


if __FILE__ == $0
  Sinatra::Application.run!
end