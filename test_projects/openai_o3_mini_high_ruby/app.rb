require 'sinatra'
require 'pg'
require 'json'

set :port, 8080
set :bind, '0.0.0.0'

def db_connection
  PG.connect(
    host: 'host.docker.internal',
    port: 5432,
    dbname: 'complang',
    user: 'testuser',
    password: ENV['PGPASSWORD']
  )
end

# POST /users: create a new user
post '/users' do
  content_type :json
  request_payload = JSON.parse(request.body.read)
  name = request_payload["name"]
  email = request_payload["email"]

  conn = db_connection
  result = conn.exec_params("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", [name, email])
  conn.close

  status 201
  result[0].to_json
end

# GET /users: list all users
get '/users' do
  content_type :json
  conn = db_connection
  result = conn.exec("SELECT id, name, email FROM users")
  users = result.map { |row| row }
  conn.close
  users.to_json
end

# GET /users/:id: get a single user
get '/users/:id' do
  content_type :json
  id = params['id']
  conn = db_connection
  result = conn.exec_params("SELECT id, name, email FROM users WHERE id = $1", [id])
  conn.close

  if result.ntuples > 0
    result[0].to_json
  else
    status 404
    { error: "User not found" }.to_json
  end
end

# PUT /users/:id: update a user
put '/users/:id' do
  content_type :json
  id = params['id']
  request_payload = JSON.parse(request.body.read)
  name = request_payload["name"]
  email = request_payload["email"]

  conn = db_connection
  result = conn.exec_params("UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email", [name, email, id])
  conn.close

  if result.ntuples > 0
    result[0].to_json
  else
    status 404
    { error: "User not found" }.to_json
  end
end

# DELETE /users/:id: delete a user
delete '/users/:id' do
  content_type :json
  id = params['id']

  conn = db_connection
  result = conn.exec_params("DELETE FROM users WHERE id = $1", [id])
  conn.close

  if result.cmd_tuples > 0
    status 204
    ""
  else
    status 404
    { error: "User not found" }.to_json
  end
end
