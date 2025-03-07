require 'sinatra'
require 'sinatra/json'
require 'pg'
require 'json'

set :bind, '0.0.0.0'
set :port, 8080

# Configure the PostgreSQL connection using environment variable for the password
configure do
  db_config = {
    host: 'host.docker.internal',
    port: 5432,
    dbname: 'complang',
    user: 'testuser',
    password: ENV['PGPASSWORD']
  }
  set :db, PG.connect(db_config)
end

# POST /users: Create a new user
post '/users' do
  begin
    payload = JSON.parse(request.body.read)
    name = payload['name']
    email = payload['email']
    unless name && email
      halt 400, json(message: 'Name and email are required')
    end
    result = settings.db.exec_params(
      "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
      [name, email]
    )
    status 201
    json result.first
  rescue JSON::ParserError
    halt 400, json(message: 'Invalid JSON')
  end
end

# GET /users: List all users
get '/users' do
  result = settings.db.exec("SELECT id, name, email FROM users ORDER BY id")
  users = result.map { |row| row }
  json users
end

# GET /users/:id: Get a single user by id
get '/users/:id' do
  id = params['id']
  result = settings.db.exec_params("SELECT id, name, email FROM users WHERE id = $1", [id])
  if result.ntuples == 0
    halt 404, json(message: 'User not found')
  end
  json result.first
end

# PUT /users/:id: Update an existing user
put '/users/:id' do
  id = params['id']
  begin
    payload = JSON.parse(request.body.read)
    name = payload['name']
    email = payload['email']
    unless name && email
      halt 400, json(message: 'Name and email are required')
    end
    result = settings.db.exec_params(
      "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email",
      [name, email, id]
    )
    if result.ntuples == 0
      halt 404, json(message: 'User not found')
    end
    json result.first
  rescue JSON::ParserError
    halt 400, json(message: 'Invalid JSON')
  end
end

# DELETE /users/:id: Delete a user
delete '/users/:id' do
  id = params['id']
  result = settings.db.exec_params("DELETE FROM users WHERE id = $1", [id])
  if result.cmd_tuples == 0
    halt 404, json(message: 'User not found')
  end
  status 204
end
