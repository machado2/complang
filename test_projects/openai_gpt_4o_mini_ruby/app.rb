require 'sinatra'
require 'pg'
require 'json'

# Database connection
DB = PG.connect(
  host: 'host.docker.internal',
  port: 5432,
  dbname: 'complang',
  user: 'testuser',
  password: ENV['PGPASSWORD']
)

# Routes

# Create a user
post '/users' do
  data = JSON.parse(request.body.read)
  result = DB.exec_params('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email;', [data['name'], data['email']])
  user = result[0]
  status 201
  user.to_json
end

# Get all users
get '/users' do
  result = DB.exec('SELECT * FROM users;')
  users = result.map { |user| user }
  users.to_json
end

# Get a single user by id
get '/users/:id' do
  result = DB.exec_params('SELECT * FROM users WHERE id = $1;', [params['id']])
  user = result.first
  if user
    user.to_json
  else
    halt 404, { error: 'User not found' }.to_json
  end
end

# Update a user by id
put '/users/:id' do
  data = JSON.parse(request.body.read)
  result = DB.exec_params('UPDATE users SET name = $1, email = $2 WHERE id = $3;', [data['name'], data['email'], params['id']])
  if result.cmd_tuples > 0
    status 200
  else
    halt 404, { error: 'User not found' }.to_json
  end
end

# Delete a user by id
delete '/users/:id' do
  result = DB.exec_params('DELETE FROM users WHERE id = $1;', [params['id']])
  if result.cmd_tuples > 0
    status 204
  else
    halt 404, { error: 'User not found' }.to_json
  end
end