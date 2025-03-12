require 'sinatra'
require 'sinatra/json'
require 'pg'
require 'json'

set :bind, '0.0.0.0'
set :port, 8080

# Database connection
def db_connection
  @conn ||= PG.connect(
    host: 'host.docker.internal',
    dbname: 'complang',
    user: 'testuser',
    password: ENV['PGPASSWORD']
  )
end

# Helper method to find a user by ID
def find_user(id)
  result = db_connection.exec_params('SELECT * FROM users WHERE id = $1', [id])
  return nil if result.ntuples.zero?
  
  user = result[0]
  { 'id' => user['id'].to_i, 'name' => user['name'], 'email' => user['email'] }
end

# Create a new user
post '/users' do
  begin
    data = JSON.parse(request.body.read)
    
    # Validate required fields
    halt 400, json(error: 'Name and email are required') unless data['name'] && data['email']
    
    # Insert user into database
    result = db_connection.exec_params(
      'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email',
      [data['name'], data['email']]
    )
    
    # Return the created user
    user = result[0]
    status 201
    json(id: user['id'].to_i, name: user['name'], email: user['email'])
  rescue JSON::ParserError
    halt 400, json(error: 'Invalid JSON')
  end
end

# Get all users
get '/users' do
  result = db_connection.exec('SELECT * FROM users')
  
  users = result.map do |row|
    { 'id' => row['id'].to_i, 'name' => row['name'], 'email' => row['email'] }
  end
  
  json users
end

# Get a specific user
get '/users/:id' do
  user = find_user(params[:id])
  halt 404, json(error: 'User not found') unless user
  
  json user
end

# Update a user
put '/users/:id' do
  begin
    # Check if user exists
    halt 404, json(error: 'User not found') unless find_user(params[:id])
    
    data = JSON.parse(request.body.read)
    
    # Validate required fields
    halt 400, json(error: 'Name and email are required') unless data['name'] && data['email']
    
    # Update user in database
    db_connection.exec_params(
      'UPDATE users SET name = $1, email = $2 WHERE id = $3',
      [data['name'], data['email'], params[:id]]
    )
    
    status 200
    json(success: true)
  rescue JSON::ParserError
    halt 400, json(error: 'Invalid JSON')
  end
end

# Delete a user
delete '/users/:id' do
  # Check if user exists
  halt 404, json(error: 'User not found') unless find_user(params[:id])
  
  # Delete user from database
  db_connection.exec_params('DELETE FROM users WHERE id = $1', [params[:id]])
  
  status 200
  json(success: true)
end
