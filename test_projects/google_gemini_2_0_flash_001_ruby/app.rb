# app.rb
require 'sinatra'
require 'sinatra/reloader'
require 'pg'
require 'json'

# Database connection
configure do
  db_url = ENV['DATABASE_URL'] || 'postgres://testuser:$PGPASSWORD@host.docker.internal:5432/complang'
  uri = URI.parse(db_url)
  set :db_params, {
    host: uri.host,
    port: uri.port,
    dbname: uri.path[1..-1],
    user: uri.user,
    password: uri.password
  }
end

helpers do
  def db_connection
    PG.connect(settings.db_params)
  end

  def json_params
    begin
      JSON.parse(request.body.read)
    rescue
      halt 400, { message: 'Invalid JSON' }.to_json
    end
  end
end

# before do
#   content_type :json
# end

# Routes

# POST /users
post '/users' do
  content_type :json
  begin
    params = json_params
    db_connection do |conn|
      result = conn.exec_params('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email', [params['name'], params['email']])
      status 201
      result.first.to_json
    end
  rescue PG::Error => e
    halt 500, { message: "Database error: #{e.message}" }.to_json
  end
end

# GET /users
get '/users' do
  content_type :json
  begin
    db_connection do |conn|
      result = conn.exec('SELECT id, name, email FROM users')
      result.map { |row| row }.to_json
    end
  rescue PG::Error => e
    halt 500, { message: "Database error: #{e.message}" }.to_json
  end
end

# GET /users/:id
get '/users/:id' do
  content_type :json
  begin
    db_connection do |conn|
      result = conn.exec_params('SELECT id, name, email FROM users WHERE id = $1', [params[:id]])
      if result.ntuples > 0
        result.first.to_json
      else
        status 404
        { message: 'User not found' }.to_json
      end
    end
  rescue PG::Error => e
    halt 500, { message: "Database error: #{e.message}" }.to_json
  end
end

# PUT /users/:id
put '/users/:id' do
  begin
    params = json_params
    db_connection do |conn|
      result = conn.exec_params('UPDATE users SET name = $1, email = $2 WHERE id = $3', [params['name'], params['email'], params[:id]])
      if result.cmd_tuples > 0
          status 204
      else
        status 404
        { message: 'User not found' }.to_json
      end
    end
  rescue PG::Error => e
    halt 500, { message: "Database error: #{e.message}" }.to_json
  end
end

# DELETE /users/:id
delete '/users/:id' do
  begin
    db_connection do |conn|
      result = conn.exec_params('DELETE FROM users WHERE id = $1', [params[:id]])
      if result.cmd_tuples > 0
        status 204
      else
        status 404
        { message: 'User not found' }.to_json
      end
    end
  rescue PG::Error => e
    halt 500, { message: "Database error: #{e.message}" }.to_json
  end
end
