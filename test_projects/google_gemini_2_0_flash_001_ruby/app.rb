
require 'sinatra'
require 'sinatra/activerecord'
require 'pg'
require 'dotenv/load'
require 'json'

# Set up database connection
 Dotenv.load

set :database, {
  adapter: 'postgresql',
  host: 'host.docker.internal',
  port: 5432,
  database: 'complang',
  user: 'testuser',
  password: ENV['PGPASSWORD']
}

ActiveRecord::Base.establish_connection(settings.database)

# Define User model
class User < ActiveRecord::Base
  validates :name, presence: true
  validates :email, presence: true
end

# Create users table if it doesn't exist
if !ActiveRecord::Base.connection.table_exists?(:users)
  ActiveRecord::Base.connection.create_table :users do |t|
    t.string :name
    t.string :email
  end
end

# API endpoints
before do
  content_type :json
end

# POST /users
post '/users' do
  user = User.new(name: params[:name], email: params[:email])
  if user.save
    status 201
    user.to_json
  else
    status 400
    user.errors.to_json
  end
end

# GET /users
get '/users' do
  User.all.to_json
end

# GET /users/:id
get '/users/:id' do
  user = User.find_by(id: params[:id])
  if user
    user.to_json
  else
    status 404
    { message: 'User not found' }.to_json
  end
end

# PUT /users/:id
put '/users/:id' do
  user = User.find_by(id: params[:id])
  if user
    if user.update(name: params[:name], email: params[:email])
      status 200
      user.to_json
    else
      status 400
      user.errors.to_json
    end
  else
    status 404
    { message: 'User not found' }.to_json
  end
end

# DELETE /users/:id
delete '/users/:id' do
  user = User.find_by(id: params[:id])
  if user
    user.destroy
    status 204
  else
    status 404
    { message: 'User not found' }.to_json
  end
end
