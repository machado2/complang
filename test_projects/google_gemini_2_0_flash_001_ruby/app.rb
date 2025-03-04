require 'sinatra'
require 'sinatra/contrib'
require 'pg'
require 'json'

register Sinatra::Contrib

configure do
  set :bind, '0.0.0.0'
  set :port, 8080
end

get '/users' do
  'Hello, users!'
end

post '/users' do
  status 201
  'User created!'
end