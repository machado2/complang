require 'sinatra'
require 'json'
require 'pg'

class UserAPI < Sinatra::Base
  configure do
    set :port, 8080
  end

  DB = PG.connect(
    host: 'host.docker.internal',
    port: 5432,
    dbname: 'complang',
    user: 'testuser',
    password: ENV['PGPASSWORD']
  )

  post '/users' do
    data = JSON.parse(request.body.read)
    name = data['name']
    email = data['email']
    result = DB.exec_params('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id;', [name, email])
    user_id = result[0]['id']
    status 201
    { id: user_id, name: name, email: email }.to_json
  end

  get '/users' do
    result = DB.exec('SELECT * FROM users;')
    users = result.map { |user| { id: user['id'], name: user['name'], email: user['email'] } }
    status 200
    users.to_json
  end

  get '/users/:id' do
    result = DB.exec_params('SELECT * FROM users WHERE id = $1;', [params['id']])
    if result.ntuples > 0
      user = result[0]
      status 200
      { id: user['id'], name: user['name'], email: user['email'] }.to_json
    else
      status 404
    end
  end

  put '/users/:id' do
    data = JSON.parse(request.body.read)
    name = data['name']
    email = data['email']
    result = DB.exec_params('UPDATE users SET name = $1, email = $2 WHERE id = $3;', [name, email, params['id']])
    if result.cmd_tuples > 0
      status 204
    else
      status 404
    end
  end

  delete '/users/:id' do
    result = DB.exec_params('DELETE FROM users WHERE id = $1;', [params['id']])
    if result.cmd_tuples > 0
      status 204
    else
      status 404
    end
  end

  run! if app_file == $0
end
