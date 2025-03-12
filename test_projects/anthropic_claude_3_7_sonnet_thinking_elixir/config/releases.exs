
import Config

config :users_api, UsersApi.Repo,
  database: "complang",
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  hostname: "host.docker.internal",
  port: 5432,
  pool_size: 10
