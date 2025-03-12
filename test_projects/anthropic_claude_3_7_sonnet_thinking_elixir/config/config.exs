
import Config

config :users_api,
  ecto_repos: [UsersApi.Repo]

config :users_api, UsersApi.Repo,
  database: "complang",
  username: "testuser",
  password: System.get_env("PGPASSWORD") || "Saloon5-Moody-Observing",
  hostname: "host.docker.internal",
  port: 5432,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

config :logger, level: :info
