
import Config

config :app, App.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "complang",
  hostname: "host.docker.internal",
  port: 5432,
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  pool_size: 10

config :app, ecto_repos: [App.Repo]
