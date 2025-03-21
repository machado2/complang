
import Config

database_url =
  System.get_env("DATABASE_URL") ||
    raise """
    environment variable DATABASE_URL is missing.
    For example: ecto://USER:PASS@HOST/DATABASE
    """

config :complang, Complang.Repo,
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  database: "complang",
  hostname: "host.docker.internal",
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
  socket_options: [:inet6],
  url: database_url,
  pool: Ecto.Adapters.SQL.Postgres.Pool

config :complang, ComplangWeb.Endpoint,
  load_from_system_env: true,
  server: true,
  root: "/",
  version: Mix.Project.config()[:version],
  secret_key_base: System.fetch_env!("SECRET_KEY_BASE"),
  port: 8080

# On production, only log what is really necessary.
config :logger, level: :warn

