
use Mix.Config

config :complang, Complang.Repo,
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  database: "complang",
  hostname: "host.docker.internal",
  port: 5432,
  pool_size: 10

config :complang, ComplangWeb.Endpoint,
  http: [port: 8080],
  secret_key_base: "some_secret_key"
