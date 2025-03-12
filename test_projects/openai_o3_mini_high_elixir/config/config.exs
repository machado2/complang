
import Config

config :complang, Complang.Repo,
  username: "testuser",
  password: System.get_env("PGPASSWORD") || "default",
  database: "complang",
  hostname: "host.docker.internal",
  port: 5432,
  pool_size: 10

config :complang,
  ecto_repos: [Complang.Repo],
  port: 8080

config :logger, level: :info
