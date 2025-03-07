use Mix.Config

config :complang_api, ComplangAPI.Repo,
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  database: "complang",
  hostname: "host.docker.internal",
  port: 5432,
  pool_size: 10

config :complang_api, ecto_repos: [ComplangAPI.Repo]

config :logger, level: :info
