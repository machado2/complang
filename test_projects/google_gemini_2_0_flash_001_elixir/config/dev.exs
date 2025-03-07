use Mix.Config

config :complang, Complang.Repo,
  database: "complang",
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  hostname: "host.docker.internal",
  port: 5432,
  pool_size: 10

config :complang_web, ComplangWeb.Endpoint,
  http: [port: 8080],
  debug: true,
  code_reloader: true,
  check_origin: false,
  watchers: []
