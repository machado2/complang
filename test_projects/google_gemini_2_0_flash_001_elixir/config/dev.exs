
import Config

# For development, we disable any cache and enable
# debugging.
#
# The configuration below means no compilation caching is used during
# development, which is equivalent to the --no-compile flag.
#
# Run "mix help compile.app" to learn more about the options given.
config :complang, :compile,
  debug_Elixir.Config.get(:debug, true),
  consolidate_protocols: false

# Configure ecto repository
config :complang, Complang.Repo,
  migrations_path: "priv/repo/migrations",
  pool_size: 10,
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  hostname: "host.docker.internal",
  database: "complang",
  show_sensitive_data_on_connection_error: true

# Include a file containing development-only plugins
# config/dev.exs is loaded and included in config/runtime.exs,
# which means that dev_routes.exs only needs to be included in dev.exs.
import_if_exists "dev_routes.exs"

config :complang, ComplangWeb.Endpoint,
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  http: [port: 8080]

