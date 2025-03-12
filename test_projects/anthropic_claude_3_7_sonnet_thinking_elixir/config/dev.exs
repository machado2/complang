
import Config

# Configure your database
config :users_api, UsersApi.Repo,
  username: "testuser",
  password: System.get_env("PGPASSWORD") || "Saloon5-Moody-Observing",
  hostname: "host.docker.internal",
  database: "complang",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# For development, we disable any cache and enable
# debugging and code reloading.
config :users_api, UsersApiWeb.Endpoint,
  # Binding to loopback ipv4 address prevents access from other machines.
  http: [ip: {0, 0, 0, 0}, port: 8080],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "vKBzWGHkUFf8iBNR1/a2pYSZ1JVfKB9KX+odeD0/pMoR6dyLF4AaZN8e/kBUTd1Q",
  watchers: []

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime
