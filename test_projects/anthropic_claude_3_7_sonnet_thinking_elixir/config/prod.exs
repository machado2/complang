
import Config

# For production, don't forget to configure the url host
# to something meaningful, Phoenix uses this information
# when generating URLs.
config :users_api, UsersApiWeb.Endpoint, 
  url: [host: "localhost", port: 8080],
  cache_static_manifest: "priv/static/cache_manifest.json"

# Do not print debug messages in production
config :logger, level: :info

# Runtime production configuration, including reading
# of environment variables and loading of .env files
# is done on config/runtime.exs.
