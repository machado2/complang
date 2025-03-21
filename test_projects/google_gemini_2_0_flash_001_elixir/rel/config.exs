
use Mix.Config

# This file is specific to the release task.
#
# Here we configure the dev and prod environments.
#
# Do not define configuration here that might be
# shared between multiple environments.

# For production, release the BEAM files only.
config :complang, :compile,
  force: false,
  debug_ Elixir.Config.get(:debug, false),
  consolidate_protocols: true

# Configure your endpoint for production
config :complang, ComplangWeb.Endpoint,
  load_from_system_env: true,
  server: true,
  root: "/",
  version: Mix.Project.config()[:version],
  secret_key_base: System.fetch_env!("SECRET_KEY_BASE")

