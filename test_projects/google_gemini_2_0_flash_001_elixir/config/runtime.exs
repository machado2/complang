import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  config :complang, Complang.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")
end

config :complang_web, ComplangWeb.Endpoint,
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  render_errors: [view: ComplangWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Complang.PubSub

# Configures Swoosh API client
config :swoosh, :api_client, Swoosh.Adapters.Local

