
use Mix.Config

config :complang_api, ComplangApi.Repo,
  database: "complang",
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  hostname: "host.docker.internal",
  port: 5432,
  pool_size: 10

config :complang_api, ComplangApiWeb.Endpoint,
  render_errors: [view: ComplangApiWeb.ChangesetView, accepts: ~w(json)],
  pubsub_server: ComplangApi.PubSub,
  live_view: [signing_salt: "some_signing_salt"],
  http: [port: 8080]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason
