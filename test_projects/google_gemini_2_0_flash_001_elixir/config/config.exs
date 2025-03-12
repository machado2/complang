
import Config

# For production-like environments, we configure things
# a little differently.
if config_env() == :prod do
  config :complang, Complang.Repo,
    ssl: true,
    url: System.get_env("DATABASE_URL"),
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

  # The secret key base is used to sign/encrypt cookies and other secrets.
  # It is required for all production applications.
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
    raise "expected the SECRET_KEY_BASE environment variable"

  config :complang, ComplangWeb.Endpoint,
    http: [port: String.to_integer(System.get_env("PORT") || "4000")],
    secret_key_base: secret_key_base
end

# Configure your database
config :complang, Complang.Repo,
  username: "testuser",
  password: System.get_env("PGPASSWORD"),
  database: "complang",
  hostname: "host.docker.internal",
  port: 5432,
  pool_size: 10

# Configure your endpoint
config :complang, ComplangWeb.Endpoint,
  render_errors: [view: ComplangWeb.ErrorView, accepts: ~w(html json)],
  pubsub_server: Complang.PubSub,
  live_view: [signing_salt: "some_salt"]

# Configures the mailer
#
# By default it uses the "send" adapter which delivers emails directly.
# If you are using Gmail, consider using Swoosh.GmailAdapter instead.
config :complang, Complang.Mailer,
  adapter: Swoosh.Adapters.Sendfile,
  api_key: "sendfile"

# Swoosh API client is needed for adapters other than Sendfile.
config :swoosh,
  api_client: Swoosh.ApiClient.Finch,
  finch: [
    default: [
      pool: false
    ]
  ]

# Configure esbuild (the bundler)
config :esbuild,
  version: "0.17.11",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configure tailwind (the CSS framework)
config :tailwind,
  version: "3.3.2",
  default: [
    args: ~w(
      -c tailwind.config.js
      -i css/app.css
      -o ../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ]
