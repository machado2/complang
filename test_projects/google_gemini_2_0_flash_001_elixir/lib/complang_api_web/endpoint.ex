
defmodule ComplangApiWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :complang_api

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would like to enable encryption.
  @session_options [
    store: :cookie,
    key: "_complang_api_key",
    signing_salt: "some_long_and_secret_signing_salt"
  ]

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [timeout: 45_000],
    longpoll: false

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should configure serve_static_assets after you are done setting
  # up your code deploy.
  plug Plug.Static,
    at: "/",
    from: :complang_api,
    gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Head
  plug Plug.Session, @session_options
  plug ComplangApiWeb.Router
end
