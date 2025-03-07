defmodule ComplangWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :complang

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  plug Plug.Session,
    store: :cookie,
    key: "_complang_key",
    signing_salt: "some_salt"

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [timeout: :infinity],
    longpoll: false

  plug Plug.Static,
    at: "/",
    from: :complang_web,
    gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)

  # Code reloading can be explicitly enabled under the
  # :code_reloader config of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Head
  plug Plug.Session, @session_options
  plug :accepts, ["json"]
  plug :fetch_session
  plug :fetch_live_flash
  plug :put_root_layout, false
  plug :protect_from_forgery
  plug :put_secure_browser_headers

  plug(Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Jason)

  plug(Plug.MethodOverride)
  plug(Plug.Head)
  plug(ComplangWeb.Router)
end
