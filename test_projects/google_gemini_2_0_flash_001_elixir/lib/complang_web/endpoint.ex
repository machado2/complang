
defmodule ComplangWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :complang

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: {:peer_data, :peer_cid}],
    longpoll: false

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should configure your server accordingly.
  plug Plug.Static,
    at: "/",
    from: :complang,
    gzip: false,
    only: ~w(assets fonts images favicon.ico robots.txt)

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
  end

  plug Phoenix.Endpoint.AddRequestId
  plug Phoenix.Endpoint.CatchParams
  plug Phoenix.Endpoint.ReadCookies
  plug Phoenix.Session, @session_options
  plug ComplangWeb.Plugs.Locale, ["json"]
  plug Phoenix.Endpoint.VerifyCSRF
  plug Phoenix.LiveFlash
  plug Plug.MethodOverride
  plug Plug.Head
  plug Phoenix.Router

  defp put_root_layout(conn, _) do
    Phoenix.Controller.put_layout(conn, {ComplangWeb.LayoutView, :app})
  end
end
