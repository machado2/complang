
defmodule ComplangApiWeb.Telemetry do
  use Supervisor
  import Telemetry.Metrics

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, :ok, arg)
  end

  @impl true
  def init(:ok) do
    children = [
      # Telemetry poller will execute the given period measurements
      #
      # See the Telemetry module for instructions on adding measurements
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
      # Add reporters as children of your supervisor
      # ie: {Telemetry.Metrics.ConsoleReporter, reporter_options}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp periodic_measurements do
    [
      # Phoenix-related metrics
      summary("phoenix.endpoint.stop.duration",
        unit: {:native, :millisecond}
      ),
      summary("phoenix.router_dispatch.stop.duration",
        router: :complang_api_web,
        unit: {:native, :millisecond}
      ),
      counter("phoenix.endpoint.http.request.total")
      # Ecto-related metrics
      # summary("ecto.repo.query.duration",
      #   unit: {:native, :millisecond},
      #   repo: ComplangApi.Repo
      # ),
      # counter("ecto.repo.query.total", repo: ComplangApi.Repo)
    ]
  end
end

defmodule ComplangApiWeb.Endpoint do
  use Phoenix.Endpoint

  socket "/socket", ComplangApiWeb.UserSocket,
    websocket: true,
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
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]


  plug Plug.Head
  plug Plug.Session, @session_options
  plug ComplangApiWeb.Router
end
