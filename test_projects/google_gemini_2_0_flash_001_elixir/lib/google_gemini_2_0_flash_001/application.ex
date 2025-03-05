defmodule GoogleGemini20Flash001.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Plug.Cowboy, scheme: :http, plug: GoogleGemini20Flash001.Router, options: [port: 8080]}
    ]

    opts = [strategy: :one_for_one, name: GoogleGemini20Flash001.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def config_change(changed, new, removed) do
    :ok
  end
end
