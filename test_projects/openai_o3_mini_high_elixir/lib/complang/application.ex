
defmodule Complang.Application do
  use Application

  def start(_type, _args) do
    children = [
      Complang.Repo,
      {Plug.Cowboy, scheme: :http, plug: Complang.Router, options: [ip: {0,0,0,0}, port: Application.get_env(:complang, :port)]}
    ]

    opts = [strategy: :one_for_one, name: Complang.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
