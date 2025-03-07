defmodule ComplangAPI.Application do
  use Application

  def start(_type, _args) do
    children = [
      ComplangAPI.Repo,
      {Plug.Cowboy, scheme: :http, plug: ComplangAPI.Router, options: [port: 8080]}
    ]

    opts = [strategy: :one_for_one, name: ComplangAPI.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
