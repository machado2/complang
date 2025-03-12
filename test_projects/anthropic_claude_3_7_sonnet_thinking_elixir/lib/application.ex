
defmodule UsersApi.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      UsersApi.Repo,
      {Plug.Cowboy, scheme: :http, plug: UsersApi.Router, options: [port: 8080]}
    ]

    opts = [strategy: :one_for_one, name: UsersApi.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
