
defmodule App do
  use Application

  def start(_type, _args) do
    children = [
      App.Repo,
      {Plug.Cowboy, scheme: :http, plug: App.Router, options: [port: 8080]}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
