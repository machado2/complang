
defmodule Complang.Application do
  use Application

  @impl true
  def start(_type, _args) do
    IO.puts("Starting the application")

    children = [
      # Start the endpoint when the application starts
      ComplangWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: Complang.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
