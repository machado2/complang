
defmodule Myapp.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    # Ensure the database is created and migrated before starting
    # {:ok, _} = Ecto.Migrator.run(Complang.Repo, :up, all: true) The migrate command should be run outside of this application code.

    children = [
      # Start the Ecto repository
     Myapp.Repo,
      # Start the Telemetry supervisor
      MyappWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Myapp.PubSub},
      # Start the Endpoint (http/https)
      MyappWeb.Endpoint
      # Start a worker by calling: Complang.Worker.start_link(arg)
      # {Complang.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Myapp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    MyappWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
