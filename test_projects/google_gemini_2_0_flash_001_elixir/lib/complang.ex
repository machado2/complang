defmodule Complang do
  @moduledoc """
  The main entry point for the Complang application.
  """
  use Application

  require Logger

  @impl true
  def start(_type, _args) do
    children = [
      Complang.Repo,
      ComplangWeb.Telemetry,
      {Phoenix.PubSub, name: Complang.PubSub},
      ComplangWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: Complang.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def init(_key, config) do
    {:ok, config}
  end
end
