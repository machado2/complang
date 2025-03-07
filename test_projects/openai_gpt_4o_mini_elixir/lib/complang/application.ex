
defmodule Complang.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Phoenix.PubSub, name: Complang.PubSub},
      Complang.Repo
    ]

    opts = [strategy: :one_for_one, name: Complang.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
