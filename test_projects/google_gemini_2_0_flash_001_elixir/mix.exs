
defmodule ComplangApi.Mixfile do
  use Mix.Project

  def project do
    [app: :complang_api,
     version: "0.0.1",
     elixir: "~> 1.14",
     deps: deps(),
     consolidate_protocols: false,
     start_permanent: Mix.env == :prod,
     preferred_cli_env: [default: :prod, phx: :server],
     aliases: aliases()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [mod: {ComplangApi.Application, []},
     extra_applications: [:logger, :runtime_tools]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path dependencies:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [{:phoenix, "~> 1.6.0"},
     {:phoenix_pubsub, "~> 2.0"},
     {:phoenix_html, "~> 3.0"},
     {:phoenix_live_reload, "~> 1.2", only: :dev},
     {:phoenix_live_view, "~> 0.17.5"},
     {:floki, ">= 0.30.0", only: :test},
     {:phoenix_ecto, "~> 4.4"},
     {:ecto_sql, "~> 3.10"},
     {:postgrex, ">= 0.15.6"},
     {:jason, "~> 1.2"},
     {:plug_cowboy, "~> 2.5"},
     {:telemetry_metrics, "~> 0.6"},
     {:telemetry_poller, "~> 1.0"}]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [setup: ["deps.get", "ecto.setup"],
     "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
     "ecto.reset": ["ecto.drop", "ecto.setup"],
     test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"]]
  end
end
