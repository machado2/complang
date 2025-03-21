
defmodule Complang.MixProject do
  use Mix.Project

  def project do
    [
      app: :complang,
      version: "0.1.0",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [
      mod: {Complang.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  # Specifies which paths to compile per environment
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies
  #
  # Type "mix help deps" for examples and options
  defp deps do
    [
      {:phoenix, "~> 1.7.7"},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.15.13"},
      {:phoenix_html, "~> 3.3"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_dashboard, "~> 0.8.0"},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.5"}
    ]
  end

  # Aliases are shortcuts for tasks. For example, to run the test
  # suite, you can type "mix test". See "mix help alias" for more info.
  defp aliases do
    [
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.deploy": ["esbuild default --minify", "phx.digest"]
    ]
  end
end
