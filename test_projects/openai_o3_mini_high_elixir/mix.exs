
defmodule Complang.MixProject do
  use Mix.Project

  def project do
    [
      app: :complang,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Complang.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.5"},
      {:plug, "~> 1.12"},
      {:ecto_sql, "~> 3.6"},
      {:postgrex, ">= 0.0.0"},
      {:jason, "~> 1.4"}
    ]
  end
end
