
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
      extra_applications: [:logger],
      mod: {Complang.Application, []}
    ]
  end

  defp deps do
    [
      {:phoenix, "~> 1.6.0"},
      {:phoenix_ecto, "~> 4.3"},
      {:ecto_sql, "~> 3.6"},
      {:postgrex, "~> 0.15"},
      {:jason, "~> 1.2"}
    ]
  end
end
