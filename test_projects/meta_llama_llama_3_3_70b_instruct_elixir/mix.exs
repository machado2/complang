
defmodule App.MixProject do
  use Mix.Project

  def project do
    [
      app: :app,
      version: "0.1.0",
      build_path: "./_build",
      config_path: "./config/config.exs",
      deps_path: "./deps",
      lockfile: "./mix.lock",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do
    [
      {:ecto, "~> 3.8"},
      {:ecto_sql, "~> 3.8"},
      {:postgrex, ">= 0.0.0"},
      {:jason, "~> 1.3"},
      {:plug_cowboy, "~> 2.5"}
    ]
  end
end
