use Mix.Config

# For production-like environments, we configure
# builds to use releases.
#
# By default, if no environment is specified, a dev
# release will be built.
if config_env() == :prod do
  # Include static assets in release.
  configure_for_static_assets(true)

  # Set the path that will be passed to `mix phx.digest.run`
  # when running `mix release`.
  #
  # Mixed into the `script` configuration value above.
  config :complang_web, :static_paths, ~w(assets files priv/static)
end

defp configure_for_static_assets(release) do
  opts = [
    path: :sys.get_env("PHX_DIGEST_PATH") || "priv/static"
  ]

  if release do
    opts = Keyword.put(opts, :digest_manifest, "priv/static/phoenix_manifest.json")
  end

  config :complang_web, :static_paths, ~w(assets files priv/static
  )
end
# Configure esbuild (the version that ships with Phoenix)
config :esbuild,
  version: "0.17.19",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]
