defmodule ComplangAPI.Repo do
  use Ecto.Repo,
    otp_app: :complang_api,
    adapter: Ecto.Adapters.Postgres
end
