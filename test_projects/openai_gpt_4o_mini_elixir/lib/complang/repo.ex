
defmodule Complang.Repo do
  use Ecto.Repo,
    otp_app: :complang,
    adapter: Ecto.Adapters.Postgres
end
