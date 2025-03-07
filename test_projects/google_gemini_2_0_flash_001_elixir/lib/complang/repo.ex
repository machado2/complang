defmodule Complang.Repo do
  use Ecto.Repo,
    otp_app: :complang

  import Ecto.Query
end
