defmodule ComplangWeb.FallbackController do
  use ComplangWeb, :controller

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(Html)
    |> render(:"404")
  end
end
