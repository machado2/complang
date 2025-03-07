
defmodule App.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  post "/users" do
    case App.User.create(conn.body_params) do
      {:ok, user} ->
        send_resp(conn, 201, Jason.encode!(user))

      {:error, _} ->
        send_resp(conn, 400, Jason.encode!(%{error: "Failed to create user"}))
    end
  end

  get "/users" do
    users = App.User.all()
    send_resp(conn, 200, Jason.encode!(users))
  end

  get "/users/:id" do
    case App.User.get(id) do
      nil ->
        send_resp(conn, 404, Jason.encode!(%{error: "User not found"}))

      user ->
        send_resp(conn, 200, Jason.encode!(user))
    end
  end

  put "/users/:id" do
    case App.User.update(id, conn.body_params) do
      {:ok, user} ->
        send_resp(conn, 200, Jason.encode!(user))

      {:error, _} ->
        send_resp(conn, 404, Jason.encode!(%{error: "User not found"}))
    end
  end

  delete "/users/:id" do
    case App.User.delete(id) do
      {:ok, _} ->
        send_resp(conn, 204, "")

      {:error, _} ->
        send_resp(conn, 404, Jason.encode!(%{error: "User not found"}))
    end
  end
end
