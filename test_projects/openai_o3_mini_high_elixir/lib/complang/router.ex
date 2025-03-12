
defmodule Complang.Router do
  use Plug.Router
  require Logger

  plug Plug.Logger
  plug :match
  plug Plug.Parsers, parsers: [:json], pass: ["application/json"], json_decoder: Jason
  plug :dispatch

  alias Complang.Users

  post "/users" do
    {:ok, body, conn} = read_body(conn)
    case Jason.decode(body) do
      {:ok, params} ->
        case Users.create_user(params) do
          {:ok, user} ->
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(201, Jason.encode!(user))
          {:error, changeset} ->
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(400, Jason.encode!(%{error: "Invalid data", details: inspect(changeset)}))
        end
      {:error, _reason} ->
        send_resp(conn, 400, "Invalid JSON")
    end
  end

  get "/users" do
    users = Users.list_users()
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(users))
  end

  get "/users/:id" do
    id = String.to_integer(id)
    case Users.get_user(id) do
      nil ->
        send_resp(conn, 404, "User not found")
      user ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(user))
    end
  end

  put "/users/:id" do
    id = String.to_integer(id)
    case Users.get_user(id) do
      nil ->
        send_resp(conn, 404, "User not found")
      user ->
        {:ok, body, conn} = read_body(conn)
        case Jason.decode(body) do
          {:ok, params} ->
            case Users.update_user(user, params) do
              {:ok, updated_user} ->
                conn
                |> put_resp_content_type("application/json")
                |> send_resp(200, Jason.encode!(updated_user))
              {:error, changeset} ->
                conn
                |> put_resp_content_type("application/json")
                |> send_resp(400, Jason.encode!(%{error: "Invalid data", details: inspect(changeset)}))
            end
          {:error, _reason} ->
            send_resp(conn, 400, "Invalid JSON")
        end
    end
  end

  delete "/users/:id" do
    id = String.to_integer(id)
    case Users.get_user(id) do
      nil ->
        send_resp(conn, 404, "User not found")
      user ->
        case Users.delete_user(user) do
          {:ok, _struct} ->
            send_resp(conn, 200, "User deleted")
          {:error, _reason} ->
            send_resp(conn, 500, "Failed to delete user")
        end
    end
  end

  match _ do
    send_resp(conn, 404, "Not found")
  end
end
