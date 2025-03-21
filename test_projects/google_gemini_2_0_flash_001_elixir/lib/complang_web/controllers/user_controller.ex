
defmodule ComplangWeb.UserController do
  use ComplangWeb, :controller

  def create(conn, _params) do
    IO.puts("Create user endpoint called")
    conn
    |> send_resp(201, ~s|{"id": 1, "name": "John Doe", "email": "iohn@example.com"}|)
  end

  def index(conn, _params) do
    IO.puts("Index user endpoint called")
    conn
    |> send_resp(200, ~s|[{"id": 1, "name": "John Doe", "email": "john@example.com"}]|)
  end

  def show(conn, %{"id" => id}) do
    IO.puts("Show user endpoint called")
    conn
    |> send_resp(200, ~s|{"id": #{id}, "name": "John Doe", "email": "john@example.com"}|)
  end

  def update(conn, %{"id" => id}) do
    IO.puts("Update user endpoint called")
    conn
    |> send_resp(200, "")
  end

  def delete(conn, %{"id" => id}) do
    IO.puts("Delete user endpoint called")
    conn
    |> send_resp(200, "")
  end
end
