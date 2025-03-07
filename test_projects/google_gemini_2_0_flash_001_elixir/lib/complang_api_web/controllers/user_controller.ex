
defmodule ComplangApiWeb.UserController do
  use ComplangApiWeb, :controller

  def index(conn, _params) do
    users = ComplangApi.Repo.all(ComplangApi.User)
    render(conn, "index.json", users: users)
  end

  def show(conn, %{"id" => id}) do
    case ComplangApi.Repo.get(ComplangApi.User, id) do
      nil ->
        send_resp(conn, 404, "Not Found")
      user ->
        render(conn, "show.json", user: user)
    end
  end

  def create(conn, %{"user" => user_params}) do
    case ComplangApi.create_user(user_params) do
      {:ok, user} ->
        conn
        |> put_status(:created)
        |> render("show.json", user: user)
      {:error, changeset} ->
        conn
        |> put_status(:bad_request)
        |> render(ComplangApiWeb.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    case ComplangApi.update_user(id, user_params) do
      {:ok, user} ->
        render(conn, "show.json", user: user)
      {:error, :not_found} ->
        send_resp(conn, 404, "Not Found")
      {:error, changeset} ->
        conn
        |> put_status(:bad_request)
        |> render(ComplangApiWeb.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    case ComplangApi.delete_user(id) do
      :ok ->
        send_resp(conn, 204, "")
      :error ->
        send_resp(conn, 404, "Not Found")
    end
  end
end
