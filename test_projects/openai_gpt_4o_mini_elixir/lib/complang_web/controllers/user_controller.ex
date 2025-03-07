
defmodule ComplangWeb.UserController do
  use ComplangWeb, :controller
  alias Complang.{Repo, User}

  action_fallback ComplangWeb.FallbackController

  def create(conn, %{"name" => name, "email" => email}) do
    %User{}
    |> User.changeset(%{name: name, email: email})
    |> Repo.insert()
    |> case do
      {:ok, user} ->
        conn
        |> put_status(:created)
        |> json(%{id: user.id, name: user.name, email: user.email})

      {:error, changeset} ->
        conn
        |> put_status(:bad_request)
        |> json(changeset)
    end
  end

  def index(conn, _params) do
    users = Repo.all(User)
    json(conn, users)
  end

  def show(conn, %{"id" => id}) do
    case Repo.get(User, id) do
      nil ->
        send_resp(conn, :not_found, "User not found")

      user ->
        json(conn, %{id: user.id, name: user.name, email: user.email})
    end
  end

  def update(conn, %{"id" => id, "name" => name, "email" => email}) do
    user = Repo.get!(User, id)

    user
    |> User.changeset(%{name: name, email: email})
    |> Repo.update()
    |> case do
      {:ok, _user} ->
        send_resp(conn, :no_content, "")

      {:error, changeset} ->
        conn
        |> put_status(:bad_request)
        |> json(changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Repo.get!(User, id)

    Repo.delete(user)
    send_resp(conn, :no_content, "")
  end
end
