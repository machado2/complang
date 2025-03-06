defmodule Complang.UserController do
  use Complang.Web, :controller

  alias Complang.Repo
  alias Complang.User
  alias Ecto.Changeset

  action_fallback Complang.Web.FallbackController

  def index(conn, _params) do
    users = Repo.all(User)
    render(conn, "index.json", users: users)
  end

  def show(conn, %{"id" => id}) do
    user = Repo.get(User, id)
    case user do
      nil ->
        conn
        |> send_resp(404, "")
      user ->
        render(conn, "show.json", user: user)
    end
  end

  def create(conn, %{"user" => user_params}) do
    changeset = User.changeset(%User{}, user_params)
    case Repo.insert(changeset) do
      {:ok, user} ->
        conn
        |> send_resp(201, {"id": user.id, "name": user.name, "email": user.email} |> Jason.encode!)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(Complang.Web.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    user = Repo.get(User, id)
    case user do
      nil ->
        conn
        |> send_resp(404, "")
      user ->
        changeset = User.changeset(user, user_params)
        case Repo.update(changeset) do
          {:ok, user} ->
            conn
            |> send_resp(200, %{"id": user.id, "name": user.name, "email": user.email} |> Jason.encode!)

          {:error, changeset} ->
             conn
            |> put_status(:unprocessable_entity)
            |> render(Complang.Web.ChangesetView, "error.json", changeset: changeset)
        end
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Repo.get(User, id)
    case user do
      nil ->
        conn
        |> send_resp(404, "")
      user ->
        Repo.delete(user)
        |> send_resp(204, "")
    end
  end
end