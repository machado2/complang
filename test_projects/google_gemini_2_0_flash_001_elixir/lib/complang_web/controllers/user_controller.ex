defmodule ComplangWeb.UserController do
  use ComplangWeb, :controller

  alias Complang.Users
  alias Complang.User

  action_fallback ComplangWeb.FallbackController

  def index(conn, _params) do
    users = Users.list_users()
    render(conn, :index, "index.json", users: users)
  end

  def create(conn, %{"user" => user_params}) do
    with {:ok, %Complang.User{} = user} <- Users.create_user(user_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", ~p"/users/#{user.id}")
      |> render(:show, "show.json", user: user)
    end
  end

  def show(conn, %{"id" => id}) do
    user = Users.get_user(id)

    if user do
      render(conn, :show, "show.json", user: user)
    else
      conn
      |> put_status(:not_found)
      |> render(ComplangWeb.FallbackController, "404")
    end
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    user = Users.get_user(id)

    if user do
      case Users.update_user(user, user_params) do
        {:ok, %Complang.User{} = user} ->
          send_resp(conn, :no_content, "")

        {:error, changeset} ->
          conn
          |> put_status(:unprocessable_entity)
          |> render(ComplangWeb.FallbackController, "422", changeset: changeset)
      end
    else
      conn
      |> put_status(:not_found)
      |> render(ComplangWeb.FallbackController, "404")
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Users.get_user(id)

    if user do
      case Users.delete_user(user) do
        {:ok, %Complang.User{}} ->
          send_resp(conn, :no_content, "")

        {:error, changeset} ->
          conn
          |> put_status(:unprocessable_entity)
          |> render(ComplangWeb.FallbackController, "422", changeset: changeset)
      end
    else
      conn
      |> put_status(:not_found)
      |> render(ComplangWeb.FallbackController, "404")
    end
  end
end
