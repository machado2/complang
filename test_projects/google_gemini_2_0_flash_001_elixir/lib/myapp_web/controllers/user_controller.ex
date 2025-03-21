
defmodule MyappWeb.UserController do
  use MyappWeb, :controller

  alias Complang.Users
  alias Complang.Users.User

  action_fallback MyappWeb.FallbackController

  def index(conn, _params) do
    users = Users.list_users()
    render(conn, :index, users: users)
  end

  def create(conn, %{"user" => user_params}) do
    with {:ok, %User{} = user} <- Users.create_user(user_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", ~p"/users/#{user.id}")
      |> render(:show, user: user)
    end
  end

  def show(conn, %{"id" => id}) do
    user = Users.get_user(id)

    if user do
      render(conn, :show, user: user)
    else
      conn
      |> put_status(:not_found)
      |> render(MyappWeb.ErrorView, :"404")
    end
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    user = Users.get_user(id)

    if user do
      with {:ok, %User{} = user} <- Users.update_user(user, user_params) do
        send_resp(conn, :no_content, "")
      end
    else
      conn
      |> put_status(:not_found)
      |> render(MyappWeb.ErrorView, :"404")
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Users.get_user(id)

    if user do
        send_resp(conn, :no_content, "")
    else
      conn
      |> put_status(:not_found)
      |> render(MyappWeb.ErrorView, :"404")
    end
  end
end
