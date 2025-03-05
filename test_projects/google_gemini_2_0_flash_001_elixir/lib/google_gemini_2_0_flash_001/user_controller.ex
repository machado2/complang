defmodule GoogleGemini20Flash001.UserController do
  import Ecto.Query, warn: false
  alias GoogleGemini20Flash001.Repo
  alias GoogleGemini20Flash001.User
  require Logger
  import Plug.Conn

  def index(conn) do
    users = Repo.all(User)
    users_json = Jason.encode!(Enum.map(users, fn user ->
      %{id: user.id, name: user.name, email: user.email}
    end))
    send_resp(conn, 200, users_json)
  end

  def create(conn) do
    {:ok, body, _conn} = read_body(conn)
    {:ok, user_params} = Jason.decode(body)
    changeset = User.new_changeset(user_params)

    case Repo.insert!(changeset) do
      user ->
        user_json = Jason.encode!(%{id: user.id, name: user.name, email: user.email})
        send_resp(conn, 201, user_json)

      exception ->
        Logger.error("Error creating user: #{inspect(exception)}")
        send_resp(conn, 400, "Bad Request")
    end
  end

  def show(conn, %{"id" => id}) do
    user = Repo.get(User, String.to_integer(id))

    case user do
      nil ->
        send_resp(conn, 404, "Not Found")
      user ->
        user_json = Jason.encode!(%{id: user.id, name: user.name, email: user.email})
        send_resp(conn, 200, user_json)
    end
  end

  def update(conn, %{"id" => id}) do
    user = Repo.get(User, String.to_integer(id))

    case user do
      nil ->
        send_resp(conn, 404, "Not Found")
      user ->
       {:ok, body, _conn} = read_body(conn)
        {:ok, user_params} = Jason.decode(body)
        changeset = User.changeset(user, user_params)

        case Repo.update!(changeset) do
          user ->
            send_resp(conn, 204, "")
           exception ->
            Logger.error("Error updating user: #{inspect(exception)}")
            send_resp(conn, 400, "Bad Request")
        end
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Repo.get(User, String.to_integer(id))

    case user do
      nil ->
        send_resp(conn, 404, "Not Found")
      user ->
        Repo.delete!(user)
        send_resp(conn, 204, "")
    end
  end
end
