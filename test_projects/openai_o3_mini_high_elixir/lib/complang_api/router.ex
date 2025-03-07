defmodule ComplangAPI.Router do
  use Plug.Router
  use Plug.ErrorHandler

  plug :match
  plug Plug.Parsers, parsers: [:json], json_decoder: Jason
  plug :dispatch

  alias ComplangAPI.{Repo, User}

  # POST /users: Create a user.
  post "/users" do
    changeset = User.changeset(%User{}, conn.body_params)
    case Repo.insert(changeset) do
      {:ok, user} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(201, Jason.encode!(%{id: user.id, name: user.name, email: user.email}))
      {:error, changeset} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(%{error: "Invalid data", details: changeset.errors}))
    end
  end

  # GET /users: List all users.
  get "/users" do
    users = Repo.all(User)
    users = Enum.map(users, fn u -> %{id: u.id, name: u.name, email: u.email} end)
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(users))
  end

  # GET /users/:id: Retrieve a single user.
  get "/users/:id" do
    case Repo.get(User, id) do
      nil ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(%{error: "User not found"}))
      user ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{id: user.id, name: user.name, email: user.email}))
    end
  end

  # PUT /users/:id: Update a user.
  put "/users/:id" do
    case Repo.get(User, id) do
      nil ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(%{error: "User not found"}))
      user ->
        changeset = User.changeset(user, conn.body_params)
        case Repo.update(changeset) do
          {:ok, updated_user} ->
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(200, Jason.encode!(%{id: updated_user.id, name: updated_user.name, email: updated_user.email}))
          {:error, changeset} ->
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(400, Jason.encode!(%{error: "Invalid data", details: changeset.errors}))
        end
    end
  end

  # DELETE /users/:id: Delete a user.
  delete "/users/:id" do
    case Repo.get(User, id) do
      nil ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(%{error: "User not found"}))
      user ->
        case Repo.delete(user) do
          {:ok, _} ->
            conn |> send_resp(204, "")
          {:error, _} ->
            conn |> send_resp(400, "")
        end
    end
  end

  match _ do
    send_resp(conn, 404, "Not found")
  end
end
