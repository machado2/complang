
defmodule UsersApi.Router do
  use Plug.Router
  require Logger

  plug :match
  plug Plug.Parsers, parsers: [:json], json_decoder: Jason
  plug :dispatch

  # GET /users - List all users
  get "/users" do
    users = UsersApi.Repo.all(UsersApi.User)
    users_json = Enum.map(users, fn user -> 
      %{id: user.id, name: user.name, email: user.email}
    end)
    
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(users_json))
  end

  # POST /users - Create a user
  post "/users" do
    changeset = UsersApi.User.changeset(%UsersApi.User{}, conn.body_params)
    
    case UsersApi.Repo.insert(changeset) do
      {:ok, user} ->
        response = %{id: user.id, name: user.name, email: user.email}
        
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(201, Jason.encode!(response))
        
      {:error, changeset} ->
        errors = Ecto.Changeset.traverse_errors(changeset, fn {msg, _} -> msg end)
        
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(%{errors: errors}))
    end
  end

  # GET /users/:id - Get a specific user
  get "/users/:id" do
    case UsersApi.Repo.get(UsersApi.User, id) do
      nil ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(%{error: "User not found"}))
        
      user ->
        response = %{id: user.id, name: user.name, email: user.email}
        
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(response))
    end
  end

  # PUT /users/:id - Update a user
  put "/users/:id" do
    case UsersApi.Repo.get(UsersApi.User, id) do
      nil ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(%{error: "User not found"}))
        
      user ->
        changeset = UsersApi.User.changeset(user, conn.body_params)
        
        case UsersApi.Repo.update(changeset) do
          {:ok, updated_user} ->
            response = %{id: updated_user.id, name: updated_user.name, email: updated_user.email}
            
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(200, Jason.encode!(response))
            
          {:error, changeset} ->
            errors = Ecto.Changeset.traverse_errors(changeset, fn {msg, _} -> msg end)
            
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(400, Jason.encode!(%{errors: errors}))
        end
    end
  end

  # DELETE /users/:id - Delete a user
  delete "/users/:id" do
    case UsersApi.Repo.get(UsersApi.User, id) do
      nil ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(%{error: "User not found"}))
        
      user ->
        UsersApi.Repo.delete(user)
        
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(204, "")
    end
  end

  # Fallback for all unmatched routes
  match _ do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end
end
