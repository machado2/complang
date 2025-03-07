
defmodule ComplangApi do
  import Ecto.Query

  alias ComplangApi.Repo
  alias ComplangApi.User

  def list_users do
    Repo.all(User)
  end

  def get_user(id) do
    Repo.get(User, id)
  end

  def create_user(attrs \ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  def update_user(id, attrs) do
    case Repo.get(User, id) do
      nil ->
        {:error, :not_found}
      user ->
        user
        |> User.changeset(attrs)
        |> Repo.update()
    end
  end

  def delete_user(id) do
    case Repo.get(User, id) do
      nil ->
        {:error, :not_found}
      user ->
        Repo.delete(user)
        |> case do
          {:ok, _} -> :ok
          {:error, _} -> :error
        end
    end
  end
end
