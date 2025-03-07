defmodule Complang.Users do
  import Ecto.Query, warn: false
  alias Complang.Repo
  alias Complang.User

  def list_users do
    Repo.all(User)
  end

  def get_user(id) do
    Repo.get(User, id)
  end

  def create_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  def update_user(user, attrs) do
    User.changeset(user, attrs)
    |> Repo.update()
  end

  def delete_user(user) do
    Repo.delete(user)
  end
end
