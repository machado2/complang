
defmodule App.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email])
    |> validate_required([:name, :email])
  end

  def create(attrs) do
    %App.User{}
    |> changeset(attrs)
    |> App.Repo.insert()
  end

  def all do
    App.Repo.all(App.User)
  end

  def get(id) do
    App.Repo.get(App.User, id)
  end

  def update(id, attrs) do
    case App.Repo.get(App.User, id) do
      nil ->
        {:error, :not_found}

      user ->
        user
        |> changeset(attrs)
        |> App.Repo.update()
    end
  end

  def delete(id) do
    case App.Repo.get(App.User, id) do
      nil ->
        {:error, :not_found}

      user ->
        App.Repo.delete(user)
    end
  end
end
