defmodule GoogleGemini20Flash001.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string

    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email])
  end


  def new_changeset(attrs) do
    %__MODULE__{}
    |> cast(attrs, [:name, :email])
  end
end
