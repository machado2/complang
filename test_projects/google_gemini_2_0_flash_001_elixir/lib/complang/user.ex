defmodule Complang.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string

    timestamps()
  end

  @required_fields ~w(name email)a
  @optional_fields ~w()a

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email])
    |> validate_required(@required_fields)
    |> validate_format(:email, ~r/@/) # Simple email validation
  end
end