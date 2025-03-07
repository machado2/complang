
defmodule ComplangApiWeb.ChangesetView do
  use ComplangApiWeb, :view

  def render("error.json", %{changeset: changeset}) do
    # When encoded, the changeset returns its errors
    # as a JSON object. So we just pass it forward.
    %{errors: errors_on(changeset)}
  end

  defp errors_on(changeset) do
    Ecto.Changeset.errors(changeset)
  end
end
