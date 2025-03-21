
defmodule MyappWeb.UserView do
  use MyappWeb, :view

  def render("index.json", %{users: users}) do
    %{data: render_many(users, __MODULE__, "show.json")}
  end

  def render("show.json", %{user: user}) do
    %{id: user.id,
      name: user.name,
      email: user.email}
  end
end
