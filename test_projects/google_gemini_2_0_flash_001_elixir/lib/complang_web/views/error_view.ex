
defmodule ComplangWeb.ErrorView do
  use ComplangWeb, :view

  # If you want to customize your error pages,
  # uncomment the module below, adapt it to your
  # template format, and use Plug.Exception.

  def render("404.json", _assigns) do
    %{errors: %{detail: "Not Found"}}
  end

  def render("500.json", _assigns) do
    %{errors: %{detail: "Internal Server Error"}}
  end

  # def template_not_found(template, assigns) do
  #   Phoenix.View.render_to_string(ComplangWeb.ErrorView, "500",
  #                                  assigns |> Map.put(:template, template))
  # end
end
