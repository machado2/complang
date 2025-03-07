defmodule ComplangWeb do
  @moduledoc """
  The entry point for defining your web interface, such
  as controllers, views, channels and so on.

  This can be used throughout your application as
  a single source of truth for web-related paths,
  plug definitions and configuration.
  """
  use Phoenix.Component

  import Phoenix.LiveView.Utils

  @doc """
  Can be used in your application routes.
  """
  def router do
    quote do
      use Phoenix.Router
      import Plug.Conn
      import Phoenix.Controller
    end
  end

  @doc """
  Use PowAssent to handle user authentication.

  Use the Phoenix.Controller.redirect/2 function in your
  controllers instead of Phoenix.Controller.render/3 to
  redirect to the appropriate page.
  """
  def controller do
    quote do
      use Phoenix.Controller, namespace: ComplangWeb

      import Plug.Conn
      import ComplangWeb.Gettext
      alias ComplangWeb.Router.Helpers, as: Routes
    end
  end

  @doc """
  Channels are useful for bidirectional communication
  between the server and client.
  """
  def channel do
    quote do
      use Phoenix.Channel
      import ComplangWeb.Gettext
    end
  end

  @doc """
  Views render the content when you don't need
  response rendering logic.
  """
  def view do
    quote do
      use Phoenix.View,
        root: "lib/complang_web/templates",
        namespace: ComplangWeb

      # Import convenience functions from Controllers like put_flash/2
      import Phoenix.Controller, only: [get_flash: 2, view_module: 1]
      import Phoenix.LiveView.Helpers
      import ComplangWeb.Gettext
      alias ComplangWeb.Router.Helpers, as: Routes
    end
  end

  @doc """
  Components are reusable parts of the page
  that encapsulate rendering logic.
  """
  def component do
    quote do
      use Phoenix.Component

      import ComplangWeb.Gettext
    end
  end

  @doc """
  Adds the `Gettext` bindin to your context.
  """
  def gettext do
    quote do
      import ComplangWeb.Gettext
    end
  end
end
