
defmodule ComplangWeb.Router do
  use ComplangWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", ComplangWeb do
    pipe_through :api

    resources "/users", UserController
  end
end
