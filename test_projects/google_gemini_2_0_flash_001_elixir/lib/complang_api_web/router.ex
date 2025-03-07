
defmodule ComplangApiWeb.Router do
  use ComplangApiWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", ComplangApiWeb do
    pipe_through :api
    resources "/users", UserController
  end
end
