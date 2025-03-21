
defmodule ComplangWeb.Router do
  use ComplangWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", ComplangWeb do
    pipe_through :api

    post "/users", ComplangWeb.UserController, :create
    get "/users", ComplangWeb.UserController, :index
    get "/users/:id", ComplangWeb.UserController, :show
    put "/users/:id", ComplangWeb.UserController, :update
    delete "/users/:id", ComplangWeb.UserController, :delete
  end
end
