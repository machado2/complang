
defmodule MyappWeb.Router do
  use MyappWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", MyappWeb do
    pipe_through :api
    resources "/users", MyappWeb.UserController, except: [:new, :edit]
  end
end
