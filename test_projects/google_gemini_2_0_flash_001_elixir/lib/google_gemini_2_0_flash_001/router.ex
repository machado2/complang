defmodule GoogleGemini20Flash001.Router do
  use Plug.Router

  plug(Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["application/json"],
    json_decoder: Jason
  )
  plug(:match)
  plug(:dispatch)

  get "/users", do: GoogleGemini20Flash001.UserController.index(conn)
  post "/users", do: GoogleGemini20Flash001.UserController.create(conn)
  get "/users/:id", do: GoogleGemini20Flash001.UserController.show(conn, %{"id" => id})
  put "/users/:id", do: GoogleGemini20Flash001.UserController.update(conn, %{"id" => id})
  delete "/users/:id", do: GoogleGemini20Flash001.UserController.delete(conn, %{"id" => id})


  match _ do
    send_resp(conn, 404, "Not Found")
  end
end
