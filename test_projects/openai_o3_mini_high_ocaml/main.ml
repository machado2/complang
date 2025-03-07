open Lwt.Infix
open Opium.Std
open Yojson.Safe
open Yojson.Safe.Util

(* Establish DB connection *)
let db =
  let password =
    try Sys.getenv "PGPASSWORD"
    with Not_found -> failwith "PGPASSWORD environment variable not set"
  in
  let conninfo =
    Printf.sprintf "host=host.docker.internal port=5432 dbname=complang user=testuser password=%s" password
  in
  Postgresql.Connection.connect ~conninfo ()

(* POST /users: Create a new user *)
let create_user req =
  req.body |> Body.to_string >>= fun body_str ->
  let json = from_string body_str in
  let name = to_string (member "name" json) in
  let email = to_string (member "email" json) in
  let res = db#exec_params "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id;" [| name; email |] in
  let id = int_of_string (res#getvalue 0 0) in
  let user_json = `Assoc [ ("id", `Int id); ("name", `String name); ("email", `String email) ] in
  let response = Response.of_json user_json in
  Lwt.return (Response.with_status `Created response)

(* GET /users: List all users *)
let list_users _req =
  let res = db#exec "SELECT id, name, email FROM users;" in
  let count = res#ntuples in
  let users = List.init count (fun i ->
    let id = int_of_string (res#getvalue i 0) in
    let name = res#getvalue i 1 in
    let email = res#getvalue i 2 in
    `Assoc [ ("id", `Int id); ("name", `String name); ("email", `String email) ]
  ) in
  Lwt.return (Response.of_json (`List users))

(* GET /users/:id: Retrieve a single user *)
let get_user req =
  let id_str = Router.param req "id" in
  let id = int_of_string id_str in
  let res = db#exec_params "SELECT id, name, email FROM users WHERE id = $1 LIMIT 1;" [| string_of_int id |] in
  if res#ntuples = 0 then
    Lwt.return (Response.of_plain_text ~status:`Not_Found "User not found")
  else
    let user_json = `Assoc [
      ("id", `Int (int_of_string (res#getvalue 0 0)));
      ("name", `String (res#getvalue 0 1));
      ("email", `String (res#getvalue 0 2))
    ] in
    Lwt.return (Response.of_json user_json)

(* PUT /users/:id: Update an existing user *)
let update_user req =
  let id_str = Router.param req "id" in
  let id = int_of_string id_str in
  req.body |> Body.to_string >>= fun body_str ->
  let json = from_string body_str in
  let name = to_string (member "name" json) in
  let email = to_string (member "email" json) in
  let res = db#exec_params "UPDATE users SET name = $1, email = $2 WHERE id = $3;" [| name; email; string_of_int id |] in
  if res#cmdtuples = "0" then
    Lwt.return (Response.of_plain_text ~status:`Not_Found "User not found")
  else
    Lwt.return (Response.empty ~status:`OK)

(* DELETE /users/:id: Delete a user *)
let delete_user req =
  let id_str = Router.param req "id" in
  let id = int_of_string id_str in
  let res = db#exec_params "DELETE FROM users WHERE id = $1;" [| string_of_int id |] in
  if res#cmdtuples = "0" then
    Lwt.return (Response.of_plain_text ~status:`Not_Found "User not found")
  else
    Lwt.return (Response.empty ~status:`OK)

let () =
  App.empty
  |> App.post "/users" create_user
  |> App.get "/users" list_users
  |> App.get "/users/:id" get_user
  |> App.put "/users/:id" update_user
  |> App.delete "/users/:id" delete_user
  |> App.run_command ~port:8080
