open Lwt.Infix
open Opium

module PG = PGOCaml_lwt

type user = {
  id : int;
  name : string;
  email : string;
}

let user_to_yojson user =
  `Assoc [
    ("id", `Int user.id);
    ("name", `String user.name);
    ("email", `String user.email)
  ]

type new_user = {
  name : string;
  email : string;
}

let new_user_of_yojson json =
  match json with
  | `Assoc assoc ->
      (match List.assoc_opt "name" assoc, List.assoc_opt "email" assoc with
       | Some (`String name), Some (`String email) ->
           Some { name; email }
       | _ -> None)
  | _ -> None

(* Connect to PostgreSQL using parameters specified in the task.
   Reads the password from environment variable PGPASSWORD. *)
let get_db_connection () =
  let password = Sys.getenv "PGPASSWORD" in
  let conninfo =
    Printf.sprintf "host=host.docker.internal port=5432 dbname=complang user=testuser password=%s" password
  in
  PG.connect ~conninfo ()

(* POST /users: Create a new user *)
let create_user_handler req =
  let open Lwt.Syntax in
  let* body = Request.to_plain_text req in
  let json =
    try Yojson.Safe.from_string body with _ -> `Null
  in
  match new_user_of_yojson json with
  | None ->
      Response.of_plain_text ~status:`Bad_Request "Invalid JSON: missing name or email" |> Lwt.return
  | Some nuser ->
      let* db = get_db_connection () in
      let sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id" in
      let* res = PG.execute ~query:sql ~params:[| nuser.name; nuser.email |] db in
      match res with
      | [| [| id_str |] |] ->
          let id = int_of_string id_str in
          let user = { id; name = nuser.name; email = nuser.email } in
          let json_resp = Yojson.Safe.to_string (user_to_yojson user) in
          Response.of_plain_text
            ~status:`Created
            ~headers:(Cohttp.Header.of_list [("Content-Type", "application/json")])
            json_resp
          |> Lwt.return
      | _ ->
          Response.of_plain_text ~status:`Internal_Server_Error "Database insertion failed" |> Lwt.return

(* GET /users: Return a list of all users *)
let get_users_handler _req =
  let open Lwt.Syntax in
  let* db = get_db_connection () in
  let sql = "SELECT id, name, email FROM users" in
  let* rows = PG.execute ~query:sql db in
  let users =
    Array.fold_left (fun acc row ->
      match row with
      | [| id_str; name; email |] ->
          let id = int_of_string id_str in
          user_to_yojson { id; name; email } :: acc
      | _ -> acc
    ) [] rows
    |> List.rev
  in
  let json_resp = Yojson.Safe.to_string (`List users) in
  Response.of_plain_text
    ~status:`OK
    ~headers:(Cohttp.Header.of_list [("Content-Type", "application/json")])
    json_resp
  |> Lwt.return

(* GET /users/:id: Return a single user by id *)
let get_user_handler req =
  let open Lwt.Syntax in
  let id_param = Router.param req "id" in
  let id_int = int_of_string id_param in
  let* db = get_db_connection () in
  let sql = "SELECT id, name, email FROM users WHERE id = $1" in
  let* rows = PG.execute ~query:sql ~params:[| string_of_int id_int |] db in
  match rows with
  | [| [| id_str; name; email |] |] ->
      let user = { id = int_of_string id_str; name; email } in
      let json_resp = Yojson.Safe.to_string (user_to_yojson user) in
      Response.of_plain_text
        ~status:`OK
        ~headers:(Cohttp.Header.of_list [("Content-Type", "application/json")])
        json_resp
      |> Lwt.return
  | _ ->
      Response.of_plain_text ~status:`Not_Found "User not found" |> Lwt.return

(* PUT /users/:id: Update a user *)
let update_user_handler req =
  let open Lwt.Syntax in
  let id_param = Router.param req "id" in
  let id_int = int_of_string id_param in
  let* body = Request.to_plain_text req in
  let json =
    try Yojson.Safe.from_string body with _ -> `Null
  in
  match new_user_of_yojson json with
  | None ->
      Response.of_plain_text ~status:`Bad_Request "Invalid JSON: missing name or email" |> Lwt.return
  | Some nuser ->
      let* db = get_db_connection () in
      let sql = "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id" in
      let* res = PG.execute ~query:sql ~params:[| nuser.name; nuser.email; string_of_int id_int |] db in
      match res with
      | [| _ |] ->
          Response.of_plain_text ~status:`OK "User updated" |> Lwt.return
      | _ ->
          Response.of_plain_text ~status:`Not_Found "User not found" |> Lwt.return

(* DELETE /users/:id: Delete a user *)
let delete_user_handler req =
  let open Lwt.Syntax in
  let id_param = Router.param req "id" in
  let id_int = int_of_string id_param in
  let* db = get_db_connection () in
  let sql = "DELETE FROM users WHERE id = $1 RETURNING id" in
  let* res = PG.execute ~query:sql ~params:[| string_of_int id_int |] db in
  match res with
  | [| _ |] ->
      Response.of_plain_text ~status:`OK "User deleted" |> Lwt.return
  | _ ->
      Response.of_plain_text ~status:`Not_Found "User not found" |> Lwt.return

let () =
  let app =
    App.empty
    |> App.post "/users" create_user_handler
    |> App.get "/users" get_users_handler
    |> App.get "/users/:id" get_user_handler
    |> App.put "/users/:id" update_user_handler
    |> App.delete "/users/:id" delete_user_handler
  in
  App.run ~port:8080 app
