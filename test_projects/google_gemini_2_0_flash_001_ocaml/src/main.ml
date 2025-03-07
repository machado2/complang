open Opium
open Lwt.Syntax
open Yojson.Safe.Util

let db_conn_promise = Db.connect ()

module User = struct
  type t = {
    id : int;
    name : string;
    email : string;
  } [@@deriving yojson]

  let t_of_tuple (id, name, email) : t = { id; name; email }
end

let json_of_users users =
  `List (List.map User.to_yojson users)

let get_users _req =
  let* db_conn = db_conn_promise in
  let* result = Pgx.query db_conn "SELECT id, name, email FROM users" in
  match result with
  | Ok result ->
    let users =
      Pgx.rows result
      |> List.map (fun row ->
             (Pgx.Value.get_int (List.nth row 0),
              Pgx.Value.get_string (List.nth row 1),
              Pgx.Value.get_string (List.nth row 2)))
      |> List.map User.t_of_tuple
    in
    let json = json_of_users users in
    Lwt.return (Response.of_json ~code:`OK json)
  | Error err ->
    Printf.eprintf "Error querying database: %s\n" (Pgx.string_of_error err);
    Lwt.return (Response.of_plain_text ~code:`Internal_server_error "Database error")


let get_user req =
  let id = Router.param req "id" |> int_of_string in
  let* db_conn = db_conn_promise in
  let* result = Pgx.query db_conn "SELECT id, name, email FROM users WHERE id = $1" ~params:[|Pgx.Value.of_int id|] in
  match result with
  | Ok result ->
    match Pgx.rows result with
    | [] -> Lwt.return (Response.of_plain_text ~code:`Not_found "User not found")
    | row :: _ ->
      let user =
        (Pgx.Value.get_int (List.nth row 0),
         Pgx.Value.get_string (List.nth row 1),
         Pgx.Value.get_string (List.nth row 2))
        |> User.t_of_tuple
      in
      Lwt.return (Response.of_json ~code:`OK (User.to_yojson user))
  | Error err ->
    Printf.eprintf "Error querying database: %s\n" (Pgx.string_of_error err);
    Lwt.return (Response.of_plain_text ~code:`Internal_server_error "Database error")


let post_user req =
  let* json = Request.to_json_exn req in
  let name = json |> member "name" |> to_string in
  let email = json |> member "email" |> to_string in
  let* db_conn = db_conn_promise in
  let* result = Pgx.query db_conn "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
      ~params:[|Pgx.Value.of_string name; Pgx.Value.of_string email|] in
  match result with
  | Ok result ->
    let row = List.hd (Pgx.rows result) in
    let user =
      (Pgx.Value.get_int (List.nth row 0),
       Pgx.Value.get_string (List.nth row 1),
       Pgx.Value.get_string (List.nth row 2))
      |> User.t_of_tuple
    in
    Lwt.return (Response.of_json ~code:`Created (User.to_yojson user))
  | Error err ->
    Printf.eprintf "Error inserting into database: %s\n" (Pgx.string_of_error err);
    Lwt.return (Response.of_plain_text ~code:`Internal_server_error "Database error")


let put_user req =
  let id = Router.param req "id" |> int_of_string in
  let* json = Request.to_json_exn req in
  let name = json |> member "name" |> to_string in
  let email = json |> member "email" |> to_string in
  let* db_conn = db_conn_promise in
  let* result = Pgx.query db_conn "UPDATE users SET name = $1, email = $2 WHERE id = $3"
      ~params:[|Pgx.Value.of_string name; Pgx.Value.of_string email; Pgx.Value.of_int id|] in
  match result with
  | Ok result ->
    if Pgx.affected_rows result > 0 then
      Lwt.return (Response.of_plain_text ~code:`No_content "")
    else
      Lwt.return (Response.of_plain_text ~code:`Not_found "User not found")
  | Error err ->
    Printf.eprintf "Error updating database: %s\n" (Pgx.string_of_error err);
    Lwt.return (Response.of_plain_text ~code:`Internal_server_error "Database error")


let delete_user req =
  let id = Router.param req "id" |> int_of_string in
  let* db_conn = db_conn_promise in
  let* result = Pgx.query db_conn "DELETE FROM users WHERE id = $1" ~params:[|Pgx.Value.of_int id|] in
  match result with
  | Ok result ->
    if Pgx.affected_rows result > 0 then
      Lwt.return (Response.of_plain_text ~code:`No_content "")
    else
      Lwt.return (Response.of_plain_text ~code:`Not_found "User not found")
  | Error err ->
    Printf.eprintf "Error deleting from database: %s\n" (Pgx.string_of_error err);
    Lwt.return (Response.of_plain_text ~code:`Internal_server_error "Database error")


let () =
  App.empty
  |> App.port 8080
  |> App.get "/users" get_users
  |> App.get "/users/:id" get_user
  |> App.post "/users" post_user
  |> App.put "/users/:id" put_user
  |> App.delete "/users/:id" delete_user
  |> App.run_command
