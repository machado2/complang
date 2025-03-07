
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Pgocaml

let db_host = "host.docker.internal"
let db_port = 5432
let db_name = "complang"
let db_user = "testuser"
let db_password = Sys.getenv "PGPASSWORD"

let conn =
  Pgocaml.connect ~host:db_host ~port:db_port ~database:db_name
    ~user:db_user ~password:db_password ()

let callback _conn req body =
  let meth = req |> Request.meth in
  let uri = req |> Request.uri |> Uri.path in
  match meth, uri with
  | `POST, "/users" ->
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let user = Yojson.Safe.from_string body in
    let name = user |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string in
    let email = user |> Yojson.Safe.Util.member "email" |> Yojson.Safe.Util.to_string in
    let%lwt () =
      conn |> Pgocaml.execute
        "INSERT INTO users (name, email) VALUES ($1, $2)"
        (`String name :: `String email :: [])
    in
    Server.respond_string ~status:`Created ~body:"User created" ()
  | `GET, "/users" ->
    let%lwt rows =
      conn |> Pgocaml.query "SELECT * FROM users"
    in
    let users =
      rows |> List.map (fun row ->
          let id = row |> Pgocaml|int in
          let name = row |> Pgocaml|string in
          let email = row |> Pgocaml|string in
          `Assoc [
            ("id", `Int id);
            ("name", `String name);
            ("email", `String email);
          ]
        )
    in
    Server.respond_string ~status:`OK ~body:(Yojson.Safe.to_string users) ()
  | `GET, path ->
    let id = String.sub path (String.length "/users/" + 1) (String.length path - String.length "/users/") in
    let%lwt row =
      conn |> Pgocaml.query_one "SELECT * FROM users WHERE id = $1" (`Int (int_of_string id))
    in
    match row with
    | Some row ->
      let id = row |> Pgocaml|int in
      let name = row |> Pgocaml|string in
      let email = row |> Pgocaml|string in
      Server.respond_string ~status:`OK ~body:(Yojson.Safe.to_string (`Assoc [
          ("id", `Int id);
          ("name", `String name);
          ("email", `String email);
        ])) ()
    | None -> Server.respond_string ~status:`Not_found ~body:"User not found" ()
  | `PUT, path ->
    let id = String.sub path (String.length "/users/" + 1) (String.length path - String.length "/users/") in
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let user = Yojson.Safe.from_string body in
    let name = user |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string in
    let email = user |> Yojson.Safe.Util.member "email" |> Yojson.Safe.Util.to_string in
    let%lwt () =
      conn |> Pgocaml.execute
        "UPDATE users SET name = $1, email = $2 WHERE id = $3"
        (`String name :: `String email :: `Int (int_of_string id) :: [])
    in
    Server.respond_string ~status:`OK ~body:"User updated" ()
  | `DELETE, path ->
    let id = String.sub path (String.length "/users/" + 1) (String.length path - String.length "/users/") in
    let%lwt () =
      conn |> Pgocaml.execute
        "DELETE FROM users WHERE id = $1"
        (`Int (int_of_string id) :: [])
    in
    Server.respond_string ~status:`OK ~body:"User deleted" ()
  | _ -> Server.respond_string ~status:`Not_found ~body:"Not found" ()

let server =
  Server.create ~port:8080 (Server.make ~callback ())

let () =
  Lwt_main.run (server ())
