
(* api.ml *)
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix
open Pgx

let db_config = { dbname = "complang"; user = "testuser"; password = Sys.getenv "PGPASSWORD"; host = "host.docker.internal"; port = 5432 }

let db_connection () =
  let%lwt conn = Connection.connect ~dbname:db_config.dbname ~user:db_config.user ~password:db_config.password ~host:db_config.host ~port:db_config.port () in
  Lwt.return conn

let create_user conn name email =
  let%lwt result = Connection.exec conn "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id" [| `String name; `String email |] in
  match result with
  | [| row |] -> Lwt.return (Some (row.(0) :> int))
  | _ -> Lwt.return None

let all_users conn =
  let%lwt result = Connection.exec conn "SELECT id, name, email FROM users" [||] in
  let users = Array.map (fun row -> `Assoc [("id", `Int (row.(0) :> int)); ("name", `String row.(1)); ("email", `String row.(2))]) result in
  Lwt.return users

let user_by_id conn id =
  let%lwt result = Connection.exec conn "SELECT id, name, email FROM users WHERE id = $1" [| `Int id |] in
  match result with
  | [| row |] -> Lwt.return (Some (row.(0) :> int, row.(1), row.(2)))
  | _ -> Lwt.return None

let update_user conn id name email =
  let%lwt result = Connection.exec conn "UPDATE users SET name = $1, email = $2 WHERE id = $3" [| `String name; `String email; `Int id |] in
  Lwt.return (result > 0)

let delete_user conn id =
  let%lwt result = Connection.exec conn "DELETE FROM users WHERE id = $1" [| `Int id |] in
  Lwt.return (result > 0)

let routes conn =
  let open Cohttp_lwt in
  let open Cohttp in
  Server.make ~callback:(fun ~body _ ->
    let%lwt body = Cohttp_lwt.Body.to_string body in
    let json_response ~status data =
      let body = Yojson.Basic.to_string data in
      Server.respond_string ~status ~body () in
    try match Cohttp_lwt.Body.to_string body with
    | _ -> (* parse json; handle each method based on path *)
      Lwt.return (json_response ~status:`Ok (`String "Not implemented yet"))
    with _ -> Lwt.return (json_response ~status:`Bad_request (`String "Invalid request"))
  ) ()

let start_server =
  let%lwt conn = db_connection () in
  let _ = Lwt_main.run (Server.create ~mode:(`TCP (`Port 8080)) (routes conn)) in
  Lwt.return ()

let () = Lwt_main.run start_server
