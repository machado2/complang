
open Lwt.Syntax
open Cohttp_lwt_unix

let user_table = "users";;

let db_host = "host.docker.internal";;
let db_port = 5432;;let db_name = "complang";;
let db_user = "testuser";;
let pg_password =
  Sys.getenv_opt "PGPASSWORD" |> Option.value ~default:"Saloon5-Moody-Observing"
;;

let db_connect_string =
  Printf.sprintf "host=%s port=%d dbname=%s user=%s password=%s" db_host db_port db_name db_user pg_password
;;

let connect () =
  try%lwt
    let%lwt c = Postgresql.connect ~host:db_host ~port:db_port ~dbname:db_name ~user:db_user ~password:pg_password () in
    Lwt.return c
  with exn ->
    Printf.eprintf "Error connecting to database: %s\n" (Printexc.to_string exn);
    Lwt.fail exn
;;

module User = struct
  type t = {
    id: int;
    name: string;
    email: string;
  } [@@deriving yojson]

  let to_json user =
    Yojson.Safe.to_string (to_yojson user)

  let of_json json =
    match of_yojson (Yojson.Safe.from_string json) with
    | Ok user -> user
    | Error msg -> failwith msg
end

let get_users conn =
  try%lwt
    let%lwt result = conn#exec ~expect:[| `Tuples |] "SELECT id, name, email FROM users" in
    let users = List.init result#ntuples (fun i ->
      let row = result#getrow i in
      let id = int_of_string (Array.get row 0) in
      let name = Array.get row 1 in
      let email = Array.get row 2 in
      { User.id; name; email }
    ) in
    Lwt.return users
  with exn ->
    Printf.eprintf "Error getting users: %s\n" (Printexc.to_string exn);
    Lwt.fail exn

let get_user conn id =
  try%lwt
    let%lwt result = conn#exec ~expect:[| `Tuples |] ~params:[| string_of_int id |] "SELECT id, name, email FROM users WHERE id = $1" in
    if result#ntuples = 0 then
      Lwt.return_none
    else
      let row = result#getrow 0 in
      let id = int_of_string (Array.get row 0) in
      let name = Array.get row 1 in
      let email = Array.get row 2 in
      Lwt.return_some { User.id; name; email }
  with exn ->
    Printf.eprintf "Error getting user: %s\n" (Printexc.to_string exn);
    Lwt.fail exn

let create_user conn name email =
  try%lwt
    let%lwt result = conn#exec ~expect:[| `Tuples |] ~params:[| name; email |] "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id" in
    let row = result#getrow 0 in
    let id = int_of_string (Array.get row 0) in
    Lwt.return id
  with exn ->
    Printf.eprintf "Error creating user: %s\n" (Printexc.to_string exn);
    Lwt.fail exn


let update_user conn id name email =
  try%lwt
    let%lwt result = conn#exec ~expect:[| `Command |] ~params:[| string_of_int id; name; email |] "UPDATE users SET name = $2, email = $3 WHERE id = $1" in
    Lwt.return (result#affected > 0)
  with exn ->
    Printf.eprintf "Error updating user: %s\n" (Printexc.to_string exn);
    Lwt.fail exn

let delete_user conn id =
  try%lwt
    let%lwt result = conn#exec ~expect:[| `Command |] ~params:[| string_of_int id |] "DELETE FROM users WHERE id = $1" in
    Lwt.return (result#affected > 0)
  with exn ->
    Printf.eprintf "Error deleting user: %s\n" (Printexc.to_string exn);
    Lwt.fail exn

let handle_request conn req body =
  let uri = Request.uri req in
  let path = Uri.path uri in
  match Request.meth req with
  | `POST ->
    if path = "/users" then
      (* Create a user *)
      match body with
      | Some body ->
        let%lwt body_string = Cohttp_lwt.Body.to_string body in
        let json = Yojson.Safe.from_string body_string in
        let name = Yojson.Safe.Util.member "name" json |> Yojson.Safe.Util.to_string in
        let email = Yojson.Safe.Util.member "email" json |> Yojson.Safe.Util.to_string in
        let%lwt id = create_user conn name email in
        let user = { User.id; name; email } in
        let json_response = User.to_json user in
        let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
        Server.respond_string ~headers ~status:`Created ~body:json_response ()
      | None -> Server.respond_string ~status:`Bad_request ~body:"Missing body" ()
    else
      Server.respond_string ~status:`Not_found ~body:"Not found" ()
  | `GET ->
    if path = "/users" then
      (* Get all users *)
      let%lwt users = get_users conn in
      let json_response = `List (List.map User.to_yojson users) |> Yojson.Safe.to_string in
      let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
      Server.respond_string ~headers ~status:`OK ~body:json_response ()
    else if String.starts_with ~prefix:"/users/" path then
      (* Get a user by ID *)
      let id_str = String.sub path 7 (String.length path - 7) in
      match int_of_string_opt id_str with
      | Some id ->
        let%lwt user_opt = get_user conn id in
        begin match user_opt with
        | Some user ->
          let json_response = User.to_json user in
          let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
          Server.respond_string ~headers ~status:`OK ~body:json_response ()
        | None -> Server.respond ~status:`Not_found ~body:`Empty ()
        end
      | None -> Server.respond_string ~status:`Bad_request ~body:"Invalid ID" ()
    else
      Server.respond_string ~status:`Not_found ~body:"Not found" ()
  | `PUT ->
    if String.starts_with ~prefix:"/users/" path then
      (* Update a user by ID *)
      let id_str = String.sub path 7 (String.length path - 7) in
      match int_of_string_opt id_str with
      | Some id ->
        match body with
        | Some body ->
          let%lwt body_string = Cohttp_lwt.Body.to_string body in
          let json = Yojson.Safe.from_string body_string in
          let name = Yojson.Safe.Util.member "name" json |> Yojson.Safe.Util.to_string in
          let email = Yojson.Safe.Util.member "email" json |> Yojson.Safe.Util.to_string in
          let%lwt updated = update_user conn id name email in
          if updated then
            Server.respond ~status:`No_content ~body:`Empty ()
          else
            Server.respond ~status:`Not_found ~body:`Empty ()
        | None -> Server.respond_string ~status:`Bad_request ~body:"Missing body" ()
      | None -> Server.respond_string ~status:`Bad_request ~body:"Invalid ID" ()
    else
      Server.respond_string ~status:`Not_found ~body:"Not found" ()
  | `DELETE ->
    if String.starts_with ~prefix:"/users/" path then
      (* Delete a user by ID *)
      let id_str = String.sub path 7 (String.length path - 7) in
      match int_of_string_opt id_str with
      | Some id ->
        let%lwt deleted = delete_user conn id in
        if deleted then
          Server.respond ~status:`No_content ~body:`Empty ()
        else
          Server.respond ~status:`Not_found ~body:`Empty ()
      | None -> Server.respond_string ~status:`Bad_request ~body:"Invalid ID" ()
    else
      Server.respond_string ~status:`Not_found ~body:"Not found" ()
  | _ ->
    Server.respond_string ~status:`Method_not_allowed ~body:"Method not allowed" ()

let start_server conn () =
  let port = 8080 in
  let callback _conn req body =
    let body' = match body with
    | `Empty -> None
    | `String s -> Some (Cohttp_lwt.Body.of_string s)
    | `Stream s ->
      let stream = Lwt_stream.map (fun chunk -> Bytes.of_string chunk) s in
      Some (Cohttp_lwt.Body.of_stream stream)
    in
    handle_request conn req body'
  in
  let server = Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ()) in
  Printf.printf "Server started on port %d\n" port;
  fst (Lwt.wait ())

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);

  Lwt_main.run (
    let%lwt conn = connect () in
    start_server conn ()
  )
