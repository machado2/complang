open Lwt.Syntax
open Cohttp_lwt_unix
open Cohttp
open Yojson.Basic.Util

module User = struct
  type t = {
    id : int; name : string; email : string
  } [@@deriving yojson]

  let to_json user = user_to_yojson user
  let of_json json = user_of_yojson json
end

let database_url = "postgresql://testuser:Saloon5-Moody-Observing@host.docker.internal:5432/complang"

let connect_db () = Pgx_lwt.connect database_url

let create_user name email = connect_db () >>= fun conn ->
  let query = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email" in
  Pgx_lwt.execute conn ~name:"create_user" ~params:[|name; email|] query >>= fun result ->
  match Pgx.Result.get_row result 0 with
  | Some row -> (
    let id = Pgx.Value.get_int (Pgx.Row.get row 0) in
    let name = Pgx.Value.get_string (Pgx.Row.get row 1) in
    let email = Pgx.Value.get_string (Pgx.Row.get row 2) in
    let user = { User.id; name; email } in
    Pgx_lwt.close conn >|= fun () -> user)
  | None -> Lwt.fail_with "Failed to create user"

let get_users () = connect_db () >>= fun conn ->
  let query = "SELECT id, name, email FROM users" in
  Pgx_lwt.execute conn ~name:"get_users" query >>= fun result ->
  let users = List.filter_map (
    fun row ->
      try
        let id = Pgx.Value.get_int (Pgx.Row.get row 0) in
        let name = Pgx.Value.get_string (Pgx.Row.get row 1) in
        let email = Pgx.Value.get_string (Pgx.Row.get row 2) in
        Some { User.id; name; email }
      with _ -> None
  ) (Pgx.Result.to_list result) in
  Pgx_lwt.close conn >|= fun () -> users

let get_user id = connect_db () >>= fun conn ->
  let query = "SELECT id, name, email FROM users WHERE id = $1" in
  Pgx_lwt.execute conn ~name:"get_user" ~params:[|string_of_int id|] query >>= fun result ->
  match Pgx.Result.get_row result 0 with
  | Some row -> (
    let id = Pgx.Value.get_int (Pgx.Row.get row 0) in
    let name = Pgx.Value.get_string (Pgx.Row.get row 1) in
    let email = Pgx.Value.get_string (Pgx.Row.get row 2) in
    let user = { User.id; name; email } in
    Pgx_lwt.close conn >|= fun () -> Some user)
  | None -> Pgx_lwt.close conn >|= fun () -> None

let update_user id name email = connect_db () >>= fun conn ->
  let query = "UPDATE users SET name = $1, email = $2 WHERE id = $3" in
  Pgx_lwt.execute conn ~name:"update_user" ~params:[|name; email; string_of_int id|] query >>= fun result ->
  let rows_affected = Pgx.Result.num_rows result in
  Pgx_lwt.close conn >|= fun () -> rows_affected > 0

let delete_user id = connect_db () >>= fun conn ->
  let query = "DELETE FROM users WHERE id = $1" in
  Pgx_lwt.execute conn ~name:"delete_user" ~params:[|string_of_int id|] query >>= fun result ->
  let rows_affected = Pgx.Result.num_rows result in
  Pgx_lwt.close conn >|= fun () -> rows_affected > 0

let json_to_user body = Lwt.catch (
  fun () ->
    let* json_string = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Basic.from_string json_string in
    let name = json |> member "name" |> to_string in
    let email = json |> member "email" |> to_string in
    Lwt.return (Some (name, email))
) (
  fun _ -> Lwt.return None
)

let handler ~conn ~body req uri =  
  let meth = req |> Request.meth in
  let path = Uri.path uri in
  match meth, path with
  | `POST, "/users" -> (
    json_to_user body >>= fun user_data ->
    match user_data with
    | Some (name, email) ->
      create_user name email >>= fun user ->
      let json = User.to_json user in
      let body = Yojson.Basic.to_string json |> Cohttp_lwt.Body.of_string in
      Server.respond ~status:`Created ~body ())    
    | None -> Server.respond ~status:`Bad_request ~body: (Cohttp_lwt.Body.of_string "Invalid JSON") ()
  )
  | `GET, "/users" -> (
    get_users () >>= fun users ->
    let json = `List (List.map User.to_json users) in
    let body = Yojson.Basic.to_string json |> Cohttp_lwt.Body.of_string in
    Server.respond ~status:`OK ~body ()
  )
  | `GET, path when String.starts_with ~prefix:"/users/" path -> (
    let id_str = String.sub path 7 (String.length path - 7) in
    match int_of_string_opt id_str with
    | Some id ->
      get_user id >>= fun user_opt ->
      match user_opt with
      | Some user ->
        let json = User.to_json user in
        let body = Yojson.Basic.to_string json |> Cohttp_lwt.Body.of_string in
        Server.respond ~status:`OK ~body ()
      | None -> Server.respond ~status:`Not_found ~body:(Cohttp_lwt.Body.of_string "User not found") ()
    | None -> Server.respond ~status:`Bad_request ~body:(Cohttp_lwt.Body.of_string "Invalid user ID") ()
  )
  | `PUT, path when String.starts_with ~prefix:"/users/" path -> (
    let id_str = String.sub path 7 (String.length path - 7) in    
    match int_of_string_opt id_str with
    | Some id ->
      json_to_user body >>= fun user_data ->
      match user_data with
      | Some (name, email) ->
        update_user id name email >>= fun updated ->
        if updated then
          Server.respond ~status:`No_content ()
        else
          Server.respond ~status:`Not_found ~body:(Cohttp_lwt.Body.of_string "User not found") ()
      | None -> Server.respond ~status:`Bad_request ~body:(Cohttp_lwt.Body.of_string "Invalid JSON") ()
    | None -> Server.respond ~status:`Bad_request ~body:(Cohttp_lwt.Body.of_string "Invalid user ID") ()
  )
  | `DELETE, path when String.starts_with ~prefix:"/users/" path -> (
    let id_str = String.sub path 7 (String.length path - 7) in
    match int_of_string_opt id_str with
    | Some id ->
      delete_user id >>= fun deleted ->
      if deleted then
        Server.respond ~status:`No_content ()
      else
        Server.respond ~status:`Not_found ~body:(Cohttp_lwt.Body.of_string "User not found") ()
    | None -> Server.respond ~status:`Bad_request ~body:(Cohttp_lwt.Body.of_string "Invalid user ID") ()
  )
  | _ -> Server.respond ~status:`Not_found ~body:(Cohttp_lwt.Body.of_string "Route not found") ()

let serve_req ~conn ~body req uri = 
  let* response = handler ~conn ~body req uri in
  Lwt.return response

let run () =
  let port = 8080 in
  (
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let* server =
      Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback:serve_req ~conn_closed:(
        fun _ ->
          print_endline "Connection closed"
      ) ())
    in
    Printf.printf "Server listening on port %d\n" port;
    server
  )

let () = Lwt_main.run (run ())