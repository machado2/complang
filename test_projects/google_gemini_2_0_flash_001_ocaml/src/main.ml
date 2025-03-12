open Lwt.Infix
open Dream

let db_host = "host.docker.internal"
let db_port = 5432
let db_name = "complang"
let db_user = "testuser"
let db_password = try Sys.getenv "PGPASSWORD" with Not_found -> ""

let connect () =
  Printf.sprintf "host=%s port=%d dbname=%s user=%s password=%s"
    db_host db_port db_name db_user db_password
  |> Postgresql.connect

let json_of_user (id, name, email) =
  `Assoc [
    ("id", `Int id);
    ("name", `String name);
    ("email", `String email);
  ]

let user_of_json json =
  let open Yojson.Basic.Util in
  let name = json |> member "name" |> to_string in
  let email = json |> member "email" |> to_string in
  (name, email)

let list_users pool _ =
  Lwt_pool.use pool (fun db ->
    db#exec "SELECT id, name, email FROM users" >|= fun result ->
    let users = List.map (fun row ->
      (row#getint 0, row#get 1, row#get 2)
    ) result#get_all
    in
    users
  )
  >|= fun users ->
  users
  |> List.map json_of_user
  |> Yojson.Basic.to_string
  |> fun body -> json ~code:`OK body

let get_user pool id _ =
  Lwt_pool.use pool (fun db ->
    db#exec_params "SELECT id, name, email FROM users WHERE id = $1" [|string_of_int id|] >|= fun result ->
    match result#get_all with
    | [] -> None
    | row :: _ ->
      let id = row#getint 0 in
      let name = row#get 1 in
      let email = row#get 2 in
      Some (id, name, email)
  )
  >|= function
  | Some user ->
    user
    |> json_of_user
    |> Yojson.Basic.to_string
    |> fun body -> json ~code:`OK body
  | None -> empty `Not_Found

let create_user pool request =
  Dream.body request >>= fun body ->
  match Yojson.Basic.from_string body |> user_of_json with
  | (name, email) ->
    Lwt_pool.use pool (fun db ->
      db#exec_params "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email" [|name; email|] >|= fun result ->
      match result#get_all with
      | row :: _ ->
        let id = row#getint 0 in
        (id, name, email)
      | _ -> failwith "Failed to retrieve inserted user"
    )
    >|= fun user ->
    user
    |> json_of_user
    |> Yojson.Basic.to_string
    |> fun body -> json ~code:`Created body
  | exception Yojson.Json_error _ ->
    empty `Bad_Request

let update_user pool id request =
  Dream.body request >>= fun body ->
  match Yojson.Basic.from_string body |> user_of_json with
  | (name, email) ->
    Lwt_pool.use pool (fun db ->
      db#exec_params "UPDATE users SET name = $1, email = $2 WHERE id = $3" [|name; email; string_of_int id|] >>= fun _ ->
      Lwt.return (db#affected_rows ())
    )
    >|= fun affected_rows ->
    if affected_rows > 0 then
      empty `No_Content
    else
      empty `Not_Found
  | exception Yojson.Json_error _ ->
    empty `Bad_Request

let delete_user pool id _ =
  Lwt_pool.use pool (fun db ->
    db#exec_params "DELETE FROM users WHERE id = $1" [|string_of_int id|] >>= fun _ ->
    Lwt.return (db#affected_rows ())
  )
  >|= fun affected_rows ->
  if affected_rows > 0 then
    empty `No_Content
  else
    empty `Not_Found

let () =
  Lwt_main.run (
    let pool = Lwt_pool.create 10 (fun () ->
      Lwt.return (connect ())
    ) in
    Dream.initialize_log ~level:`Debug ();
    Dream.run ~port:8080
    @@ Dream.logger
    @@ middleware (fun next request ->
      try%lwt
        next request
      with exn ->
        Dream.error (fun log ->
          log "Request raised %s" (Printexc.to_string exn));
        raise exn
    )
    @@ Dream.router [
         Dream.get "/users" (list_users pool);
         Dream.get "/users/:id" (fun request ->
           Dream.param request "id" |> int_of_string |> get_user pool
         );
         Dream.post "/users" (create_user pool);
         Dream.put "/users/:id" (fun request ->
           Dream.param request "id" |> int_of_string |> update_user pool request
         );
         Dream.delete "/users/:id" (fun request ->
           Dream.param request "id" |> int_of_string |> delete_user pool
         );
       ]
  )
