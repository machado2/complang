open Dream
open Lwt.Syntax
open Postgresql

let ( let* ) = Lwt.bind

let pg_host = "host.docker.internal"
let pg_port = 5432
let pg_database = "test_google_gemini_2_0_flash_001_ocaml"
let pg_user = "postgres"
let pg_password = Sys.getenv "PGPASSWORD"

let connect () = new Postgresql.connection ~host:pg_host ~port:pg_port ~dbname:pg_database ~user:pg_user ~password:pg_password ()

let create_table () = 
  let db = connect () in
  try
    db#exec "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT, email TEXT)";
    db#close;
    Lwt.return ()  
  with e -> 
    Printf.eprintf "Error creating table: %s\n" (Printexc.to_string e);
    db#close;
    Lwt.fail (Failure "Failed to create table")

let check_table_exists () = 
  let db = connect () in
  try
    let result = db#exec "SELECT EXISTS (SELECT FROM pg_tables WHERE tablename = 'users')" in
    let exists = result#get 0 0 = "t" in
    db#close;
    Lwt.return exists
  with e ->
    Printf.eprintf "check_table error: %s\n" (Printexc.to_string e);
    db#close;
    Lwt.fail (Failure "Failed to check table existence")

let insert_user name email = 
  let db = connect () in
  try
    let query = Printf.sprintf "INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email" (Sql.quote name) (Sql.quote email) in
    let result = db#exec query in
    let id = int_of_string (result#get 0 0) in
    let name = result#get 0 1 in
    let email = result#get 0 2 in
    db#close;
    Lwt.return (`Json (Printf.sprintf "{\"id\": %d, \"name\": \"%s\", \"email\": \"%s\"}" id name email))
  with e ->
    Printf.eprintf "insert_user error: %s\n" (Printexc.to_string e);
    db#close;
    Lwt.fail (Failure "Failed to insert user")


let get_all_users () = 
  let db = connect () in
  try
    let result = db#exec "SELECT id, name, email FROM users" in
    let num_rows = result#ntuples in
    let users = ref [] in
    for i = 0 to num_rows - 1 do
      let id = int_of_string (result#get i 0) in
      let name = result#get i 1 in
      let email = result#get i 2 in
      users := Printf.sprintf "{\"id\": %d, \"name\": \"%s\", \"email\": \"%s\"}" id name email :: !users
    done;
    db#close;
    Lwt.return (`Json (Printf.sprintf "[%s]" (String.concat ", " (List.rev !users))))
  with e ->
    Printf.eprintf "get_all_users error: %s\n" (Printexc.to_string e);
    db#close;
    Lwt.fail (Failure "Failed to get all users")

let get_user id_str = 
  let db = connect () in
  try
    let id = int_of_string id_str in
    let query = Printf.sprintf "SELECT id, name, email FROM users WHERE id = %d" id in
    let result = db#exec query in
    if result#ntuples = 0 then (
      db#close;
      Lwt.return `Not_Found
    ) else (
      let id = int_of_string (result#get 0 0) in
      let name = result#get 0 1 in
      let email = result#get 0 2 in
      db#close;
      Lwt.return (`Json (Printf.sprintf "{\"id\": %d, \"name\": \"%s\", \"email\": \"%s\"}" id name email))
    )
  with e ->
    Printf.eprintf "get_user error: %s\n" (Printexc.to_string e);
    db#close;
    Lwt.fail (Failure "Failed to get user")

let update_user id_str name email = 
  let db = connect () in
  try
    let id = int_of_string id_str in
    let query = Printf.sprintf "UPDATE users SET name = %s, email = %s WHERE id = %d" (Sql.quote name) (Sql.quote email) id in
    let result = db#exec query in
    if result#affected_rows = 0 then (
      db#close;
      Lwt.return `Not_Found
    ) else (
      db#close;
      Lwt.return `OK
    )
  with e ->
    Printf.eprintf "update_user error: %s\n" (Printexc.to_string e);
    db#close;
    Lwt.fail (Failure "Failed to update user")

let delete_user id_str = 
  let db = connect () in
  try
    let id = int_of_string id_str in
    let query = Printf.sprintf "DELETE FROM users WHERE id = %d" id in
    let result = db#exec query in
    if result#affected_rows = 0 then (
      db#close;
      Lwt.return `Not_Found
    ) else (
      db#close;
      Lwt.return `OK
    )
  with e ->
    Printf.eprintf "delete_user error: %s\n" (Printexc.to_string e);
    db#close;
    Lwt.fail (Failure "Failed to delete user")


let initialize_db () = 
  let* exists = check_table_exists () in
  if not exists then (
    let* () = create_table () in
    Lwt.return () 
  ) else (
    Lwt.return ()
  )

let () = 
  Lwt_main.run (initialize_db ());
  Dream.run ~port:8080
  @@ Dream.router [
    Dream.post "/users" (fun request ->
      let* body = Dream.body request in
      let json = Yojson.Basic.from_string body in
      let name = Yojson.Basic.Util.(json |> member "name" |> to_string) in
      let email = Yojson.Basic.Util.(json |> member "email" |> to_string) in
      let* result = insert_user name email in
      match result with
      | `Json user -> Dream.json ~status:`Created user
      | _ -> Dream.respond ~status:`Internal_Server_Error "Failed to insert user"
    );
    Dream.get "/users" (fun _ ->
      let* result = get_all_users () in
      match result with
      | `Json users -> Dream.json users
      | _ -> Dream.respond ~status:`Internal_Server_Error "Failed to get all users"
    );
    Dream.get "/users/:id" (fun request ->
      let id = Dream.param request "id" in
      let* result = get_user id in
      match result with
      | `Json user -> Dream.json user
      | `Not_Found -> Dream.respond ~status:`Not_Found "User not found"
      | _ -> Dream.respond ~status:`Internal_Server_Error "Failed to get user"
    );
    Dream.put "/users/:id" (fun request ->
      let id = Dream.param request "id" in
      let* body = Dream.body request in
      let json = Yojson.Basic.from_string body in
      let name = Yojson.Basic.Util.(json |> member "name" |> to_string) in
      let email = Yojson.Basic.Util.(json |> member "email" |> to_string) in
      let* result = update_user id name email in
      match result with
      | `OK -> Dream.respond ~status:`No_Content ""
      | `Not_Found -> Dream.respond ~status:`Not_Found "User not found"
      | _ -> Dream.respond ~status:`Internal_Server_Error "Failed to update user"
    );
    Dream.delete "/users/:id" (fun request ->
      let id = Dream.param request "id" in
      let* result = delete_user id in
      match result with
      | `OK -> Dream.respond ~status:`No_Content ""
      | `Not_Found -> Dream.respond ~status:`Not_Found "User not found"
      | _ -> Dream.respond ~status:`Internal_Server_Error "Failed to delete user"
    );
  ]
  