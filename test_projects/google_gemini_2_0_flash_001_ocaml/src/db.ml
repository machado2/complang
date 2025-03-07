open Lwt.Syntax

let connect () =
  let pg_host = "host.docker.internal" in
  let pg_port = 5432 in
  let pg_user = "testuser" in
  let pg_password = Sys.getenv_opt "PGPASSWORD" |> Option.value ~default:"Saloon5-Moody-Observing" in
  let pg_database = "complang" in

  let connection_string = Printf.sprintf
    "host=%s port=%d user=%s password=%s dbname=%s"
    pg_host pg_port pg_user pg_password pg_database
  in
  match Pgx.connect connection_string with
  | Ok conn -> Lwt.return conn
  | Error err ->
    Printf.eprintf "Error connecting to database: %s\n" (Pgx.string_of_error err);
    failwith "Failed to connect to database"
