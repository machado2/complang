
type t = {
  id : int option; (* Option because new users won't have an ID yet *)
  name : string;
  email : string;
} [@@deriving yojson]

(* For creating a new user without an ID *)
let create name email = { id = None; name; email }

(* For retrieving a user with an ID from the database *)
let create_with_id id name email = { id = Some id; name; email }

(* Convert from database row to user object *)
let of_tuple (id, name, email) = create_with_id id name email

(* Convert to database parameters *)
let to_params user =
  let id = match user.id with Some id -> id | None -> 0 in
  (id, user.name, user.email)

(* For creating JSON response without null ID *)
let to_response_json user =
  let id = match user.id with
    | Some id -> id
    | None -> failwith "User ID is required for response"
  in
  `Assoc [
    ("id", `Int id);
    ("name", `String user.name);
    ("email", `String user.email)
  ]

(* For parsing JSON from request body for creating/updating *)
let of_request_json json =
  match Yojson.Safe.Util.(
    try
      let name = json |> member "name" |> to_string in
      let email = json |> member "email" |> to_string in
      Some (create name email)
    with _ -> None
  ) with
  | Some user -> Ok user
  | None -> Error "Invalid user JSON"
