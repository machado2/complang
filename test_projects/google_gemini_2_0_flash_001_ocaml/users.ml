type user = {
  id : int;
  name : string;
  email : string;
}

let user_to_json user = 
  Printf.sprintf "{\"id\": %d, \"name\": \"%s\", \"email\": \"%s\"}" user.id user.name user.email

let user_from_json json_string = 
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_string json_string in
  let id = json |> member "id" |> to_int in
  let name = json |> member "name" |> to_string in
  let email = json |> member "email" |> to_string in
  { id; name; email }
