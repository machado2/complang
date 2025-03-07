type user = {
  id : int;
  name : string;
  email : string;
} [@@deriving yojson]

let user_of_tuple (id, name, email) : user = { id; name; email }