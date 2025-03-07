
use super::schema::users;
use serde::{Serialize, Deserialize};  // Ensure to import Serialize and Deserialize

#[derive(Queryable, Serialize, Deserialize)]
pub struct User {
    pub id: i32,
    pub name: String,
    pub email: String,
}

#[derive(Insertable, Serialize, Deserialize)]  // Ensure to add Serialize and Deserialize
#[table_name = "users"]
pub struct NewUser {
    pub name: String,
    pub email: String,
}
