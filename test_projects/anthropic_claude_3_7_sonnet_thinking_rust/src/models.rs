
use serde::{Deserialize, Serialize};
use tokio_postgres::Row;

#[derive(Debug, Serialize, Deserialize)]
pub struct User {
    pub id: i32,
    pub name: String,
    pub email: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateUserRequest {
    pub name: String,
    pub email: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UpdateUserRequest {
    pub name: String,
    pub email: String,
}

impl From<Row> for User {
    fn from(row: Row) -> Self {
        User {
            id: row.get("id"),
            name: row.get("name"),
            email: row.get("email"),
        }
    }
}
