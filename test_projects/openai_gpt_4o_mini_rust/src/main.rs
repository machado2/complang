
use actix_web::{web, App, HttpServer, Responder, HttpResponse};
use diesel::prelude::*;
use dotenv::dotenv;
use std::env;
use serde::{Serialize, Deserialize};  // Ensure to import Serialize and Deserialize

#[macro_use]
extern crate diesel;

pub mod schema;
pub mod models;

use models::{User, NewUser};

// Function to create a new user
async fn create_user(user: web::Json<NewUser>) -> impl Responder {
    let conn = establish_connection();
    let new_user = NewUser {
        name: user.name.clone(),
        email: user.email.clone(),
    };

    diesel::insert_into(schema::users::table)
        .values(&new_user)
        .execute(&mut conn)  // Make the connection mutable
        .expect("Error creating new user");

    HttpResponse::Created().json(new_user)
}

// Function to get all users
async fn get_users() -> impl Responder {
    let conn = establish_connection();
    let results = schema::users::table.load::<User>(&mut conn)  // Make the connection mutable
        .expect("Error loading users");

    HttpResponse::Ok().json(results)
}

// Function to handle getting a user by id
async fn get_user_by_id(web::Path(id): web::Path<i32>) -> impl Responder {
    let conn = establish_connection();
    let user = schema::users::table.find(id).first::<User>(&mut conn).optional();  // Make the connection mutable

    match user {
        Ok(Some(user)) => HttpResponse::Ok().json(user),
        _ => HttpResponse::NotFound().finish(),
    }
}

// Function to handle user updates
async fn update_user(web::Path(id): web::Path<i32>, user: web::Json<NewUser>) -> impl Responder {
    let conn = establish_connection();
    
    let updated_rows = diesel::update(schema::users::table.find(id))
        .set((schema::users::name.eq(&user.name), schema::users::email.eq(&user.email)))
        .execute(&mut conn);  // Make the connection mutable

    match updated_rows {
        Ok(_) => HttpResponse::Ok().finish(),
        _ => HttpResponse::NotFound().finish(),
    }
}

// Function to delete a user
async fn delete_user(web::Path(id): web::Path<i32>) -> impl Responder {
    let conn = establish_connection();

    let deleted_rows = diesel::delete(schema::users::table.find(id)).execute(&mut conn);  // Make the connection mutable
    match deleted_rows {
        Ok(_) => HttpResponse::Ok().finish(),
        _ => HttpResponse::NotFound().finish(),
    }
}

// Establish database connection
fn establish_connection() -> PgConnection {
    dotenv().ok();
    let database_url = "postgres://testuser:Saloon5-Moody-Observing@host.docker.internal:5432/complang";
    PgConnection::establish(&database_url).expect(&format!("Error connecting to {}", database_url))
}

// Main Function
#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/users", web::post().to(create_user))
            .route("/users", web::get().to(get_users))
            .route("/users/{id}", web::get().to(get_user_by_id))
            .route("/users/{id}", web::put().to(update_user))
            .route("/users/{id}", web::delete().to(delete_user))
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
