
use actix_web::{web, App, HttpResponse, HttpServer, Responder};
use serde::{Deserialize, Serialize};
use sqlx::{postgres::PgPoolOptions, PgPool};

#[derive(Serialize, Deserialize, sqlx::FromRow)]
struct User {
    id: i32,
    name: String,
    email: String,
}

#[derive(Serialize, Deserialize)]
struct CreateUser {
    name: String,
    email: String,
}

#[derive(Serialize, Deserialize)]
struct UpdateUser {
    name: String,
    email: String,
}

async fn create_user(pool: web::Data<PgPool>, item: web::Json<CreateUser>) -> impl Responder {
    let result = sqlx::query_as::<_, User>(
        "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
    )
    .bind(&item.name)
    .bind(&item.email)
    .fetch_one(pool.get_ref())
    .await;

    match result {
        Ok(user) => HttpResponse::Created().json(user),
        Err(e) => {
            eprintln!("Error inserting user: {:?}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

async fn get_users(pool: web::Data<PgPool>) -> impl Responder {
    let result = sqlx::query_as::<_, User>("SELECT id, name, email FROM users")
        .fetch_all(pool.get_ref())
        .await;
    match result {
        Ok(users) => HttpResponse::Ok().json(users),
        Err(e) => {
            eprintln!("Error fetching users: {:?}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

async fn get_user(pool: web::Data<PgPool>, user_id: web::Path<i32>) -> impl Responder {
    let result = sqlx::query_as::<_, User>("SELECT id, name, email FROM users WHERE id = $1")
        .bind(user_id.into_inner())
        .fetch_optional(pool.get_ref())
        .await;
    match result {
        Ok(Some(user)) => HttpResponse::Ok().json(user),
        Ok(None) => HttpResponse::NotFound().finish(),
        Err(e) => {
            eprintln!("Error fetching user: {:?}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

async fn update_user(pool: web::Data<PgPool>, user_id: web::Path<i32>, item: web::Json<UpdateUser>) -> impl Responder {
    let result = sqlx::query("UPDATE users SET name = $1, email = $2 WHERE id = $3")
        .bind(&item.name)
        .bind(&item.email)
        .bind(user_id.into_inner())
        .execute(pool.get_ref())
        .await;
    match result {
        Ok(r) if r.rows_affected() > 0 => HttpResponse::Ok().finish(),
        Ok(_) => HttpResponse::NotFound().finish(),
        Err(e) => {
            eprintln!("Error updating user: {:?}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

async fn delete_user(pool: web::Data<PgPool>, user_id: web::Path<i32>) -> impl Responder {
    let result = sqlx::query("DELETE FROM users WHERE id = $1")
        .bind(user_id.into_inner())
        .execute(pool.get_ref())
        .await;
    match result {
        Ok(r) if r.rows_affected() > 0 => HttpResponse::Ok().finish(),
        Ok(_) => HttpResponse::NotFound().finish(),
        Err(e) => {
            eprintln!("Error deleting user: {:?}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Retrieve the PGPASSWORD from environment variable.
    let pg_password = std::env::var("PGPASSWORD").expect("PGPASSWORD env var not set");
    let database_url = format!("postgres://testuser:{}@host.docker.internal:5432/complang", pg_password);

    let pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&database_url)
        .await
        .expect("Failed to create pool");

    println!("Starting server on 0.0.0.0:8080");

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(pool.clone()))
            .route("/users", web::post().to(create_user))
            .route("/users", web::get().to(get_users))
            .route("/users/{id}", web::get().to(get_user))
            .route("/users/{id}", web::put().to(update_user))
            .route("/users/{id}", web::delete().to(delete_user))
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
