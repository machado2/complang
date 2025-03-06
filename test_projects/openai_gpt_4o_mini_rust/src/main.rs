use actix_web::{post, get, put, delete, web, App, HttpServer, HttpResponse, Responder};
use serde::{Deserialize, Serialize};
use sqlx::PgPool;

#[derive(Debug, Serialize, Deserialize)]
struct User {
    id: Option<i32>,
    name: String,
    email: String,
}

#[post("/users")]
async fn create_user(user: web::Json<User>, pool: web::Data<PgPool>) -> impl Responder {
    let user = user.into_inner();
    let rec = sqlx::query!("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", user.name, user.email)
        .fetch_one(pool.get_ref())
        .await
        .unwrap();
    HttpResponse::Created().json(rec)
}

#[get("/users")]
async fn get_users(pool: web::Data<PgPool>) -> impl Responder {
    let users = sqlx::query!("SELECT id, name, email FROM users")
        .fetch_all(pool.get_ref())
        .await
        .unwrap();
    HttpResponse::Ok().json(users)
}

#[get("/users/{id}")]
async fn get_user(web::Path(id): web::Path<i32>, pool: web::Data<PgPool>) -> impl Responder {
    let user = sqlx::query!("SELECT id, name, email FROM users WHERE id = $1", id)
        .fetch_optional(pool.get_ref())
        .await
        .unwrap();
    match user {
        Some(user) => HttpResponse::Ok().json(user),
        None => HttpResponse::NotFound().finish(),
    }
}

#[put("/users/{id}")]
async fn update_user(web::Path(id): web::Path<i32>, user: web::Json<User>, pool: web::Data<PgPool>) -> impl Responder {
    let user = user.into_inner();
    let updated = sqlx::query!("UPDATE users SET name = $1, email = $2 WHERE id = $3", user.name, user.email, id)
        .execute(pool.get_ref())
        .await
        .unwrap();
    if updated.rows_affected() == 0 {
        HttpResponse::NotFound().finish()
    } else {
        HttpResponse::Ok().finish()
    }
}

#[delete("/users/{id}")]
async fn delete_user(web::Path(id): web::Path<i32>, pool: web::Data<PgPool>) -> impl Responder {
    let deleted = sqlx::query!("DELETE FROM users WHERE id = $1", id)
        .execute(pool.get_ref())
        .await
        .unwrap();
    if deleted.rows_affected() == 0 {
        HttpResponse::NotFound().finish()
    } else {
        HttpResponse::Ok().finish()
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let database_url = std::env::var("DATABASE_URL").unwrap();
    let pool = PgPool::connect(&database_url).await.unwrap();
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(pool.clone()))
            .service(create_user)
            .service(get_users)
            .service(get_user)
            .service(update_user)
            .service(delete_user)
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
