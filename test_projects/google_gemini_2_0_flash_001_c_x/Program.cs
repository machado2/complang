
using Npgsql;
using System.Text.Json;
using Microsoft.AspNetCore.Mvc;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

string? pgPassword = Environment.GetEnvironmentVariable("PGPASSWORD");
string connectionString = $"Host=host.docker.internal;Port=5432;Database=test_google_gemini_2_0_flash_001_c_x;Username=postgres;Password={pgPassword}";

using (var conn = new NpgsqlConnection(connectionString))
{
    conn.Open();

    using (var cmd = new NpgsqlCommand("SELECT EXISTS (SELECT 1 FROM pg_tables WHERE tablename = 'users')", conn))
    {
        var tableExists = (bool)cmd.ExecuteScalar();

        if (!tableExists)
        {
            using (var createTableCmd = new NpgsqlCommand(@"
                CREATE TABLE users (
                    id SERIAL PRIMARY KEY,
                    name TEXT,
                    email TEXT
                )", conn))
            {
                createTableCmd.ExecuteNonQuery();
            }
        }
    }
}

app.MapPost("/users", async ([FromBody] User user) =>
{
    using (var conn = new NpgsqlConnection(connectionString))
    {
        conn.Open();
        using (var cmd = new NpgsqlCommand("INSERT INTO users (name, email) VALUES (@name, @email) RETURNING id", conn))
        {
            cmd.Parameters.AddWithValue("name", user.name);
            cmd.Parameters.AddWithValue("email", user.email);

            int id = (int)cmd.ExecuteScalar();
            user.id = id;
        }
    }

    return Results.Created($"/users/{user.id}", user);
})
.WithName("CreateUser")
.WithOpenApi();

app.MapGet("/users", async () =>
{
    List<User> users = new List<User>();
    using (var conn = new NpgsqlConnection(connectionString))
    {
        conn.Open();
        using (var cmd = new NpgsqlCommand("SELECT id, name, email FROM users", conn))
        {
            using (var reader = cmd.ExecuteReader())
            {
                while (reader.Read())
                {
                    users.Add(new User
                    {
                        id = reader.GetInt32(0),
                        name = reader.GetString(1),
                        email = reader.GetString(2)
                    });
                }
            }
        }
    }
    return Results.Ok(users);
})
.WithName("GetUsers")
.WithOpenApi();

app.MapGet("/users/{id}", async (int id) =>
{
    using (var conn = new NpgsqlConnection(connectionString))
    {
        conn.Open();
        using (var cmd = new NpgsqlCommand("SELECT id, name, email FROM users WHERE id = @id", conn))
        {
            cmd.Parameters.AddWithValue("id", id);

            using (var reader = cmd.ExecuteReader())
            {
                if (reader.Read())
                {
                    return Results.Ok(new User
                    {
                        id = reader.GetInt32(0),
                        name = reader.GetString(1),
                        email = reader.GetString(2)
                    });
                }
                else
                {
                    return Results.NotFound();
                }
            }
        }
    }
})
.WithName("GetUser")
.WithOpenApi();

app.MapPut("/users/{id}", async (int id, [FromBody] User updatedUser) =>
{
    using (var conn = new NpgsqlConnection(connectionString))
    {
        conn.Open();
        using (var cmd = new NpgsqlCommand("UPDATE users SET name = @name, email = @email WHERE id = @id", conn))
        {
            cmd.Parameters.AddWithValue("id", id);
            cmd.Parameters.AddWithValue("name", updatedUser.name);
            cmd.Parameters.AddWithValue("email", updatedUser.email);

            int rowsAffected = cmd.ExecuteNonQuery();
            return rowsAffected == 0 ? Results.NotFound() : Results.NoContent();
        }
    }
})
.WithName("UpdateUser")
.WithOpenApi();

app.MapDelete("/users/{id}", async (int id) =>
{
    using (var conn = new NpgsqlConnection(connectionString))
    {
        conn.Open();
        using (var cmd = new NpgsqlCommand("DELETE FROM users WHERE id = @id", conn))
        {
            cmd.Parameters.AddWithValue("id", id);

            int rowsAffected = cmd.ExecuteNonQuery();
            return rowsAffected == 0 ? Results.NotFound() : Results.NoContent();
        }
    }
})
.WithName("DeleteUser")
.WithOpenApi();

app.Run("http://*:8080");

public class User
{
    public int id { get; set; }
    public string? name { get; set; }
    public string? email { get; set; }
}
