using System;
using System.Collections.Generic;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.Hosting;
using Npgsql;

namespace ComplangApi
{
    record User(int Id, string Name, string Email);
    record UserInput(string Name, string Email);

    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();

            string connectionString = "Host=host.docker.internal;Port=5432;Database=complang;Username=testuser;Password=" 
                                      + Environment.GetEnvironmentVariable("PGPASSWORD");

            app.MapPost("/users", async (UserInput user) =>
            {
                await using var connection = new NpgsqlConnection(connectionString);
                await connection.OpenAsync();
                var sql = "INSERT INTO users (name, email) VALUES (@name, @email) RETURNING id";
                await using var cmd = new NpgsqlCommand(sql, connection);
                cmd.Parameters.AddWithValue("name", user.Name);
                cmd.Parameters.AddWithValue("email", user.Email);
                var idObj = await cmd.ExecuteScalarAsync();
                int id = Convert.ToInt32(idObj);
                var createdUser = new User(id, user.Name, user.Email);
                return Results.Created($"/users/{id}", createdUser);
            });

            app.MapGet("/users", async () =>
            {
                var users = new List<User>();
                await using var connection = new NpgsqlConnection(connectionString);
                await connection.OpenAsync();
                var sql = "SELECT id, name, email FROM users";
                await using var cmd = new NpgsqlCommand(sql, connection);
                await using var reader = await cmd.ExecuteReaderAsync();
                while (await reader.ReadAsync())
                {
                    var user = new User(reader.GetInt32(0), reader.GetString(1), reader.GetString(2));
                    users.Add(user);
                }
                return Results.Ok(users);
            });

            app.MapGet("/users/{id:int}", async (int id) =>
            {
                await using var connection = new NpgsqlConnection(connectionString);
                await connection.OpenAsync();
                var sql = "SELECT id, name, email FROM users WHERE id=@id";
                await using var cmd = new NpgsqlCommand(sql, connection);
                cmd.Parameters.AddWithValue("id", id);
                await using var reader = await cmd.ExecuteReaderAsync();
                if (await reader.ReadAsync())
                {
                    var user = new User(reader.GetInt32(0), reader.GetString(1), reader.GetString(2));
                    return Results.Ok(user);
                }
                return Results.NotFound();
            });

            app.MapPut("/users/{id:int}", async (int id, UserInput user) =>
            {
                await using var connection = new NpgsqlConnection(connectionString);
                await connection.OpenAsync();
                var sql = "UPDATE users SET name=@name, email=@email WHERE id=@id";
                await using var cmd = new NpgsqlCommand(sql, connection);
                cmd.Parameters.AddWithValue("name", user.Name);
                cmd.Parameters.AddWithValue("email", user.Email);
                cmd.Parameters.AddWithValue("id", id);
                var affectedRows = await cmd.ExecuteNonQueryAsync();
                if (affectedRows == 0)
                {
                    return Results.NotFound();
                }
                return Results.Ok();
            });

            app.MapDelete("/users/{id:int}", async (int id) =>
            {
                await using var connection = new NpgsqlConnection(connectionString);
                await connection.OpenAsync();
                var sql = "DELETE FROM users WHERE id=@id";
                await using var cmd = new NpgsqlCommand(sql, connection);
                cmd.Parameters.AddWithValue("id", id);
                var affectedRows = await cmd.ExecuteNonQueryAsync();
                if (affectedRows == 0)
                {
                    return Results.NotFound();
                }
                return Results.Ok();
            });

            app.Run("http://0.0.0.0:8080");
        }
    }
}
