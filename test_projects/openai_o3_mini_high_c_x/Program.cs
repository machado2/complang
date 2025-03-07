using System;
using System.Data;
using Dapper;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.Hosting;
using Npgsql;

var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

var pgPassword = Environment.GetEnvironmentVariable("PGPASSWORD") ?? "";
var connectionString = $"Host=host.docker.internal;Port=5432;Database=complang;Username=testuser;Password={pgPassword};";

IDbConnection CreateConnection() => new NpgsqlConnection(connectionString);

app.MapPost("/users", async (UserRequest userRequest) =>
{
    using var connection = CreateConnection();
    var id = await connection.ExecuteScalarAsync<int>(
        "INSERT INTO users (name, email) VALUES (@Name, @Email) RETURNING id",
        new { userRequest.Name, userRequest.Email });
    var user = new User(id, userRequest.Name, userRequest.Email);
    return Results.Created($"/users/{id}", user);
});

app.MapGet("/users", async () =>
{
    using var connection = CreateConnection();
    var users = await connection.QueryAsync<User>(
        "SELECT id, name, email FROM users");
    return Results.Ok(users);
});

app.MapGet("/users/{id:int}", async (int id) =>
{
    using var connection = CreateConnection();
    var user = await connection.QuerySingleOrDefaultAsync<User>(
        "SELECT id, name, email FROM users WHERE id = @Id",
        new { Id = id });
    return user is not null ? Results.Ok(user) : Results.NotFound();
});

app.MapPut("/users/{id:int}", async (int id, UserRequest userRequest) =>
{
    using var connection = CreateConnection();
    var affected = await connection.ExecuteAsync(
        "UPDATE users SET name = @Name, email = @Email WHERE id = @Id",
        new { Name = userRequest.Name, Email = userRequest.Email, Id = id });
    if (affected == 0) return Results.NotFound();
    return Results.Ok();
});

app.MapDelete("/users/{id:int}", async (int id) =>
{
    using var connection = CreateConnection();
    var affected = await connection.ExecuteAsync(
        "DELETE FROM users WHERE id = @Id",
        new { Id = id });
    if (affected == 0) return Results.NotFound();
    return Results.Ok();
});

app.Run();

public record User(int Id, string Name, string Email);
public record UserRequest(string Name, string Email);
