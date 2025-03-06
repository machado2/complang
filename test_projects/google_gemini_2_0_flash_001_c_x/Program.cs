using Microsoft.AspNetCore.Mvc;
using Npgsql;

var builder = WebApplication.CreateBuilder(args);
builder.WebHost.UseUrls("http://0.0.0.0:8080");

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

//app.UseHttpsRedirection();

string? pgPassword = Environment.GetEnvironmentVariable("PGPASSWORD");
string connectionString = $"Host=host.docker.internal;Database=complang;Username=testuser;Password={pgPassword}";

app.MapPost("/users", async ([FromBody] User user) =>
{
    using var conn = new NpgsqlConnection(connectionString);
    await conn.OpenAsync();

    string sql = "INSERT INTO users (name, email) VALUES (@name, @email) RETURNING id";
    using var cmd = new NpgsqlCommand(sql, conn);
    cmd.Parameters.AddWithValue("name", user.Name);
    cmd.Parameters.AddWithValue("email", user.Email);

    int id = (int)await cmd.ExecuteScalarAsync();
    var createdUser = new User { Id = id, Name = user.Name, Email = user.Email };

    return Results.Created($"/users/{id}", createdUser);
})
.WithName("CreateUser");

app.MapGet("/users", async () =>
{
    using var conn = new NpgsqlConnection(connectionString);
    await conn.OpenAsync();

    string sql = "SELECT id, name, email FROM users";
    using var cmd = new NpgsqlCommand(sql, conn);
    using var reader = await cmd.ExecuteReaderAsync();

    var users = new List<User>();
    while (await reader.ReadAsync())
    {
        var user = new User
        {
            Id = reader.GetInt32(0),
            Name = reader.GetString(1),
            Email = reader.GetString(2)
        };
        users.Add(user);
    }

    return Results.Ok(users);
})
.WithName("GetAllUsers");

app.MapGet("/users/{id}", async (int id) =>
{
    using var conn = new NpgsqlConnection(connectionString);
    await conn.OpenAsync();

    string sql = "SELECT id, name, email FROM users WHERE id = @id";
    using var cmd = new NpgsqlCommand(sql, conn);
    cmd.Parameters.AddWithValue("id", id);

    using var reader = await cmd.ExecuteReaderAsync();

    if (await reader.ReadAsync())
    {
        var user = new User
        {
            Id = reader.GetInt32(0),
            Name = reader.GetString(1),
            Email = reader.GetString(2)
        };
        return Results.Ok(user);
    }
    else
    {
        return Results.NotFound();
    }
})
.WithName("GetUserById");

app.MapPut("/users/{id}", async (int id, [FromBody] User user) =>
{
    using var conn = new NpgsqlConnection(connectionString);
    await conn.OpenAsync();

    string sql = "UPDATE users SET name = @name, email = @email WHERE id = @id";
    using var cmd = new NpgsqlCommand(sql, conn);
    cmd.Parameters.AddWithValue("id", id);
    cmd.Parameters.AddWithValue("name", user.Name);
    cmd.Parameters.AddWithValue("email", user.Email);

    int affected = await cmd.ExecuteNonQueryAsync();

    if (affected == 1)
    {
        return Results.NoContent();
    }
    else
    {
        return Results.NotFound();
    }
})
.WithName("UpdateUser");

app.MapDelete("/users/{id}", async (int id) =>
{
    using var conn = new NpgsqlConnection(connectionString);
    await conn.OpenAsync();

    string sql = "DELETE FROM users WHERE id = @id";
    using var cmd = new NpgsqlCommand(sql, conn);
    cmd.Parameters.AddWithValue("id", id);

    int affected = await cmd.ExecuteNonQueryAsync();

    if (affected == 1)
    {
        return Results.NoContent();
    }
    else
    {
        return Results.NotFound();
    }
})
.WithName("DeleteUser");

app.Run();

public class User
{
    public int Id { get; set; }
    public string? Name { get; set; }
    public string? Email { get; set; }
}