
using Microsoft.EntityFrameworkCore;
using ComplangApi;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

// Configure PostgreSQL connection
var pgPassword = Environment.GetEnvironmentVariable("PGPASSWORD");
var connectionString = $"Host=host.docker.internal;Port=5432;Database=complang;Username=testuser;Password={pgPassword}";
builder.Services.AddDbContext<UserContext>(options => options.UseNpgsql(connectionString));

// Configure Kestrel to listen on port 8080
builder.WebHost.UseUrls("http://*:8080");

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

app.UseAuthorization();

app.MapControllers();

app.Run();
