
using Microsoft.EntityFrameworkCore;
using UserAPI.Models;

namespace UserAPI.Data
{
    public class AppDbContext : DbContext
    {
        public AppDbContext(DbContextOptions<AppDbContext> options) : base(options)
        {
        }

        public DbSet<User> Users { get; set; } = null!;
        
        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<User>().ToTable("users");
            
            // Map proper column names (lowercase as per PostgreSQL convention)
            modelBuilder.Entity<User>()
                .Property(u => u.Id)
                .HasColumnName("id");
                
            modelBuilder.Entity<User>()
                .Property(u => u.Name)
                .HasColumnName("name");
                
            modelBuilder.Entity<User>()
                .Property(u => u.Email)
                .HasColumnName("email");
        }
    }
}
