
using System.ComponentModel.DataAnnotations;

namespace UserAPI.Models
{
    public class User
    {
        public int Id { get; set; }
        
        [Required]
        public string Name { get; set; } = string.Empty;
        
        [Required]
        public string Email { get; set; } = string.Empty;
    }
    
    // DTO for create/update operations
    public class UserDto
    {
        [Required]
        public string Name { get; set; } = string.Empty;
        
        [Required]
        public string Email { get; set; } = string.Empty;
    }
}
