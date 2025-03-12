
local pgmoon = require "pgmoon"
local cjson = require "cjson"

local _M = {}

-- Database connection configuration
local db_config = {
    host = "host.docker.internal",
    port = 5432,
    database = "complang",
    user = "testuser",
    password = os.getenv("PGPASSWORD"),
    ssl = false
}

-- Connect to the PostgreSQL database
local function get_connection()
    local pg = pgmoon.new(db_config)
    
    local ok, err = pg:connect()
    if not ok then
        ngx.log(ngx.ERR, "Failed to connect to database: ", err)
        return nil, err
    end
    
    return pg
end

-- Function to create a new user
function _M.create_user(name, email)
    local pg, err = get_connection()
    if not pg then
        return nil, err
    end
    
    local query = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
    local res, err = pg:query({query, name, email})
    
    pg:keepalive()
    
    if not res then
        return nil, err
    end
    
    return res[1]
end

-- Function to get all users
function _M.get_all_users()
    local pg, err = get_connection()
    if not pg then
        return nil, err
    end
    
    local res, err = pg:query("SELECT id, name, email FROM users")
    
    pg:keepalive()
    
    if not res then
        return nil, err
    end
    
    return res
end

-- Function to get a user by ID
function _M.get_user(id)
    local pg, err = get_connection()
    if not pg then
        return nil, err
    end
    
    local query = "SELECT id, name, email FROM users WHERE id = $1"
    local res, err = pg:query({query, id})
    
    pg:keepalive()
    
    if not res or not res[1] then
        return nil, "User not found"
    end
    
    return res[1]
end

-- Function to update a user
function _M.update_user(id, name, email)
    local pg, err = get_connection()
    if not pg then
        return nil, err
    end
    
    local query = "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email"
    local res, err = pg:query({query, name, email, id})
    
    pg:keepalive()
    
    if not res or not res[1] then
        return nil, "User not found"
    end
    
    return res[1]
end

-- Function to delete a user
function _M.delete_user(id)
    local pg, err = get_connection()
    if not pg then
        return nil, err
    end
    
    local check_query = "SELECT id FROM users WHERE id = $1"
    local check_res, check_err = pg:query({check_query, id})
    
    if not check_res or not check_res[1] then
        pg:keepalive()
        return nil, "User not found"
    end
    
    local query = "DELETE FROM users WHERE id = $1"
    local res, err = pg:query({query, id})
    
    pg:keepalive()
    
    if not res then
        return nil, err
    end
    
    return true
end

return _M
