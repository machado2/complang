// src/main.zig
const std = @import("std");
const http = std.http;
const log = std.log;
const json = std.json;
const mem = std.mem;
const os = std.os;

const PG = struct {
    const DB_HOST = "host.docker.internal";
    const DB_PORT = 5432;
    const DB_NAME = "complang";
    const DB_USER = "testuser";

    fn getPassword() ![]const u8 {
        const password = std.os.getenv("PGPASSWORD") orelse return error.MissingPGPassword;
        return password;
    }
};

const User = struct {
    id: i32,
    name: []const u8,
    email: []const u8,
};

const Error = enum {
    NotFound,
    BadRequest,
    InternalServerError,
    MissingPGPassword,
    PgError,
    OutOfMemory,
    EncodingError,
};

fn respondError(connection: *http.Server.Connection, error: Error) !void {
    const message = switch (error) {
        .NotFound => "Not Found",
        .BadRequest => "Bad Request",
        .InternalServerError => "Internal Server Error",
        .MissingPGPassword => "Missing PGPASSWORD environment variable",
        .PgError => "PostgreSQL Error",
        .OutOfMemory => "Out of Memory",
        .EncodingError => "Encoding Error",
    };

    const statusCode = switch (error) {
        .NotFound => 404,
        .BadRequest => 400,
        else => 500,
    };

    var response = http.Server.Response.init(mem.Allocator{ .allocFn = connection.allocator.alloc, .freeFn = connection.allocator.free });
    defer response.deinit();
    response.statusCode = statusCode;
    try response.headers.append("Content-Type", "text/plain");
    response.body = message;
    try connection.send(response);
}


fn createUser(connection: *http.Server.Connection, reque" + 5131 + " .Compact });

    var response = http.Server.Response.init(mem.Allocator{ .allocFn = connection.allocator.alloc, .freeFn = connection.allocator.free });
    defer response.deinit();
    response.statusCode = 204;
    try connection.send(response);
}


fn deleteUser(connection: *http.Server.Connection, id: i32) !void {
    var arena = std.heap.ArenaAllocator.init(connection.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();


    const password = try PG.getPassword();
    var pg_conn = std.postgres.Client.init(allocator, .{ .host = PG.DB_HOST, .port = PG.DB_PORT, .user = PG.DB_USER, .password = password, .database = PG.DB_NAME });
    defer pg_conn.deinit();
    errdefer {
        log.err("Error connecting to PostgreSQL: {any}", .{@error()});
        try respondError(connection, .InternalServerError);
    }
    try pg_conn.connect();

    const query = try std.fmt.allocPrint(allocator, "DELETE FROM users WHERE id = $1", .{id});
    defer allocator.free(query);

    const rows = try pg_conn.query(query, .{}) catch |err| {
        log.err("PGError {any}", .{err});
        return respondError(connection, .PgError);
    };
    
    if (rows.items.len == 0) {
       return respondError(connection, .NotFound);
    }

    var response = http.Server.Response.init(mem.Allocator{ .allocFn = connection.allocator.alloc, .freeFn = connection.allocator.free });
    defer response.deinit();
    response.statusCode = 204;
    try connection.send(response);
}


pub fn main() !void {
    std.log.info("Starting server...", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var server = http.Server.init(allocator, .{}) orelse {
        std.log.err(
            \Failed to initialize server.
            \This may be due to another process already listening on the port.
        , .{});
        return;
    };
    defer server.deinit();

    server.listen(8080, handleRequest) orelse {
        std.log.err("Failed to listen on port 8080", .{});
        return;
    };
}

fn handleRequest(connection: *http.Server.Connection, request: *http.Server.Request) !void {
    std.log.info("{s} {s}", .{ .name = request.method.name, .url = request.url });

    if (mem.eql(u8, request.url, "/users")) {
        if (request.method == .POST) {
             try createUser(connection, request);
        } else if (request.method == .GET) {
            try getAllUsers(connection);
        } else {
            try respondError(connection, .BadRequest);
        }
    } else if (mem.startsWith(u8, request.url, "/users/")) {
        const id_str = request.url[6..];
        const id = try std.fmt.parseInt(i32, id_str, 10) catch {
           return respondError(connection, .BadRequest);
        };

        if (request.method == .GET) {
            try getUser(connection, id);
        } else if (request.method == .PUT) {
            try updateUser(connection, request, id);
        } else if (request.method == .DELETE) {
            try deleteUser(connection, id);
        } else {
            try respondError(connection, .BadRequest);
        }
    } else {
        try respondError(connection, .NotFound);
    }
}
