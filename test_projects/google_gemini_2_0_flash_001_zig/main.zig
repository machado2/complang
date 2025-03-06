            const email = parsed.root.Obj.get("email") orelse return try sendErrorResponse(res, 400, "Bad Request: Missing 'email' field");

            const name_str = name.Str;
            const email_str = email.Str;

    const query = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email";
    const result = try client.query(query, .{ name_str, email_str });
    defer result.deinit();

    if (result.rows.len != 1) {
        return try sendErrorResponse(res, 500, "Internal Server Error: Failed to insert user");
    }

    const row = result.rows[0];
    const id: i32 = try row.get(0).toInt(i32);
    const inserted_name: []const u8 = row.get(1).?.asString();
    const inserted_email: []const u8 = row.get(2).?.asString();


    res.headers.append(.{ .name = "Content-Type", .value = "application/json" });
    try res.setStatus(201);


    var pw = std.io.getStdOut().writer();
    try pw.print("HTTP/1.1 201 Created\r\nContent-Type: application/json\r\n\r\n", .{});
    var serializer = json.StringSerializer.new(allocator, .{ .whitespace = .Compact });
    defer serializer.deinit();

    try serializer.openObject();
    try serializer.addObjectKey("id");
    try serializer.addValue(id);
    try serializer.addObjectKey("name");
    try serializer.addValue(inserted_name);
    try serializer.addObjectKey("email");
    try serializer.addValue(inserted_email);
    try serializer.closeObject();

    const serialized = serializer.string();
    try pw.print("{s}", .{serialized});

}

fn handleGetUser(req: *http.Request, res: *http.Response, id: i32) !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
        var client = pg.Client.init(allocator, db_url) catch |err| {
        std.debug.print("Error connecting to DB: {}\n", .{@errorName(err)});
        return sendErrorResponse(res, 500, "Internal Server Error");
    };
    defer client.deinit();
    try client.connect();

    const query = "SELECT id, name, email FROM users WHERE id = $1";
    const result = try client.query(query, .{id});
    defer result.deinit();

    if (result.rows.len == 0) {
                return try res.setStatus(404);

    }

        const row = result.rows[0];
        const user_id: i32 = try row.get(0).toInt(i32);
        const name: []const u8 = row.get(1).?.asString();
        const email: []const u8 = row.get(2).?.asString();

           var pw = std.io.getStdOut().writer();
            try pw.print("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n", .{});

            var serializer = json.StringSerializer.new(allocator, .{ .whitespace = .Compact });
            defer serializer.deinit();

            try serializer.openObject();
            try serializer.addObjectKey("id");
            try serializer.addValue(user_id);
            try serializer.addObjectKey("name");
            try serializer.addValue(name);
            try serializer.addObjectKey("email");
            try serializer.addValue(email);
            try serializer.closeObject();

            const serialized = serializer.string();
            try pw.print("{s}", .{serialized});

}


fn handlePutUser(req: *http.Request, res: *http.Response, id: i32) !void {
           var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
        var client = pg.Client.init(allocator, db_url) catch |err| {
        std.debug.print("Error connecting to DB: {}\n", .{@errorName(err)});
        return sendErrorResponse(res, 500, "Internal Server Error");
    };
    defer client.deinit();
    try client.connect();

    const body = req.body orelse return try sendErrorResponse(res, 400, "Bad Request: Missing body");
        var parsed = std.json.parseFromSlice(std.AutoFreeList(u8).*, allocator, body, .{}) catch |err| {
        std.debug.print("Failed to parse JSON: {}\n", .{@errorName(err)});
        return try sendErrorResponse(res, 400, "Bad Request: Invalid JSON");
    };
    defer parsed.deinit();

            const name = parsed.root.Obj.get("name") orelse return try sendErrorResponse(res, 400, "Bad Request: Missing 'name' field");
            const email = parsed.root.Obj.get("email") orelse return try sendErrorResponse(res, 400, "Bad Request: Missing 'email' field");

    const name_str = name.Str;
    const email_str = email.Str;
    const query = "UPDATE users SET name = $1, email = $2 WHERE id = $3";

    const result = try client.query(query, .{ name_str, email_str, id });
    defer result.deinit();

    if (result.rowsAffected == 0) {
        return try res.setStatus(404);
    }

    try res.setStatus(204);
}


fn handleDeleteUser(req: *http.Request, res: *http.Response, id: i32) !void {
      var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
        var client = pg.Client.init(allocator, db_url) catch |err| {
        std.debug.print("Error connecting to DB: {}\n", .{@errorName(err)});
        return sendErrorResponse(res, 500, "Internal Server Error");
    };
    defer client.deinit();
    try client.connect();

    const query = "DELETE FROM users WHERE id = $1";
    const result = try client.query(query, .{id});
    defer result.deinit();

        if (result.rowsAffected == 0) {
        return try res.setStatus(404);
    }

    try res.setStatus(204);
}


fn sendErrorResponse(res: *http.Response, status: u16, message: []const u8) !void {
    res.headers.append(.{ .name = "Content-Type", .value = "text/plain" });
    try res.setStatus(status);
    try res.print(message);
}



