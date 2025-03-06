// build.zig
const std = @import("std");

pub fn build(b: *std.build.Builder) !void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed.
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "crud-api",
        // By default build a hello world program.
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    // This compiles the zig code defined above into machine code
    const step = b.step("build", "Build the executable");
    step.dependOn(&exe.step);

    // Creates an install step such that running `zig build install`
    // will copy the executable wherever the user specifies. Usually
    // this is /usr/local/bin.
    const install_step = b.addInstallArtifact(exe);

    // This is how we ensure that the install step is available, even
    // when it's not the default.
    b.default_step.dependOn(&install_step.step);

    // Run tests
    const run_cmd = b.addRunArtifact(exe);

    // By adding a step that depends on the test step, we allow the person
    // running `zig build` to choose to skip running tests.
    if (b.args.contains("--skip-tests")) {
        return;
    }

    const test_step = b.step(
        "test",
        "Run tests",
    );
    test_step.dependOn(&run_cmd.step);
}
