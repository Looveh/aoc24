const std = @import("std");

pub fn build(b: *std.Build) void {
    const root_source_file = b.path("./src/main.zig");
    const target = b.host;
    const optimize = .ReleaseFast;

    const exe = b.addExecutable(.{
        .name = "aoc",
        .root_source_file = root_source_file,
        .target = target,
        .optimize = optimize,
    });

    const install_step = b.addInstallArtifact(exe, .{
        .dest_dir = .{ .override = .{ .custom = "../" } },
    });

    b.getInstallStep().dependOn(&install_step.step);

    // ----------------------
    // Tests

    const unit_tests = b.addTest(.{
        .root_source_file = root_source_file,
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");

    test_step.dependOn(&run_unit_tests.step);
}
