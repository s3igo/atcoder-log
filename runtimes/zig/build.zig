const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "judge",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    const ac_library = b.dependency("ac-library", .{
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("ac-library", ac_library.module("ac-library"));

    const string = b.dependency("string", .{
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("string", string.module("string"));

    const ziter = b.dependency("ziter", .{
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("ziter", ziter.module("ziter"));
}
