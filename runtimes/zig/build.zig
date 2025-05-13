const std = @import("std");

pub fn build(b: *std.Build) void {
    const name = "judge";

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    { // Deps
        const ac_library = b.dependency("ac-library", .{
            .target = target,
            .optimize = optimize,
        });
        exe_mod.addImport("ac-library", ac_library.module("ac-library"));

        const proconio = b.dependency("proconio", .{
            .target = target,
            .optimize = optimize,
        });
        exe_mod.addImport("proconio", proconio.module("proconio"));

        const string = b.dependency("string", .{
            .target = target,
            .optimize = optimize,
        });
        exe_mod.addImport("string", string.module("string"));

        const ziter = b.dependency("ziter", .{
            .target = target,
            .optimize = optimize,
        });
        exe_mod.addImport("ziter", ziter.module("ziter"));
    }

    { // Check step
        const exe_check = b.addExecutable(.{
            .name = name,
            .root_module = exe_mod,
        });

        const check = b.step("check", "Check if the app compiles");
        check.dependOn(&exe_check.step);
    }

    { // Test step
        const exe_unit_tests = b.addTest(.{
            .root_module = exe_mod,
        });

        const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

        const test_step = b.step("test", "Run unit tests");
        test_step.dependOn(&run_exe_unit_tests.step);
    }

    const exe = b.addExecutable(.{
        .name = name,
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    { // Run step
        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());

        const run_step = b.step("run", "Run the app");
        run_step.dependOn(&run_cmd.step);
    }
}
