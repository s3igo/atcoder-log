const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const snippets_mod = b.createModule(.{
        .root_source_file = b.path("snippets/zig/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    { // Test step
        const snippets_unit_tests = b.addTest(.{
            .root_module = snippets_mod,
        });

        const run_lib_unit_tests = b.addRunArtifact(snippets_unit_tests);

        const test_step = b.step("test", "Run unit tests");
        test_step.dependOn(&run_lib_unit_tests.step);
    }

    { // Format step
        const snippets_fmt = b.addFmt(.{
            .paths = &.{"snippets/zig"},
        });

        const fmt_step = b.step("fmt", "Format Zig code");
        fmt_step.dependOn(&snippets_fmt.step);
    }

    { // Format-check step
        const snippets_fmt_check = b.addFmt(.{
            .paths = &.{"snippets/zig"},
            .check = true,
        });

        const fmt_check_step = b.step("fmt-check", "Check Zig code formatting");
        fmt_check_step.dependOn(&snippets_fmt_check.step);
    }
}
