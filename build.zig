const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.createModule(.{
        .root_source_file = b.path("lib/tomboy.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe_ir = b.addExecutable(.{
        .name = "tomboy-ir",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/ir/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{
                    .name = "tomboy",
                    .module = module,
                },
            },
        }),
    });

    b.installArtifact(exe_ir);

    const module_tests = b.addTest(.{
        .root_module = module,
    });

    const run_module_tests = b.addRunArtifact(module_tests);

    const doc_step = b.step("docs", "Generate documentation");

    doc_step.dependOn(&b.addInstallDirectory(.{
        .source_dir = module_tests.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs/tomboy",
    }).step);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_module_tests.step);
}
