const Builder = @import("std").build.Builder;
const std = @import("std");

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const lib = b.addStaticLibrary(.{
        .name = "toml",
        .root_source_file = .{ .path = "src/toml.zig" },
        .optimize = optimize,
        .target = target,
    });
    b.installArtifact(lib);
    const module = b.addModule("toml", std.Build.CreateModuleOptions{
        .source_file = .{ .path = "src/toml.zig" },
    });
    lib.addModule("toml", module);

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/toml.zig" },
        .optimize = optimize,
        .target = target,
    });

    main_tests.linkSystemLibrary("c");

    const run_test_cmd = b.addRunArtifact(main_tests);
    run_test_cmd.has_side_effects = true;
    run_test_cmd.step.dependOn(b.getInstallStep());

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_test_cmd.step);
}
