const std = @import("std");

pub fn build(b: *std.Build) void {
    const linkage = b.option(std.builtin.LinkMode, "linkage", "Link mode for zig-toml library") orelse .static; // or other default
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.addModule("toml", .{
        .root_source_file = b.path("src/toml.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .linkage = linkage,
        .name = "toml",
        .root_module = module,
    });
    b.installArtifact(lib);

    const main_tests = b.addTest(.{
        .root_module = module,
    });

    main_tests.root_module.link_libc = true;

    const run_test_cmd = b.addRunArtifact(main_tests);
    run_test_cmd.has_side_effects = true;
    run_test_cmd.step.dependOn(b.getInstallStep());

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_test_cmd.step);
}
