const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("toml", "src/toml.zig");
    lib.setBuildMode(mode);
    lib.install();

    var main_tests = b.addTest("src/toml.zig");
    main_tests.setBuildMode(mode);

    main_tests.linkSystemLibrary("c");

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
