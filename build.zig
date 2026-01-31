const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "zppc",
        .root_module = b.addModule("zppc", .{
            .root_source_file = b.path("src/main.zig"),
            .target = b.standardTargetOptions(.{}),
            .optimize = b.standardOptimizeOption(.{}),
        })
    });

    b.installArtifact(exe);

    const test_step = b.step("test", "");
    const tests = b.addTest(.{.root_module = exe.root_module});
    test_step.dependOn(&b.addRunArtifact(tests).step);
}
