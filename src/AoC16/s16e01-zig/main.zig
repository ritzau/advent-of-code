const std = @import("std");
const common = @import("common.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const input = try stdin.readAllAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    const stdout = std.io.getStdOut().writer();

    try stdout.print("AoC 2016 Day 1: No Time for a Taxicab\n", .{});
    try stdout.print("======================================\n", .{});

    // Part 1
    const start1 = std.time.nanoTimestamp();
    const result1 = try common.solvePart1(allocator, input);
    const duration1 = std.time.nanoTimestamp() - start1;
    const expected_part1: i32 = 300;
    const pass1 = result1 == expected_part1;

    try stdout.print("Part 1: {s} {d} (expected: {d}) [{d:.2}ms]\n", .{
        if (pass1) "✅" else "❌",
        result1,
        expected_part1,
        @as(f64, @floatFromInt(duration1)) / 1_000_000.0,
    });

    // Part 2
    const start2 = std.time.nanoTimestamp();
    const result2 = try common.solvePart2(allocator, input);
    const duration2 = std.time.nanoTimestamp() - start2;
    const expected_part2: i32 = 159;
    const pass2 = result2 == expected_part2;

    try stdout.print("Part 2: {s} {d} (expected: {d}) [{d:.2}ms]\n", .{
        if (pass2) "✅" else "❌",
        result2,
        expected_part2,
        @as(f64, @floatFromInt(duration2)) / 1_000_000.0,
    });

    try stdout.print("Total: {d:.2}ms\n", .{
        @as(f64, @floatFromInt(duration1 + duration2)) / 1_000_000.0,
    });

    if (pass1 and pass2) {
        try stdout.print("\n🌟🌟 All tests passed!\n", .{});
    } else {
        try stdout.print("\n❌ Some tests failed\n", .{});
        std.process.exit(1);
    }
}
