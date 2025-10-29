const std = @import("std");
const common = @import("common.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const input = try stdin.readAllAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    const result = try common.solvePart1(allocator, std.mem.trim(u8, input, "\n"));
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{result});
}

test "part1_sample_1" {
    // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
    const result = try common.solvePart1(std.testing.allocator, "R2, L3");
    try std.testing.expectEqual(@as(i32, 5), result);
}

test "part1_sample_2" {
    // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
    const result = try common.solvePart1(std.testing.allocator, "R2, R2, R2");
    try std.testing.expectEqual(@as(i32, 2), result);
}

test "part1_sample_3" {
    // R5, L5, R5, R3 leaves you 12 blocks away
    const result = try common.solvePart1(std.testing.allocator, "R5, L5, R5, R3");
    try std.testing.expectEqual(@as(i32, 12), result);
}
