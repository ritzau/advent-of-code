const std = @import("std");

pub fn solvePart1(allocator: std.mem.Allocator, input: []const u8) !i64 {
    _ = allocator;
    _ = input;
    // TODO: Implement part 1 solution
    return 0;
}

pub fn solvePart2(allocator: std.mem.Allocator, input: []const u8) !i64 {
    _ = allocator;
    _ = input;
    // TODO: Implement part 2 solution
    return 0;
}

// Tests
test "part1_sample_1" {
    // TODO: Add test case from problem description
    const result = try solvePart1(std.testing.allocator, "sample input");
    try std.testing.expectEqual(@as(i64, 0), result);
}

test "part2_sample_1" {
    // TODO: Add test case from problem description
    const result = try solvePart2(std.testing.allocator, "sample input");
    try std.testing.expectEqual(@as(i64, 0), result);
}
