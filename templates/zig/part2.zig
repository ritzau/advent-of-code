const std = @import("std");
const common = @import("common.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn();
    const input = try stdin.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    const result = try common.solvePart2(allocator, std.mem.trim(u8, input, "\n"));
    const stdout = std.io.getStdOut();
    try stdout.writer().print("{d}\n", .{result});
}
