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
