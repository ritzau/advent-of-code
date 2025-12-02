const std = @import("std");
const common = @import("common.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read from stdin using Zig 0.15+ API with streamRemaining
    var stdin_buffer: [4096]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const reader = &stdin_reader.interface;

    var input_writer = std.Io.Writer.Allocating.init(allocator);
    defer input_writer.deinit();
    _ = try reader.streamRemaining(&input_writer.writer);
    const input = input_writer.written();

    const result = try common.solvePart2(allocator, std.mem.trim(u8, input, "\n"));

    // Write to stdout using Zig 0.15+ API
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const writer = &stdout_writer.interface;
    try writer.print("{d}\n", .{result});
    try writer.flush();
}
