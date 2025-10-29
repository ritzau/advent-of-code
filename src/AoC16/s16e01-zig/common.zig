const std = @import("std");

pub const Direction = enum {
    North,
    East,
    South,
    West,

    pub fn turnRight(self: Direction) Direction {
        return switch (self) {
            .North => .East,
            .East => .South,
            .South => .West,
            .West => .North,
        };
    }

    pub fn turnLeft(self: Direction) Direction {
        return switch (self) {
            .North => .West,
            .West => .South,
            .South => .East,
            .East => .North,
        };
    }

    pub fn delta(self: Direction) struct { x: i32, y: i32 } {
        return switch (self) {
            .North => .{ .x = 0, .y = 1 },
            .East => .{ .x = 1, .y = 0 },
            .South => .{ .x = 0, .y = -1 },
            .West => .{ .x = -1, .y = 0 },
        };
    }
};

pub const Instruction = struct {
    turn: u8,
    blocks: i32,
};

pub fn parseInput(allocator: std.mem.Allocator, input: []const u8) !std.ArrayList(Instruction) {
    var instructions = std.ArrayList(Instruction).init(allocator);
    errdefer instructions.deinit();

    var iter = std.mem.splitSequence(u8, std.mem.trim(u8, input, " \n\r\t"), ", ");
    while (iter.next()) |part| {
        if (part.len == 0) continue;

        const turn = part[0];
        const blocks = try std.fmt.parseInt(i32, part[1..], 10);
        try instructions.append(Instruction{ .turn = turn, .blocks = blocks });
    }

    return instructions;
}

pub fn abs(x: i32) i32 {
    return if (x < 0) -x else x;
}

pub fn solvePart1(allocator: std.mem.Allocator, input: []const u8) !i32 {
    const instructions = try parseInput(allocator, input);
    defer instructions.deinit();

    var x: i32 = 0;
    var y: i32 = 0;
    var direction = Direction.North;

    for (instructions.items) |instruction| {
        direction = switch (instruction.turn) {
            'R' => direction.turnRight(),
            'L' => direction.turnLeft(),
            else => return error.InvalidTurn,
        };

        const d = direction.delta();
        x += d.x * instruction.blocks;
        y += d.y * instruction.blocks;
    }

    return abs(x) + abs(y);
}

pub fn solvePart2(allocator: std.mem.Allocator, input: []const u8) !i32 {
    const instructions = try parseInput(allocator, input);
    defer instructions.deinit();

    var x: i32 = 0;
    var y: i32 = 0;
    var direction = Direction.North;

    var visited = std.AutoHashMap([2]i32, void).init(allocator);
    defer visited.deinit();

    try visited.put([2]i32{ 0, 0 }, {});

    for (instructions.items) |instruction| {
        direction = switch (instruction.turn) {
            'R' => direction.turnRight(),
            'L' => direction.turnLeft(),
            else => return error.InvalidTurn,
        };

        const d = direction.delta();

        var i: i32 = 0;
        while (i < instruction.blocks) : (i += 1) {
            x += d.x;
            y += d.y;

            const pos = [2]i32{ x, y };
            const result = try visited.getOrPut(pos);
            if (result.found_existing) {
                return abs(x) + abs(y);
            }
        }
    }

    return 0;
}
