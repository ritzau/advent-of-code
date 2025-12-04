const std = @import("std");

const sample_input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

fn count_digits(n: usize) usize {
    if (n == 0) {
        return 1;
    }

    var f: usize = 0;
    var num = @abs(n);
    while (num > 0) {
        f += 1;
        num /= 10;
    }

    return f;
}

fn split_intervall(intervall: []const u8) ![2]usize {
    var it = std.mem.splitScalar(u8, intervall, '-');
    const first = try std.fmt.parseInt(usize, it.next().?, 10);
    const last = try std.fmt.parseInt(usize, it.next().?, 10);
    return .{ first, last };
}

fn sum_sns(sns: std.AutoHashMap(usize, usize)) usize {
    var sum: usize = 0;
    var sni = sns.keyIterator();
    while (sni.next()) |sn| {
        sum += sn.*;
    }
    return sum;
}

pub fn solvePart1(allocator: std.mem.Allocator, input: []const u8) !u64 {
    var sns = std.AutoHashMap(usize, usize).init(allocator);
    defer sns.deinit();

    var it = std.mem.splitScalar(u8, input, ',');
    while (it.next()) |intervall| {
        const first, const last = try split_intervall(intervall);
        const first_count = count_digits(first);
        const last_count = count_digits(last);

        for (first_count / 2..last_count / 2 + 1) |c| {
            const c_factor = std.math.pow(usize, 10, c);
            for (c_factor / 10..c_factor) |x| {
                const sn = x * c_factor + x;
                if (first <= sn and sn <= last) {
                    try sns.put(sn, sn);
                }
            }
        }
    }

    return sum_sns(sns);
}

pub fn solvePart2(allocator: std.mem.Allocator, input: []const u8) !u64 {
    var sns = std.AutoHashMap(usize, usize).init(allocator);
    defer sns.deinit();

    var it = std.mem.splitScalar(u8, input, ',');
    while (it.next()) |intervall| {
        const first, const last = try split_intervall(intervall);
        const last_count = count_digits(last);

        for (1..last_count / 2 + 1) |c| {
            const c_factor = std.math.pow(usize, 10, c);
            for (c_factor / 10..c_factor) |x| {
                var sn = x * c_factor + x;
                while (sn <= last) {
                    if (first <= sn and sn <= last) {
                        try sns.put(sn, sn);
                    }
                    sn *= c_factor;
                    sn += x;
                }
            }
        }
    }

    return sum_sns(sns);
}

// Tests
test "part1_sample_1" {
    const result = try solvePart1(std.testing.allocator, sample_input);
    try std.testing.expectEqual(@as(usize, 1227775554), result);
}

test "part2_sample_1" {
    const result = try solvePart2(std.testing.allocator, sample_input);
    try std.testing.expectEqual(@as(usize, 4174379265), result);
}
