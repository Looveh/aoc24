const std = @import("std");
const aoc = @import("aoc.zig");

pub fn run(alc: std.mem.Allocator) !void {
    const input = try aoc.readInput(alc, 24, 1);
    defer alc.free(input);

    var left = std.ArrayList(i64).init(alc);
    defer left.deinit();
    var right = std.ArrayList(i64).init(alc);
    defer right.deinit();

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var iter = std.mem.tokenize(u8, line, " ");
        if (iter.next()) |part| {
            try left.append(try std.fmt.parseInt(i64, part, 10));
        }
        if (iter.next()) |part| {
            try right.append(try std.fmt.parseInt(i64, part, 10));
        }
    }

    std.mem.sort(i64, left.items, {}, comptime std.sort.asc(i64));
    std.mem.sort(i64, right.items, {}, comptime std.sort.asc(i64));

    var pt1: i64 = 0;
    for (0.., left.items) |i, _| {
        pt1 += @intCast(@abs(left.items[i] - right.items[i]));
    }

    std.debug.print("pt1: {d}\n", .{pt1});

    var pt2: i64 = 0;

    for (left.items) |n| {
        for (right.items) |m| {
            if (n == m) {
                pt2 += n;
            }
        }
    }

    std.debug.print("pt2: {d}\n", .{pt2});
}
