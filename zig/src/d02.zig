const std = @import("std");
const aoc = @import("aoc.zig");

pub fn run(alc: std.mem.Allocator) !void {
    const input = try aoc.readInput(alc, 24, 2);
    defer alc.free(input);

    var lines = std.ArrayList(std.ArrayList(i64)).init(alc);
    defer lines.deinit();

    var in_lines = std.mem.split(u8, input, "\n");
    while (in_lines.next()) |in_line| {
        if (in_line.len == 0) {
            continue;
        }

        var line = std.ArrayList(i64).init(alc);

        var cols = std.mem.tokenize(u8, in_line, " ");
        while (cols.next()) |col| {
            try line.append(try std.fmt.parseInt(i64, col, 10));
        }

        try lines.append(line);
    }

    var pt1: i64 = 0;
    var pt2: i64 = 0;

    for (lines.items) |line| {
        if (isSafe(line, null)) pt1 += 1;
        if (isTolerantSafe(line)) pt2 += 1;
    }

    std.debug.print("pt1: {d}\n", .{pt1});
    std.debug.print("pt2: {d}\n", .{pt2});
}

fn isTolerantSafe(line: std.ArrayList(i64)) bool {
    for (0..line.items.len) |i| {
        if (isSafe(line, i)) {
            return true;
        }
    }

    return false;
}

fn isSafe(line: std.ArrayList(i64), skip: ?usize) bool {
    var isIncreasing = false;
    var isDecreasing = false;
    var minDelta: i64 = std.math.maxInt(i64);
    var maxDelta: i64 = 0;

    for (0..line.items.len - 1) |i| {
        const from: i64 = @intCast(if (skip == i) i - 1 else i);
        const to: i64 = @intCast(if (skip == i + 1) i + 2 else i + 1);

        if (from < 0 or to >= line.items.len) {
            continue;
        }

        const a = line.items[@intCast(from)];
        const b = line.items[@intCast(to)];
        const d: i64 = @intCast(@abs(a - b));

        if (a > b) isDecreasing = true;
        if (a < b) isIncreasing = true;
        if (d < minDelta) minDelta = d;
        if (d > maxDelta) maxDelta = d;
    }

    return ((minDelta >= 1) and
        (maxDelta <= 3) and
        ((isIncreasing and !isDecreasing) or
        (!isIncreasing and isDecreasing)));
}
