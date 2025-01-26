const std = @import("std");
const aoc = @import("aoc.zig");

fn isDigit(c: u8) bool {
    return c >= 48 and c <= 57;
}

pub fn run(alc: std.mem.Allocator) !void {
    const input = try aoc.readInput(alc, 24, 3);
    defer alc.free(input);

    var pt1: i64 = 0;
    var pt2: i64 = 0;
    var enabled = true;

    for (0..input.len - 4) |i| {
        if (std.mem.startsWith(u8, input[i..], "do()")) {
            enabled = true;
            continue;
        } else if (std.mem.startsWith(u8, input[i..], "don't()")) {
            enabled = false;
            continue;
        } else if (!std.mem.startsWith(u8, input[i..], "mul(")) {
            continue;
        }

        const n1_start: usize = i + 4;
        var n1_end: usize = n1_start;

        while (n1_end < input.len and isDigit(input[n1_end])) {
            n1_end += 1;
        }

        if (n1_end <= n1_start or n1_end + 1 >= input.len) {
            continue;
        }

        if (input[n1_end] != ',') {
            continue;
        }

        const n2_start: usize = n1_end + 1;
        var n2_end: usize = n2_start;

        while (n2_end < input.len and isDigit(input[n2_end])) {
            n2_end += 1;
        }

        if (n2_end <= n2_start or n2_end + 1 >= input.len) {
            continue;
        }

        if (input[n2_end] != ')') {
            continue;
        }

        const a = try std.fmt.parseInt(i64, input[n1_start..n1_end], 10);
        const b = try std.fmt.parseInt(i64, input[n2_start..n2_end], 10);

        pt1 += a * b;

        if (enabled) {
            pt2 += a * b;
        }
    }

    std.debug.print("pt1: {d}\n", .{pt1});
    std.debug.print("pt2: {d}\n", .{pt2});
}
