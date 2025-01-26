const std = @import("std");
const aoc = @import("aoc.zig");

pub fn run(alc: std.mem.Allocator) !void {
    const input = try aoc.readInput(alc, 24, 4);
    defer alc.free(input);

    const grid = try aoc.Grid.init(alc, input);
    defer grid.deinit();

    var pt1: i64 = 0;
    var pt2: i64 = 0;

    for (0..grid.width) |x| {
        for (0..grid.height) |y| {
            if (grid.get(x, y) == 'X' and grid.get(x + 1, y) == 'M' and grid.get(x + 2, y) == 'A' and grid.get(x + 3, y) == 'S') {
                pt1 += 1;
            }

            if (grid.get(x, y) == 'X' and grid.get(x - 1, y) == 'M' and grid.get(x - 2, y) == 'A' and grid.get(x - 3, y) == 'S') {
                pt1 += 1;
            }

            if (grid.get(x, y) == 'X' and grid.get(x, y + 1) == 'M' and grid.get(x, y + 2) == 'A' and grid.get(x, y + 3) == 'S') {
                pt1 += 1;
            }

            if (grid.get(x, y) == 'X' and grid.get(x, y - 1) == 'M' and grid.get(x, y - 2) == 'A' and grid.get(x, y - 3) == 'S') {
                pt1 += 1;
            }

            if (grid.get(x, y) == 'X' and grid.get(x + 1, y + 1) == 'M' and grid.get(x + 2, y + 2) == 'A' and grid.get(x + 3, y + 3) == 'S') {
                pt1 += 1;
            }

            if (grid.get(x, y) == 'X' and grid.get(x - 1, y + 1) == 'M' and grid.get(x - 2, y + 2) == 'A' and grid.get(x - 3, y + 3) == 'S') {
                pt1 += 1;
            }

            if (grid.get(x, y) == 'X' and grid.get(x + 1, y - 1) == 'M' and grid.get(x + 2, y - 2) == 'A' and grid.get(x + 3, y - 3) == 'S') {
                pt1 += 1;
            }

            if (grid.get(x, y) == 'X' and grid.get(x - 1, y - 1) == 'M' and grid.get(x - 2, y - 2) == 'A' and grid.get(x - 3, y - 3) == 'S') {
                pt1 += 1;
            }

            const c1 = grid.get(x, y);
            const c2 = grid.get(x - 1, y - 1);
            const c3 = grid.get(x + 1, y + 1);
            const c4 = grid.get(x - 1, y + 1);
            const c5 = grid.get(x + 1, y - 1);

            if (c1 == 'A' and c2 == 'M' and c3 == 'S' and c4 == 'M' and c5 == 'S') {
                pt2 += 1;
            }

            if (c1 == 'A' and c2 == 'S' and c3 == 'M' and c4 == 'M' and c5 == 'S') {
                pt2 += 1;
            }

            if (c1 == 'A' and c2 == 'M' and c3 == 'S' and c4 == 'S' and c5 == 'M') {
                pt2 += 1;
            }

            if (c1 == 'A' and c2 == 'S' and c3 == 'M' and c4 == 'S' and c5 == 'M') {
                pt2 += 1;
            }
        }
    }

    std.debug.print("pt1: {d}\n", .{pt1});
    std.debug.print("pt2: {d}\n", .{pt2});
}
