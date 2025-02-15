const std = @import("std");
const aoc = @import("aoc.zig");

const width = 135;
const height = 135;

fn doesLoop(grid: aoc.Grid, p: aoc.Point, d: aoc.Direction) bool {
    var visited: [width][height]aoc.Direction = [_][height]aoc.Direction{[_]aoc.Direction{aoc.Direction.none} ** height} ** width;
    var pp = p;
    var dd = d;

    while (true) {
        const next = pp.move(dd);

        if (!grid.isInBounds(next)) {
            return false;
        }

        if (grid.get(next) == '#') {
            dd = dd.rotateRight();
        } else {
            pp = next;

            if (visited[@intCast(pp.x)][@intCast(pp.y)] == dd) {
                return true;
            }

            visited[@intCast(pp.x)][@intCast(pp.y)] = dd;
        }
    }
}

pub fn run(alc: std.mem.Allocator) !void {
    const input = try aoc.readInput(alc, 24, 6);
    defer alc.free(input);

    const grid = try aoc.Grid.init(alc, input);
    defer grid.deinit();

    var visited: [width][height]bool = [_][height]bool{[_]bool{false} ** height} ** width;

    const startP: aoc.Point = grid.posOf('^').?;
    const startD: aoc.Direction = aoc.Direction.up;

    var p: aoc.Point = startP;
    var d: aoc.Direction = startD;

    var pt1: i64 = 0;
    var pt2: i64 = 0;

    while (true) {
        if (!visited[@intCast(p.x)][@intCast(p.y)]) {
            pt1 += 1;

            grid.set(p, '#');

            if (doesLoop(grid, startP, startD)) {
                pt2 += 1;
            }

            grid.set(p, '.');
        }

        visited[@intCast(p.x)][@intCast(p.y)] = true;

        if (!grid.isInBounds(p.move(d))) {
            break;
        }

        if (grid.get(p.move(d)) == '#') {
            d = d.rotateRight();
        } else {
            p = p.move(d);
        }
    }

    std.debug.print("pt1: {}\n", .{pt1});
    std.debug.print("pt2: {}\n", .{pt2});
}
