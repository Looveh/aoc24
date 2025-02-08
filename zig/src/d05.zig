const std = @import("std");
const aoc = @import("aoc.zig");

var orderings: [100][100]bool = [_][100]bool{[_]bool{false} ** 100} ** 100;

fn comparator(_: void, this: u64, that: u64) bool {
    return orderings[this][that];
}

pub fn run(alc: std.mem.Allocator) !void {
    const input = try aoc.readInput(alc, 24, 5);
    defer alc.free(input);

    var pt1: u64 = 0;
    var pt2: u64 = 0;
    var pages: [50]u64 = [_]u64{0} ** 50;
    var sorted_pages: [50]u64 = [_]u64{0} ** 50;
    var lines = std.mem.split(u8, input, "\n");

    while (lines.next()) |line| {
        if (line.len == 0) {
            break;
        }

        var nums = std.mem.split(u8, line, "|");

        const left: u64 = try std.fmt.parseInt(u64, nums.next().?, 10);
        const right: u64 = try std.fmt.parseInt(u64, nums.next().?, 10);

        orderings[left][right] = true;
    }

    while (lines.next()) |line| {
        if (line.len == 0) {
            break;
        }

        var len: u64 = 0;
        var is_ordered = true;
        var nums = std.mem.split(u8, line, ",");

        while (nums.next()) |num| {
            const n = try std.fmt.parseInt(u64, num, 10);
            pages[len] = n;
            sorted_pages[len] = n;
            len += 1;
        }

        std.mem.sort(u64, sorted_pages[0..len], {}, comparator);

        for (0..len - 1) |i| {
            if (pages[i] != sorted_pages[i]) {
                is_ordered = false;
                break;
            }
        }

        if (is_ordered) {
            pt1 += pages[len / 2];
        } else {
            pt2 += sorted_pages[len / 2];
        }
    }

    std.debug.print("pt1: {d}\n", .{pt1});
    std.debug.print("pt2: {d}\n", .{pt2});
}
