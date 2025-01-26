const std = @import("std");

pub fn run(alc: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile("../inputsff/24/1.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;

    var left = std.ArrayList(i64).init(alc);
    defer left.deinit();
    var right = std.ArrayList(i64).init(alc);
    defer right.deinit();

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
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
