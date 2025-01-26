const std = @import("std");
const d01 = @import("d01.zig");
const d02 = @import("d02.zig");
const d03 = @import("d03.zig");
const d04 = @import("d04.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alc = gpa.allocator();

    var args = try std.process.argsWithAllocator(alc);
    defer args.deinit();

    _ = args.next();

    const day = args.next() orelse {
        std.debug.print("Please provide a day number as argument\n", .{});
        std.process.exit(1);
    };

    if (std.mem.eql(u8, day, "1")) {
        try d01.run(alc);
    } else if (std.mem.eql(u8, day, "2")) {
        try d02.run(alc);
    } else if (std.mem.eql(u8, day, "3")) {
        try d03.run(alc);
    } else if (std.mem.eql(u8, day, "4")) {
        try d04.run(alc);
    } else {
        std.debug.print("Day {s} not implemented\n", .{day});
        std.process.exit(1);
    }
}
