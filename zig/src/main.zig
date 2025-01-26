const std = @import("std");
const d01 = @import("d01.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.next();

    const day = args.next() orelse {
        std.debug.print("Please provide a day number as argument\n", .{});
        std.process.exit(1);
    };

    if (std.mem.eql(u8, day, "1")) {
        try d01.run(allocator);
    } else {
        std.debug.print("Day {s} not implemented\n", .{day});
        std.process.exit(1);
    }
}
