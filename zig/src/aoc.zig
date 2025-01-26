const std = @import("std");

pub fn readInput(alc: std.mem.Allocator, year: i64, day: i64) ![]u8 {
    const path = try std.fmt.allocPrint(alc, "../inputs/{d}/{d}.txt", .{ year, day });
    defer alc.free(path);

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const buffer = try alc.alloc(u8, file_size);

    const bytes_read = try file.readAll(buffer);
    if (bytes_read != file_size) {
        alc.free(buffer);
        return error.IncompleteRead;
    }

    return buffer;
}
