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

    const result = try alc.dupe(u8, std.mem.trim(u8, buffer, " \t\n\r"));

    alc.free(buffer);

    return result;
}

pub const Grid = struct {
    alc: std.mem.Allocator,
    grid: []u8,
    width: usize,
    height: usize,

    pub fn init(alc: std.mem.Allocator, str: []u8) !Grid {
        var width: usize = 0;
        var height: usize = 1;

        for (str) |c| {
            if (c == '\n') {
                height += 1;
            }

            if (height == 1) {
                width += 1;
            }
        }

        const grid = try alc.alloc(u8, width * height);

        var x: usize = 0;
        var y: usize = 0;
        for (str) |c| {
            if (c == '\n') {
                x = 0;
                y += 1;
                continue;
            }

            grid[y * height + x] = c;

            x += 1;
        }

        return .{
            .alc = alc,
            .grid = grid,
            .width = width,
            .height = height,
        };
    }

    pub fn deinit(self: Grid) void {
        self.alc.free(self.grid);
    }

    pub fn isInBounds(self: *const Grid, x: usize, y: usize) bool {
        return x >= 0 and x < self.width and y >= 0 and y < self.height;
    }

    pub fn get(self: *const Grid, x: usize, y: usize) u8 {
        if (!self.isInBounds(x, y)) {
            return ' ';
        }

        return self.grid[y * self.height + x];
    }

    pub fn print(self: *const Grid) void {
        for (0..self.height) |y| {
            for (0..self.width) |x| {
                std.debug.print("{c}", .{self.get(x, y)});
            }
            std.debug.print("\n", .{});
        }
    }
};
