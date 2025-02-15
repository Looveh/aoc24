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

pub const Point = struct {
    x: i64,
    y: i64,

    pub fn add(self: Point, other: Point) Point {
        return .{ .x = self.x + other.x, .y = self.y + other.y };
    }

    pub fn move(self: Point, direction: Direction) Point {
        return switch (direction) {
            .up => .{ .x = self.x, .y = self.y - 1 },
            .down => .{ .x = self.x, .y = self.y + 1 },
            .left => .{ .x = self.x - 1, .y = self.y },
            .right => .{ .x = self.x + 1, .y = self.y },
            .none => .{ .x = self.x, .y = self.y },
        };
    }

    pub fn eql(self: Point, other: Point) bool {
        return self.x == other.x and self.y == other.y;
    }

    pub fn dupe(self: Point) Point {
        return .{ .x = self.x, .y = self.y };
    }
};

pub const Direction = enum {
    up,
    down,
    left,
    right,
    none,

    pub fn rotateLeft(self: Direction) Direction {
        return switch (self) {
            .up => .left,
            .left => .down,
            .down => .right,
            .right => .up,
            .none => .none,
        };
    }

    pub fn rotateRight(self: Direction) Direction {
        return switch (self) {
            .up => .right,
            .right => .down,
            .down => .left,
            .left => .up,
            .none => .none,
        };
    }
};

pub const Grid = struct {
    alc: std.mem.Allocator,
    grid: []u8,
    width: u32,
    height: u32,

    pub fn init(alc: std.mem.Allocator, str: []u8) !Grid {
        var width: u32 = 0;
        var height: u32 = 1;

        for (str) |c| {
            if (c == '\n') {
                height += 1;
            }

            if (height == 1) {
                width += 1;
            }
        }

        var grid = try alc.alloc(u8, @intCast(width * height));

        var x: u32 = 0;
        var y: u32 = 0;
        for (str) |c| {
            if (c == '\n') {
                x = 0;
                y += 1;
                continue;
            }

            grid[y * width + x] = c;

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

    pub fn isInBounds(self: *const Grid, p: Point) bool {
        return p.x >= 0 and p.x < @as(i64, self.width) and p.y >= 0 and p.y < @as(i64, self.height);
    }

    pub fn get(self: *const Grid, p: Point) u8 {
        if (!self.isInBounds(p)) {
            return ' ';
        }

        return self.grid[@intCast(p.y * self.width + p.x)];
    }

    pub fn set(self: *const Grid, p: Point, c: u8) void {
        self.grid[@intCast(p.y * self.width + p.x)] = c;
    }

    pub fn posOf(self: *const Grid, c: u8) ?Point {
        for (0..self.height) |y| {
            for (0..self.width) |x| {
                const p = Point{ .x = @intCast(x), .y = @intCast(y) };

                if (self.get(p) == c) {
                    return p;
                }
            }
        }

        return null;
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
