const std = @import("std");

const Grid = struct {
    const max_size = 300;

    serial_number: u32,

    // const _private = struct {
    //     fn cellIndex(x: usize, y: usize) usize {
    //         return y * max_size + x;
    //     }
    //
    //     // Number of squares of the specified size in the grid.
    //     fn numSquares(size: usize) usize {
    //         const side = max_size - (size - 1);
    //         return side * side;
    //     }
    //
    //     // Cumulative table of numSquares() for each square size possible.
    //     fn numSquaresTable() [max_size + 1]usize {
    //         var table: [max_size + 1]usize = .{0} ** (max_size + 1);
    //         for (1..max_size + 1) |size| {
    //             table[size] = table[size - 1] + numSquares(size);
    //         }
    //         return table;
    //     }
    //
    //     // fn gridEndCum(square_size: usize) usize {
    //     //     // TODO: Array for sizes up to 300.
    //     //     if (square_size == 0) {
    //     //         return 0;
    //     //     } else {
    //     //         return gridEnd(square_size) + gridEndCum(square_size - 1);
    //     //     }
    //     // }
    //     //
    //     // fn index(size: usize, x: usize, y: usize) usize {
    //     //     return gridEndCum(size) + cellIndex(x, y);
    //     // }
    //     //
    //     // fn totalSize() usize {
    //     //     return index(max_size, max_size - 1, max_size - 1) + 1;
    //     // }
    //
    //     // memo: [totalSize()]?i16 = .{null} ** totalSize(),
    // };

    _power: [max_size][max_size][max_size]?isize = .{.{.{null} ** max_size} ** max_size} ** max_size,

    fn squarePower(self: *Grid, size: usize, x: usize, y: usize) isize {
        std.debug.assert(size >= 1 and size <= max_size);
        std.debug.assert((x + size) <= max_size);
        std.debug.assert((y + size) <= max_size);
        if (self._power[size - 1][y][x] == null) {
            if (size == 1) {
                self._power[size - 1][y][x] = cellPower(@intCast(x), @intCast(y), self.serial_number);
            } else if (size % 2 == 0) {
                const top_left = self.squarePower(size / 2, x, y);
                const top_right = self.squarePower(size / 2, x + size / 2, y);
                const bottom_left = self.squarePower(size / 2, x, y + size / 2);
                const bottom_right = self.squarePower(size / 2, x + size / 2, y + size / 2);
                self._power[size - 1][y][x] = top_left + top_right + bottom_left + bottom_right;
            } else {
                // FIXME
                const top_left = self.squarePower(size - 1, x, y);
                var right: isize = 0;
                var bottom: isize = 0;
                for (0..size - 1) |d| {
                    right += self.squarePower(1, x + size - 1, y + d);
                    bottom += self.squarePower(1, x + d, y + size - 1);
                }
                const bottom_right = self.squarePower(1, x + size - 1, y + size - 1);
                self._power[size - 1][y][x] = top_left + right + bottom + bottom_right;
            }
        }
        return self._power[size - 1][y][x].?;
    }
};

fn cellPower(x: u16, y: u16, serial_number: u32) i8 {
    const rack_id: usize = x + 10;
    const power: usize = (rack_id * y + serial_number) * rack_id;
    return @intCast(@as(isize, @intCast(power / 100 % 10)) - 5);
}

fn gridPower(serial_number: u32) [300][300]i8 {
    var grid: [300][300]i8 = .{.{0} ** 300} ** 300;
    for (0..300) |y| {
        for (0..300) |x| {
            grid[y][x] = cellPower(@intCast(x + 1), @intCast(y + 1), serial_number);
        }
    }
    return grid;
}

test "fuel cell power" {
    const expectEqual = std.testing.expectEqual;

    try expectEqual(4, cellPower(3, 5, 8));

    try expectEqual(-5, cellPower(122, 79, 57));
    try expectEqual(0, cellPower(217, 196, 39));
    try expectEqual(4, cellPower(101, 153, 71));
}

fn readSerialNumber(reader: *std.Io.Reader) !u32 {
    const line = try reader.takeDelimiter('\n') orelse return error.InvalidInput;
    return try std.fmt.parseInt(u32, line, 10);
}

pub fn part1(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var grid: *Grid = try init.arena.allocator().create(Grid);
    grid.* = .{ .serial_number = try readSerialNumber(stdin) };

    var largest: ?struct { x: usize, y: usize, power: isize } = null;
    for (0..Grid.max_size - 3 + 1) |y| {
        for (0..Grid.max_size - 3 + 1) |x| {
            const square_power = grid.squarePower(3, x, y);
            if (largest == null or square_power > largest.?.power) {
                largest = .{ .x = x, .y = y, .power = square_power };
            }
        }
    }

    try stdout.print("{d},{d}\n", .{ largest.?.x, largest.?.y });
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var grid: *Grid = try init.arena.allocator().create(Grid);
    grid.* = .{ .serial_number = try readSerialNumber(stdin) };

    var largest: ?struct { x: usize, y: usize, size: usize, power: isize } = null;
    for (1..Grid.max_size + 1) |size| {
        for (0..Grid.max_size - size + 1) |y| {
            for (0..Grid.max_size - size + 1) |x| {
                const square_power = grid.squarePower(size, x, y);
                if (largest == null or square_power > largest.?.power) {
                    largest = .{ .x = x, .y = y, .size = size, .power = square_power };
                }
            }
        }
    }

    try stdout.print("{d},{d},{d}\n", .{ largest.?.x, largest.?.y, largest.?.size });
}
