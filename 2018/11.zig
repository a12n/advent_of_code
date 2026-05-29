const std = @import("std");

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

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const line = try stdin.takeDelimiter('\n') orelse return error.InvalidInput;
    const serial_number = try std.fmt.parseInt(u32, line, 10);
    const grid_power = gridPower(serial_number);

    var largest: ?struct { x: u16, y: u16, power: i16 } = null;
    for (0..300 - 3 + 1) |y| {
        for (0..300 - 3 + 1) |x| {
            const square_power: i16 =
                grid_power[y + 0][x + 0] + grid_power[y + 0][x + 1] + grid_power[y + 0][x + 2] +
                grid_power[y + 1][x + 0] + grid_power[y + 1][x + 1] + grid_power[y + 1][x + 2] +
                grid_power[y + 2][x + 0] + grid_power[y + 2][x + 1] + grid_power[y + 2][x + 2];
            if (largest == null or square_power > largest.?.power) {
                largest = .{ .x = @intCast(x), .y = @intCast(y), .power = square_power };
            }
        }
    }

    try stdout.print("{d},{d}\n", .{ largest.?.x + 1, largest.?.y + 1 });
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
