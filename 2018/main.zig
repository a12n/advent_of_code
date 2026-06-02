const std = @import("std");

const day01 = @import("01.zig");
const day02 = @import("02.zig");
const day03 = @import("03.zig");
const day04 = @import("04.zig");
const day05 = @import("05.zig");
const day07 = @import("07.zig");
const day08 = @import("08.zig");
const day09 = @import("09.zig");
const day10 = @import("10.zig");
const day11 = @import("11.zig");
const day12 = @import("12.zig");
const day13 = @import("13.zig");
const day14 = @import("14.zig");
const day15 = @import("15.zig");

pub fn main(init: std.process.Init) !void {
    var args_iter = init.minimal.args.iterate();
    const stem = std.fs.path.stem(args_iter.next() orelse return error.InvalidExecName);
    if (stem.len != "00-0".len) {
        return error.InvalidExecName;
    }

    const day = std.fmt.parseUnsigned(u8, stem[0..2], 10) catch return error.InvalidPuzzle;
    const part = std.fmt.parseUnsigned(u8, stem[3..4], 10) catch return error.InvalidPuzzle;
    if (day < 1 or day > 25 or part < 1 or (day == 25 and part > 1) or part > 2) {
        return error.InvalidPuzzle;
    }

    var stdin_buf: [3584]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().reader(init.io, &stdin_buf);
    const stdin = &stdin_reader.interface;

    var stdout_buf: [512]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(init.io, &stdout_buf);
    const stdout = &stdout_writer.interface;

    const puzzles: [15][2](*const fn (std.process.Init, *std.Io.Reader, *std.Io.Writer) anyerror!void) = .{
        .{ day01.part1, day01.part2 },
        .{ day02.part1, day02.part2 },
        .{ day03.part1, day03.part2 },
        .{ day04.part1, day04.part2 },
        .{ day05.part1, day05.part2 },
        .{ day05.part1, day05.part2 }, // FIXME
        .{ day07.part1, day07.part2 },
        .{ day08.part1, day08.part2 },
        .{ day09.part1, day09.part2 },
        .{ day10.part1, day10.part2 },
        .{ day11.part1, day11.part2 },
        .{ day12.part1, day12.part2 },
        .{ day13.part1, day13.part2 },
        .{ day14.part1, day14.part2 },
        .{ day15.part1, day15.part2 },
    };

    try puzzles[day - 1][part - 1](init, stdin, stdout);
    try stdout.flush();
}
