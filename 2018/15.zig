const std = @import("std");

const aoc = @import("aoc.zig");
const grid = aoc.grid.planar;

const Tile = enum {
    open,
    wall,
    elf,
    goblin,

    pub fn fromChar(c: u8) ?Tile {
        return switch (c) {
            '.' => .open,
            '#' => .wall,
            'E', 'e' => .elf,
            'G', 'g' => .goblin,
            else => null,
        };
    }

    pub fn toString(self: Tile) []const u8 {
        return switch (self) {
            .open => ".",
            .wall => "▒",
            .elf => "\x1b[42mE\x1b[0m",
            .goblin => "\x1b[41mG\x1b[0m",
        };
    }
};

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var map: grid.DenseBounded(Tile, 50, 50) = try .read(stdin);
    map.debugPrint();
    map.items[0][0] = map.items[0][0];
    _ = stdout;
    // TODO
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
