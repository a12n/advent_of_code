const std = @import("std");

const aoc = @import("aoc.zig");
const grid = aoc.grid.planar;

const Tile = union(enum) {
    open,
    wall,
    elf: u8,
    goblin: u8,

    pub fn fromChar(c: u8) ?Tile {
        return switch (c) {
            '.' => .open,
            '#' => .wall,
            'E', 'e' => .{ .elf = 200 },
            'G', 'g' => .{ .goblin = 200 },
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

const Map = grid.DenseBounded(Tile, 50, 50);

fn round(map: *Map) void {
    for (0..map.n_rows) |row| {
        for (0..map.n_cols) |col| {
            _ = row;
            _ = col;
            // TODO
        }
    }
}

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var map: Map = try .read(stdin);
    map.debugPrint();
    while (true) {
        round(&map);
    }
    // TODO
    _ = stdout;
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
