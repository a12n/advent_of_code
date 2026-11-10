const std = @import("std");

const aoc = @import("aoc.zig");
const grid = aoc.grid.planar;

const max_rows = 32;
const max_cols = 32;

const Path = struct {
    items: [max_rows * max_cols]grid.Point = undefined,
    len: usize = 0,
};

const UnitType = enum {
    elf,
    goblin,

    fn isTarget(self: UnitType, other: UnitType) bool {
        return (self == .elf and other == .goblin) or
            (self == .goblin and other == .elf);
        // return self != other;
    }
};

const Unit = union(UnitType) {
    elf: u8,
    goblin: u8,

    pub fn fromChar(c: u8) ?Unit {
        return switch (c) {
            'E', 'e' => .{ .elf = 200 },
            'G', 'g' => .{ .goblin = 200 },
            else => null,
        };
    }

    pub fn toString(self: Unit) []const u8 {
        return switch (self) {
            .elf => "\x1b[42mE\x1b[0m",
            .goblin => "\x1b[41mG\x1b[0m",
        };
    }
};

const Tile = union(enum) {
    open,
    wall,
    unit: Unit,

    pub fn fromChar(c: u8) ?Tile {
        if (Unit.fromChar(c)) |unit| {
            return .{ .unit = unit };
        }
        return switch (c) {
            '.' => .open,
            '#' => .wall,
            else => null,
        };
    }

    pub fn toString(self: Tile) []const u8 {
        return switch (self) {
            .open => ".",
            .wall => "▒",
            .unit => |unit| unit.toString(),
        };
    }
};

const Map = grid.DenseBounded(Tile, max_rows, max_cols);

fn isInTargetRange(map: Map, unit: UnitType, pos: grid.Point) bool {
    for (map.neighbors4(pos)) |maybe_tile| {
        switch (maybe_tile orelse continue) {
            .unit => |other_unit| if (unit.isTarget(other_unit)) return true,
            else => {},
        }
    }
    return false;
}

fn nextStep(map: Map, unit: UnitType, start: grid.Point) ?grid.Direction {
    const State = struct { len: usize, pos: grid.Point };
    var queue_buf: [max_rows * max_cols]State = undefined;
    var queue: std.Deque(State) = .initBuffer(&queue_buf);

    // var dist: [max_rows][max_rows]?usize = .{.{null} ** max_cols} ** max_rows;
    // var prev: [max_rows][max_cols]?grid.Direction = .{.{null} ** max_cols} ** max_rows;

    _ = start;

    while (queue.popFront()) |s| {
        if (isInTargetRange(map, unit, s.pos)) {
            // TODO
            // found, return path
        }

        for (map.neighbors4(s.pos)) |maybe_tile| {
            const tile = maybe_tile orelse continue;
        }

        // _ = dist;
        // _ = prev;
        // if (s.pos)
        // queue.pushBackBounded();
    }

    // BFS, add states in up, left, right, down order
    // If state position is in range of a target, return the path

    return null;
}

fn round(map: *Map) bool {
    var completed = true;
    var iter = map.extent().iterate();
    while (iter.next()) |p| {
        const unit = switch (map.get(p) orelse continue) {
            .unit => |unit| unit,
            else => continue,
        };

        // TODO
        _ = unit;
    }

    for (0..map.n_rows) |row| {
        for (0..map.n_cols) |col| {
            const unit = switch (map.items[row][col]) {
                .unit => |unit| unit,
                else => continue,
            };

            if (nextStep(map.*, unit, grid.Point{ @intCast(col), @intCast(row) })) |dir| {
                // TODO
                _ = step;
                continue;
            }

            completed = completed;
        }
    }
    return completed;
}

fn sumHitPoints(map: Map) usize {
    var sum: usize = 0;
    for (0..map.n_rows) |row| {
        for (0..map.n_cols) |col| {
            switch (map.items[row][col]) {
                .unit => |unit| {
                    switch (unit) {
                        .elf => |hp| sum += hp,
                        .goblin => |hp| sum += hp,
                    }
                },
                else => {},
            }
        }
    }
    return sum;
}

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var map: Map = try .read(stdin);
    var n: usize = 0;
    map.debugPrint();
    while (true) : (n += 1) {
        if (!round(&map)) {
            break;
        }
    }
    std.debug.print("full rounds completed {d}\n", .{n});
    try stdout.print("{d}\n", .{n * sumHitPoints(map)});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
