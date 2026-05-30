const std = @import("std");

const aoc = @import("aoc.zig");
const grid = aoc.grid.planar;
const Point = grid.Point;
const Vector = grid.Vector;

const Mine = struct {
    const max_size = 150;

    const Tile = enum {
        empty,
        track,
        curve1,
        curve2,
        crossing,
    };

    tiles: [max_size][max_size]Tile = .{.{.empty} ** max_size} ** max_size,

    fn get(self: Mine, pos: Point) Tile {
        if (pos[0] < 0 or pos[0] >= max_size or
            pos[1] < 0 or pos[1] >= max_size)
        {
            return .empty;
        }
        return self.tiles[@intCast(pos[1])][@intCast(pos[0])];
    }

    fn set(self: *Mine, pos: Point, tile: Tile) void {
        self.tiles[@intCast(pos[1])][@intCast(pos[0])] = tile;
    }
};

const Cart = struct {
    pos: Point = .{ 0, 0 },
    dir: Vector = .{ 0, 0 },
    n_turns: usize = 0,

    fn lessThan(_: void, lhs: Cart, rhs: Cart) bool {
        if (lhs.pos[1] < rhs.pos[1]) {
            return true;
        } else if (lhs.pos[1] > rhs.pos[1]) {
            return false;
        }
        if (lhs.pos[0] < rhs.pos[0]) {
            return true;
        } else if (lhs.pos[0] > rhs.pos[0]) {
            return false;
        }
        // Undetected collision
        unreachable;
    }

    fn turn(n_turns: usize, dir: Vector) Vector {
        return switch (n_turns % 3) {
            0 => grid.rotate(.ccw, dir),
            1 => dir,
            2 => grid.rotate(.cw, dir),
            else => unreachable,
        };
    }
};

const Fleet = struct {
    const max_carts = 20;

    carts: [max_carts]Cart = .{Cart{}} ** max_carts,
    n_carts: usize = 0,

    fn tick(self: *Fleet, mine: Mine) ?Point {
        var crash: ?Point = null;

        std.sort.block(Cart, self.carts[0..self.n_carts], {}, Cart.lessThan);

        for (0.., self.carts[0..self.n_carts]) |i, *cart| {
            // FIXME: Proper updates
            switch (mine.get(cart.pos + cart.dir)) {
                .empty => {
                    // TODO
                    if (mine.get(cart.pos + grid.rotate(.ccw, cart.dir)) != .empty) {
                        cart.dir = grid.rotate(.ccw, cart.dir);
                        cart.pos += cart.dir;
                    } else if (mine.get(cart.pos + grid.rotate(.cw, cart.dir)) != .empty) {
                        cart.dir = grid.rotate(.cw, cart.dir);
                        cart.pos += cart.dir;
                    } else {
                        unreachable;
                    }
                },
                .track => {
                    cart.pos += cart.dir;
                },
                .crossing => {
                    cart.pos += cart.dir;
                    cart.dir = Cart.turn(cart.n_turns, cart.dir);
                    cart.n_turns += 1;
                },
            }

            // Check collisions of this cart with already moved carts
            for (self.carts[0..i]) |other_cart| {
                if (crash != null) {
                    break;
                }
                if (cart.pos[0] == other_cart.pos[0] and
                    cart.pos[1] == other_cart.pos[1])
                {
                    crash = cart.pos;
                }
            }
        }

        return crash;
    }
};

fn readInput(reader: *std.Io.Reader) !struct { Mine, Fleet } {
    var mine: Mine = .{};
    var fleet: Fleet = .{};
    var pos: Point = .{ 0, 0 };
    while (true) {
        if (reader.takeByte()) |c| {
            switch (c) {
                '<', '>', '^', 'v' => {
                    mine.set(pos, .track);
                    fleet.carts[fleet.n_carts] = .{
                        .pos = pos,
                        .dir = switch (c) {
                            '<' => Vector{ -1, 0 },
                            '>' => Vector{ 1, 0 },
                            'v' => Vector{ 0, 1 },
                            '^' => Vector{ 0, -1 },
                            else => unreachable,
                        },
                    };
                    std.debug.print("cart {any}\n", .{fleet.carts[fleet.n_carts]});
                    fleet.n_carts += 1;
                },
                '|', '-' => {
                    mine.set(pos, .track);
                },
                '/' => {
                    mine.set(pos, .curve1);
                },
                '\\' => {
                    mine.set(pos, .curve2);
                },
                '+' => {
                    mine.set(pos, .crossing);
                },
                ' ', '\n' => {},
                else => return error.InvalidInput,
            }
            pos[0] += 1;
            if (c == '\n') {
                pos[0] = 0;
                pos[1] += 1;
            }
        } else |err| {
            if (err == std.Io.Reader.Error.EndOfStream) {
                return .{ mine, fleet };
            } else {
                return err;
            }
        }
    }
}

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const mine, var fleet = try readInput(stdin);
    std.debug.print("fleet {any}\n", .{fleet});
    while (true) {
        if (fleet.tick(mine)) |crash| {
            try stdout.print("{d},{d}\n", .{ crash[0], crash[1] });
            return;
        }
    }
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
