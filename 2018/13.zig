const std = @import("std");

const aoc = @import("aoc.zig");

const grid = aoc.grid.planar;

const Direction = grid.Direction;
const Point = grid.Point;
const Vector = grid.Vector;

const GridMine = struct {
    const max_size = 150 + 1;

    const Tile = struct {
        // (cart_turns << 6) | (cart_dir << 3) | track
        bits: u8,

        fn combineBits(track: u8, cart_dir: u8, cart_turns: u8) u8 {
            return ((cart_turns & 0b11) << (3 + 3)) |
                ((cart_dir & 0b111) << 3) |
                (track & 0b111);
        }

        const track_horiz = 0b001;
        const track_vert = 0b010;
        const track_curve = 0b011;
        const track_curve_rev = 0b100;
        const track_crossing = 0b101;

        fn trackBits(self: Tile) u8 {
            return self.bits & 0b111;
        }

        fn trackStr(track: u8) []const u8 {
            return switch (track) {
                track_horiz => "─",
                track_vert => "│",
                track_curve => "╱",
                track_curve_rev => "╲",
                track_crossing => "┼",
                else => unreachable,
            };
        }

        const cart_up = 0b001;
        const cart_left = 0b010;
        const cart_right = 0b011;
        const cart_down = 0b100;

        fn cartBits(self: Tile) u8 {
            return (self.bits >> 3) & 0b111;
        }

        fn cartStr(cart_dir: u8) []const u8 {
            return switch (cart_dir) {
                cart_up => "^",
                cart_left => "<",
                cart_right => ">",
                cart_down => "v",
                else => unreachable,
            };
        }

        fn cartTurnsBits(self: Tile) u8 {
            return (self.bits >> (3 + 3)) & 0b11;
        }

        fn move(cart_dir: u8, row: *usize, col: *usize) void {
            switch (cart_dir) {
                cart_up => row.* -= 1,
                cart_left => col.* -= 1,
                cart_right => col.* += 1,
                cart_down => row.* += 1,
                else => unreachable,
            }
        }

        fn turnCw(cart_dir: u8) u8 {
            return switch (cart_dir) {
                cart_up => cart_right,
                cart_left => cart_up,
                cart_right => cart_down,
                cart_down => cart_left,
                else => unreachable,
            };
        }

        fn turnCcw(cart_dir: u8) u8 {
            return switch (cart_dir) {
                cart_up => cart_left,
                cart_left => cart_down,
                cart_right => cart_up,
                cart_down => cart_right,
                else => unreachable,
            };
        }

        fn turnOnCurve(curve_dir: u8, cart_dir: u8) u8 {
            return switch (curve_dir) {
                track_curve => switch (cart_dir) {
                    cart_up => cart_right,
                    cart_left => cart_down,
                    cart_right => cart_up,
                    cart_down => cart_left,
                    else => unreachable,
                },
                track_curve_rev => switch (cart_dir) {
                    cart_up => cart_left,
                    cart_left => cart_up,
                    cart_right => cart_down,
                    cart_down => cart_right,
                    else => unreachable,
                },
                else => unreachable,
            };
        }

        fn turnOnCrossing(cart_turns: u8, cart_dir: u8) u8 {
            return switch (cart_turns % 3) {
                0 => turnCcw(cart_dir),
                1 => cart_dir,
                2 => turnCw(cart_dir),
                else => unreachable,
            };
        }
    };

    tiles: [max_size][max_size]Tile,
    n_rows: usize,
    n_cols: usize,

    fn tick(self: *GridMine) ?struct { row: usize, col: usize } {
        const copy = self.*; // XXX
        for (0..self.n_rows) |row| {
            for (0..self.n_cols) |col| {
                const tile = copy.tiles[row][col];
                const track = tile.trackBits();
                const cart_dir = tile.cartBits();
                const cart_turns = tile.cartTurnsBits();

                if (cart_dir == 0) {
                    continue;
                }

                var next_cart_row = row;
                var next_cart_col = col;
                var next_cart_dir = cart_dir;
                var next_cart_turns = cart_turns;

                switch (track) {
                    Tile.track_horiz, Tile.track_vert => {
                        Tile.move(cart_dir, &next_cart_row, &next_cart_col);
                    },
                    Tile.track_curve, Tile.track_curve_rev => {
                        next_cart_dir = Tile.turnOnCurve(track, cart_dir);
                        Tile.move(next_cart_dir, &next_cart_row, &next_cart_col);
                    },
                    Tile.track_crossing => {
                        next_cart_dir = Tile.turnOnCrossing(cart_turns, cart_dir);
                        Tile.move(next_cart_dir, &next_cart_row, &next_cart_col);
                        next_cart_turns = (cart_turns + 1) % 3;
                    },
                    else => unreachable,
                }

                std.debug.print("tick {d} {d} track {s}: cart_dir {s} cart_turns {d} -> {d} {d} cart_dir {s} cart_turns {d}\n", .{
                    row,
                    col,
                    Tile.trackStr(track),
                    Tile.cartStr(cart_dir),
                    cart_turns,
                    next_cart_row,
                    next_cart_col,
                    Tile.cartStr(next_cart_dir),
                    next_cart_turns,
                });

                // Collision
                if (self.tiles[next_cart_row][next_cart_col].cartBits() != 0) {
                    return .{ .row = next_cart_row, .col = next_cart_col };
                }

                // Update
                self.tiles[row][col].bits = Tile.combineBits(track, 0, 0);
                self.tiles[next_cart_row][next_cart_col].bits = Tile.combineBits(
                    self.tiles[next_cart_row][next_cart_col].trackBits(),
                    next_cart_dir,
                    next_cart_turns,
                );
            }
        }
        return null;
    }

    fn debugPrint(self: GridMine) !void {
        for (0..self.n_rows) |row| {
            for (0..self.n_cols) |col| {
                const tile = self.tiles[row][col];
                if (tile.cartBits() != 0) {
                    std.debug.print("{s}", .{Tile.cartStr(tile.cartBits())});
                } else if (tile.trackBits() != 0) {
                    std.debug.print("{s}", .{Tile.trackStr(tile.trackBits())});
                } else {
                    std.debug.print("{s}", .{" "});
                }
            }
            std.debug.print("\n", .{});
        }
        std.debug.print("\n", .{});
    }
};

fn readMine(reader: *std.Io.Reader) !GridMine {
    var mine: GridMine = std.mem.zeroes(GridMine);
    var row: usize = 0;
    var col: usize = 0;
    while (true) {
        if (reader.takeByte()) |c| {
            switch (c) {
                '^', '<', '>', 'v' => {
                    mine.tiles[row][col].bits =
                        switch (c) {
                            '^' => GridMine.Tile.combineBits(GridMine.Tile.track_vert, GridMine.Tile.cart_up, 0),
                            '<' => GridMine.Tile.combineBits(GridMine.Tile.track_horiz, GridMine.Tile.cart_left, 0),
                            '>' => GridMine.Tile.combineBits(GridMine.Tile.track_horiz, GridMine.Tile.cart_right, 0),
                            'v' => GridMine.Tile.combineBits(GridMine.Tile.track_vert, GridMine.Tile.cart_down, 0),
                            else => unreachable,
                        };
                },
                '-' => {
                    mine.tiles[row][col].bits = GridMine.Tile.track_horiz;
                },
                '|' => {
                    mine.tiles[row][col].bits = GridMine.Tile.track_vert;
                },
                '/' => {
                    mine.tiles[row][col].bits = GridMine.Tile.track_curve;
                },
                '\\' => {
                    mine.tiles[row][col].bits = GridMine.Tile.track_curve_rev;
                },
                '+' => {
                    mine.tiles[row][col].bits = GridMine.Tile.track_crossing;
                },
                ' ' => {
                    mine.tiles[row][col].bits = 0;
                },
                '\n' => {},
                else => return error.InvalidInput,
            }

            if (c == '\n') {
                if (mine.n_cols == 0) {
                    mine.n_cols = col + 1;
                } else if (mine.n_cols != (col + 1)) {
                    return error.InvalidInput;
                }
                mine.n_rows = row + 1;

                row += 1;
                col = 0;
            } else {
                col += 1;
            }
        } else |err| {
            if (err == std.Io.Reader.Error.EndOfStream) {
                return mine;
            } else {
                return err;
            }
        }
    }
}

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
    dir: Direction = .up,
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

    fn turn(n_turns: usize, dir: Direction) Direction {
        return switch (n_turns % 3) {
            0 => dir.rotate(.ccw),
            1 => dir,
            2 => dir.rotate(.cw),
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
            const next_pos = cart.pos + cart.dir.toVector();
            switch (mine.get(next_pos)) {
                .empty => unreachable,
                .curve1 => {
                    cart.pos = next_pos;
                    cart.dir = switch (cart.dir) {
                        .up => .right,
                        .left => .down,
                        .right => .up,
                        .down => .left,
                    };
                },
                .curve2 => {
                    cart.pos = next_pos;
                    cart.dir = switch (cart.dir) {
                        .up => .left,
                        .left => .up,
                        .right => .down,
                        .down => .right,
                    };
                },
                .track => {
                    cart.pos = next_pos;
                },
                .crossing => {
                    cart.pos = next_pos;
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
                    fleet.carts[fleet.n_carts] = .{ .pos = pos, .dir = Direction.fromChar(c).? };
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
    var mine = try readMine(stdin);
    while (true) {
        if (mine.tick()) |crash| {
            try stdout.print("{d},{d}\n", .{ crash.col, crash.row });
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
