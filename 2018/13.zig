const std = @import("std");

const GridMine = struct {
    const max_size = 150 + 1;

    const Tile = packed struct {
        // Track segments
        const horiz: u3 = 0b001;
        const vert: u3 = 0b010;
        const curve: u3 = 0b011;
        const curve_rev: u3 = 0b100;
        const crossing: u3 = 0b101;

        // Cart direction
        const up: u3 = 0b001;
        const left: u3 = 0b010;
        const right: u3 = 0b011;
        const down: u3 = 0b100;

        track: u3 = 0,
        cart: u3 = 0,
        turns: u2 = 0,

        fn trackStr(track: u3) []const u8 {
            return switch (track) {
                horiz => "─",
                vert => "│",
                curve => "╱",
                curve_rev => "╲",
                crossing => "┼",
                else => unreachable,
            };
        }

        fn cartStr(cart_dir: u3) []const u8 {
            return switch (cart_dir) {
                up => "^",
                left => "<",
                right => ">",
                down => "v",
                else => unreachable,
            };
        }

        fn move(cart_dir: u3, row: *usize, col: *usize) void {
            switch (cart_dir) {
                up => row.* -= 1,
                left => col.* -= 1,
                right => col.* += 1,
                down => row.* += 1,
                else => unreachable,
            }
        }

        fn turnCw(cart_dir: u3) u3 {
            return switch (cart_dir) {
                up => right,
                left => up,
                right => down,
                down => left,
                else => unreachable,
            };
        }

        fn turnCcw(cart_dir: u3) u3 {
            return switch (cart_dir) {
                up => left,
                left => down,
                right => up,
                down => right,
                else => unreachable,
            };
        }

        fn turnCurve(curve_dir: u3, cart_dir: u3) u3 {
            return switch (curve_dir) {
                curve => switch (cart_dir) {
                    up => right,
                    left => down,
                    right => up,
                    down => left,
                    else => unreachable,
                },
                curve_rev => switch (cart_dir) {
                    up => left,
                    left => up,
                    right => down,
                    down => right,
                    else => unreachable,
                },
                else => unreachable,
            };
        }

        fn turnCrossing(cart_turns: u2, cart_dir: u3) u3 {
            return switch (cart_turns) {
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

    const TickResult = struct {
        // Position of the first crash occured
        crash: ?struct { row: usize, col: usize } = null,

        // Number of carts left in the mine
        n_carts: usize = 0,

        // Position of the last encounterd cart
        cart: ?struct { row: usize, col: usize } = null,
    };

    fn nCarts(self: GridMine) usize {
        var n: usize = 0;
        for (0..self.n_rows) |row| {
            for (0..self.n_cols) |col| {
                if (self.tiles[row][col].cart != 0) {
                    n += 1;
                }
            }
        }
        return n;
    }

    fn tick(self: *GridMine) TickResult {
        var result: TickResult = .{};

        var copy = self.*; // XXX
        for (0..self.n_rows) |row| {
            for (0..self.n_cols) |col| {
                const tile = copy.tiles[row][col];

                if (tile.cart == 0) {
                    // No cart
                    continue;
                }

                var next_row = row;
                var next_col = col;
                var next_cart = tile.cart;
                var next_turns = tile.turns;

                switch (tile.track) {
                    Tile.horiz, Tile.vert => {
                        Tile.move(tile.cart, &next_row, &next_col);
                    },
                    Tile.curve, Tile.curve_rev => {
                        next_cart = Tile.turnCurve(tile.track, tile.cart);
                        Tile.move(next_cart, &next_row, &next_col);
                    },
                    Tile.crossing => {
                        next_cart = Tile.turnCrossing(tile.turns, tile.cart);
                        Tile.move(next_cart, &next_row, &next_col);
                        next_turns = (tile.turns + 1) % 3;
                    },
                    else => unreachable,
                }

                std.debug.print("tick: {d} {d} track {s} cart_dir {s} cart_turns {d} -> {d} {d} cart_dir {s} cart_turns {d}\n", .{
                    row,
                    col,
                    Tile.trackStr(tile.track),
                    Tile.cartStr(tile.cart),
                    tile.turns,
                    next_row,
                    next_col,
                    Tile.cartStr(next_cart),
                    next_turns,
                });

                // No more cart at current position
                self.tiles[row][col].cart = 0;
                self.tiles[row][col].turns = 0;

                // Collision
                if (self.tiles[next_row][next_col].cart != 0) {
                    // Keep position of the first crash
                    if (result.crash == null) {
                        result.crash = .{ .row = next_row, .col = next_col };
                    }
                    // The cart of the next position is also destroyed
                    self.tiles[next_row][next_col].cart = 0;
                    self.tiles[next_row][next_col].turns = 0;
                    if (next_row > row) {
                        copy.tiles[next_row][next_col] = self.tiles[next_row][next_col];
                    }
                    // Both current cart and the cart in the next position are destroyed.
                    std.debug.print("tick: collision at {d} {d}\n", .{ next_row, next_col });
                } else {
                    // The current cart is now in the next position
                    self.tiles[next_row][next_col].cart = next_cart;
                    self.tiles[next_row][next_col].turns = next_turns;
                    result.cart = .{ .row = next_row, .col = next_col };
                    std.debug.print("tick: cart moved to {d} {d}\n", .{ next_row, next_col });
                }
            }
        }

        // XXX: Remove field?
        result.n_carts = self.nCarts();

        return result;
    }

    fn debugPrint(self: GridMine) !void {
        for (0..self.n_rows) |row| {
            for (0..self.n_cols) |col| {
                const tile = self.tiles[row][col];
                if (tile.cart != 0) {
                    std.debug.print("{s}", .{Tile.cartStr(tile.cart)});
                } else if (tile.track != 0) {
                    std.debug.print("{s}", .{Tile.trackStr(tile.track)});
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
                    mine.tiles[row][col] =
                        switch (c) {
                            '^' => GridMine.Tile{ .track = GridMine.Tile.vert, .cart = GridMine.Tile.up },
                            '<' => GridMine.Tile{ .track = GridMine.Tile.horiz, .cart = GridMine.Tile.left },
                            '>' => GridMine.Tile{ .track = GridMine.Tile.horiz, .cart = GridMine.Tile.right },
                            'v' => GridMine.Tile{ .track = GridMine.Tile.vert, .cart = GridMine.Tile.down },
                            else => unreachable,
                        };
                },
                '-' => {
                    mine.tiles[row][col].track = GridMine.Tile.horiz;
                },
                '|' => {
                    mine.tiles[row][col].track = GridMine.Tile.vert;
                },
                '/' => {
                    mine.tiles[row][col].track = GridMine.Tile.curve;
                },
                '\\' => {
                    mine.tiles[row][col].track = GridMine.Tile.curve_rev;
                },
                '+' => {
                    mine.tiles[row][col].track = GridMine.Tile.crossing;
                },
                ' ', '\n' => {},
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

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var mine = try readMine(stdin);

    while (true) {
        const res = mine.tick();

        if (res.crash) |pos| {
            try stdout.print("{d},{d}\n", .{ pos.col, pos.row });
            return;
        }
    }
}

pub fn part2(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var mine = try readMine(stdin);

    while (true) {
        const res = mine.tick();

        if (res.n_carts == 0) {
            return error.NoSolution;
        } else if (res.n_carts == 1) {
            try stdout.print("{d},{d}\n", .{ res.cart.?.col, res.cart.?.row });
            return;
        }
    }
}
