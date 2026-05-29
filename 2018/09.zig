const std = @import("std");

fn parseGame(str: []const u8) !struct { usize, usize } {
    var fields = std.mem.tokenizeAny(u8, str, "players; last marble is worth points");
    const n_players = try std.fmt.parseUnsigned(usize, fields.next() orelse return error.InvalidInput, 10);
    const last_marble = try std.fmt.parseUnsigned(usize, fields.next() orelse return error.InvalidInput, 10);
    if (fields.next() != null) return error.InvalidInput;
    return .{ n_players, last_marble };
}

pub fn part1(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    const n_players, const last_marble = try parseGame(
        try stdin.takeDelimiter('\n') orelse
            return error.InvalidInput,
    );

    var circle: std.ArrayList(usize) = try .initCapacity(allocator, last_marble + 1);
    var score = try allocator.alloc(usize, n_players);
    var marble: usize = 0;
    var player: usize = 0;
    var index: usize = 0;

    @memset(score, 0);
    try circle.append(allocator, marble);
    marble += 1;

    while (marble <= last_marble) : (marble += 1) {
        // std.debug.print("circle {any} {d} index {any}\n", .{ circle.items, circle.items.len, index });
        // std.debug.print("marble {any} player {any}\n", .{ marble, player });
        if (marble % 23 == 0) {
            const next_index = if (index < 7) circle.items.len - (7 - index) else index - 7;
            score[player] += marble;
            score[player] += circle.orderedRemove(next_index);
            index = next_index;
        } else {
            const next_index = (index + 1) % circle.items.len;
            // std.debug.print("next_index {any}\n", .{next_index});
            try circle.insert(allocator, next_index + 1, marble);
            index = next_index + 1;
        }
        player = (player + 1) % n_players;
        // std.debug.print("circle {any} {d} index {any}\n\n", .{ circle.items, circle.items.len, index });
    }

    std.debug.print("n_players {d} last_marble {d}\n", .{ n_players, last_marble });
    std.debug.print("score {any}\n", .{score});

    try stdout.print("{d}\n", .{score[std.mem.findMax(usize, score)]});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
