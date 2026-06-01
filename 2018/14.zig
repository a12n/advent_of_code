const std = @import("std");

const Recipe = u4;
const Score = u4;

fn readNumRecipes(reader: *std.Io.Reader) !usize {
    const line = try reader.takeDelimiter('\n') orelse return error.InvalidInput;
    return try std.fmt.parseUnsigned(usize, line, 10);
}

const ScoreSequence = struct {
    const max_size = 10;

    bytes: [max_size]Score = undefined,
    len: usize = 0,
};

fn readScoreSequence(reader: *std.Io.Reader) !ScoreSequence {
    var seq: ScoreSequence = .{};
    if (try reader.takeDelimiter('\n')) |line| {
        for (line) |c| {
            seq.bytes[seq.len] = @intCast(try std.fmt.charToDigit(c, 10));
            seq.len += 1;
        }
    } else {
        return error.InvalidInput;
    }
    return seq;
}

pub fn part1(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();
    const n = try readNumRecipes(stdin);

    var recipes: std.ArrayList(Recipe) = try .initCapacity(allocator, n + 10);
    try recipes.append(allocator, 3);
    try recipes.append(allocator, 7);

    var elves: [2]usize = .{ 0, 1 };

    // std.debug.print("recipes {any}\n", .{recipes.items});
    for (0..n + 10) |_| {
        const current: [2]Recipe = .{ recipes.items[elves[0]], recipes.items[elves[1]] };
        const sum: u8 = @as(u8, @intCast(current[0])) + current[1];
        // std.debug.print("elves {any} current {any} sum {any}\n", .{ elves, current, sum });
        const a: Recipe = @intCast(@divTrunc(sum, 10));
        const b: Recipe = @intCast(sum % 10);
        if (a != 0) try recipes.append(allocator, a);
        try recipes.append(allocator, b);
        // std.debug.print("recipes {any}\n", .{recipes.items});
        elves[0] = (elves[0] + 1 + current[0]) % recipes.items.len;
        elves[1] = (elves[1] + 1 + current[1]) % recipes.items.len;
    }

    for (recipes.items[n .. n + 10]) |r| {
        try stdout.printAsciiChar(@as(u8, r) + '0', .{});
    }
    try stdout.printAsciiChar('\n', .{});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    const seq = try readScoreSequence(stdin);
    std.debug.print("seq {any}\n", .{seq.bytes[0..seq.len]});

    var recipes: std.ArrayList(Recipe) = try .initCapacity(allocator, 8192);
    try recipes.append(allocator, 3);
    try recipes.append(allocator, 7);

    var elves: [2]usize = .{ 0, 1 };

    // std.debug.print("recipes {any}\n", .{recipes.items});
    const n = blk: while (true) {
        const current: [2]Recipe = .{ recipes.items[elves[0]], recipes.items[elves[1]] };
        const sum: u8 = @as(u8, @intCast(current[0])) + current[1];
        // std.debug.print("elves {any} current {any} sum {any}\n", .{ elves, current, sum });
        const a: Recipe = @intCast(@divTrunc(sum, 10));
        const b: Recipe = @intCast(sum % 10);
        if (a != 0) {
            try recipes.append(allocator, a);
            if (recipes.items.len >= seq.len) {
                if (std.mem.eql(Score, recipes.items[recipes.items.len - seq.len ..], seq.bytes[0..seq.len])) {
                    std.debug.print("after {d}\n", .{recipes.items.len - seq.len});
                    break :blk (recipes.items.len - seq.len);
                }
            }
        }
        try recipes.append(allocator, b);
        if (recipes.items.len >= seq.len) {
            if (std.mem.eql(Score, recipes.items[recipes.items.len - seq.len ..], seq.bytes[0..seq.len])) {
                std.debug.print("after {d}\n", .{recipes.items.len - seq.len});
                break :blk (recipes.items.len - seq.len);
            }
        }

        // std.debug.print("recipes {any}\n", .{recipes.items});
        elves[0] = (elves[0] + 1 + current[0]) % recipes.items.len;
        elves[1] = (elves[1] + 1 + current[1]) % recipes.items.len;
    };

    try stdout.print("{d}\n", .{n});
}
