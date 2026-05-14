const std = @import("std");

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var frequency: isize = 0;

    while (try stdin.takeDelimiter('\n')) |line| {
        frequency += try std.fmt.parseInt(isize, line, 10);
    }

    try stdout.print("{d}\n", .{frequency});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    var changes: std.ArrayList(isize) = try .initCapacity(allocator, 1000);

    while (try stdin.takeDelimiter('\n')) |line| {
        try changes.append(allocator, try std.fmt.parseInt(isize, line, 10));
    }

    var frequency: isize = 0;
    var seen: std.AutoHashMap(isize, void) = .init(allocator);

    while (true) {
        for (changes.items) |change| {
            if (seen.contains(frequency)) {
                try stdout.print("{d}\n", .{frequency});
                return;
            }
            try seen.put(frequency, {});
            frequency += change;
        }
    }
}
