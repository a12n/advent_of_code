const std = @import("std");

const FrequencyArray = struct {
    const Self = @This();

    freq: ['z' - 'a' + 1]usize = .{0} ** ('z' - 'a' + 1),

    fn init(buf: []const u8) Self {
        var self: Self = .{};
        for (buf) |c| {
            self.freq[c - 'a'] += 1;
        }
        return self;
    }

    fn contains(self: Self, n: usize) bool {
        return std.mem.findScalar(usize, &self.freq, n) != null;
    }
};

fn hamming(comptime T: type, a: []const T, b: []const T) usize {
    var distance: usize = 0;
    for (a, b) |ai, bi| {
        if (ai != bi) {
            distance += 1;
        }
    }
    return distance;
}

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var two: usize = 0;
    var three: usize = 0;
    while (try stdin.takeDelimiter('\n')) |line| {
        const freq: FrequencyArray = .init(line);
        two += @intFromBool(freq.contains(2));
        three += @intFromBool(freq.contains(3));
    }
    try stdout.print("{d}\n", .{two * three});
}

const BoxId = []const u8;

fn correctBoxes(boxes: []BoxId) ?struct { BoxId, BoxId } {
    for (0.., boxes[0..(boxes.len - 1)]) |i, a| {
        for (boxes[i + 1 ..]) |b| {
            if (hamming(u8, a, b) == 1) {
                return .{ a, b };
            }
        }
    }
    return null;
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    var boxes: std.ArrayList(BoxId) = try .initCapacity(allocator, 250);
    while (try stdin.takeDelimiter('\n')) |line| {
        try boxes.append(allocator, try allocator.dupe(u8, line));
    }

    const correct = correctBoxes(boxes.items) orelse return error.NoSolution;
    for (correct[0], correct[1]) |a, b| {
        if (a == b) {
            try stdout.printAsciiChar(a, .{});
        }
    }
    try stdout.printAsciiChar('\n', .{});
}
