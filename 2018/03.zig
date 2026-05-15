const std = @import("std");

const Claim = struct {
    id: usize,
    x: usize,
    y: usize,
    width: usize,
    height: usize,

    const State = enum {
        Unknown,
        Intact,
        Overlaps,
    };
};

fn parseField(field: ?[]const u8) !usize {
    return std.fmt.parseUnsigned(usize, field orelse return error.InvalidClaim, 10);
}

fn parseClaim(line: []const u8) !Claim {
    var fields = std.mem.tokenizeAny(u8, line, " #,:@x");
    var claim: Claim = undefined;
    claim.id = try parseField(fields.next());
    claim.x = try parseField(fields.next());
    claim.y = try parseField(fields.next());
    claim.width = try parseField(fields.next());
    claim.height = try parseField(fields.next());
    if (fields.next() != null) {
        return error.InvalidClaim;
    }
    return claim;
}

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var claimed_times: [1000][1000]usize = .{.{0} ** 1000} ** 1000;

    while (try stdin.takeDelimiter('\n')) |line| {
        const claim = try parseClaim(line);

        for (claim.y..(claim.y + claim.height)) |y| {
            for (claim.x..(claim.x + claim.width)) |x| {
                claimed_times[y][x] += 1;
            }
        }
    }

    var overlaps: usize = 0;

    for (claimed_times) |row| {
        for (row) |times| {
            if (times > 1) {
                overlaps += 1;
            }
        }
    }
    try stdout.print("{d}\n", .{overlaps});
}

pub fn part2(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    var claimed_by: [1000][1000]usize = .{.{0} ** 1000} ** 1000;
    var intact: [1500]Claim.State = .{.Unknown} ** 1500;

    while (try stdin.takeDelimiter('\n')) |line| {
        const claim = try parseClaim(line);

        if (intact[claim.id] == .Unknown) {
            intact[claim.id] = .Intact;
        }

        for (claim.y..(claim.y + claim.height)) |y| {
            for (claim.x..(claim.x + claim.width)) |x| {
                if (claimed_by[y][x] == 0) {
                    claimed_by[y][x] = claim.id;
                } else {
                    intact[claim.id] = .Overlaps;
                    intact[claimed_by[y][x]] = .Overlaps;
                }
            }
        }
    }

    for (1.., intact[1..]) |id, status| {
        if (status == .Intact) {
            try stdout.print("{d}\n", .{id});
            return;
        }
    }
}
