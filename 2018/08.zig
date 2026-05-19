const std = @import("std");

fn takeUnsigned(reader: *std.Io.Reader) !usize {
    const field = try reader.takeDelimiter(' ') orelse return error.InvalidInput;
    return try std.fmt.parseUnsigned(usize, std.mem.trimEnd(u8, field, "\n"), 10);
}

fn firstCheck(reader: *std.Io.Reader) !usize {
    const n_children = try takeUnsigned(reader);
    const n_metadata = try takeUnsigned(reader);

    var sum: usize = 0;

    for (0..n_children) |_| {
        sum += try firstCheck(reader);
    }

    for (0..n_metadata) |_| {
        sum += try takeUnsigned(reader);
    }

    return sum;
}

fn secondCheck(gpa: std.mem.Allocator, reader: *std.Io.Reader) !usize {
    const n_children = try takeUnsigned(reader);
    const n_metadata = try takeUnsigned(reader);

    var sum: usize = 0;
    var child_sum = try gpa.alloc(usize, n_children);
    defer gpa.free(child_sum);

    if (n_children != 0) {
        for (0..n_children) |i| {
            child_sum[i] = try secondCheck(gpa, reader);
        }
        for (0..n_metadata) |_| {
            const ref = try takeUnsigned(reader) - 1;
            if (ref < n_children) {
                sum += child_sum[ref];
            }
        }
    } else {
        for (0..n_metadata) |_| {
            sum += try takeUnsigned(reader);
        }
    }

    return sum;
}

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    try stdout.print("{d}\n", .{try firstCheck(stdin)});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    try stdout.print("{d}\n", .{try secondCheck(init.gpa, stdin)});
}
